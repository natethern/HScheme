-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>

This file is part of HScheme.

HScheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

HScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HScheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module Org.Org.Semantic.HScheme.Interpret.Pattern
	(
	Pattern(),SchemePattern,
	ellipsisSymbol,makeObjectPattern,makeListPattern,
	patternZip,patternBind,
	throwMismatchObjError,
	mustMatch,makeLambda
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Abstract;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;
	import Data.List(zip);

	-- lists always the same size
	data Pattern m n obj subject = MkPattern [Symbol] (subject -> m (n [obj]));

	patternZip :: (Functor m,Functor n) =>
	 Pattern m n obj subject ->
	 subject ->
	 m (n [(Symbol,obj)]);
	patternZip (MkPattern syms func) subject = fmap (fmap (zip syms)) (func subject);

	type SchemePattern pm obj = Pattern pm (MatchMonad obj) obj;

	makeEllipsisPatternFunc' ::
		(
 		ListObject r obj,
		Build pm r,
		?objType :: Type obj
		) =>
	 [Symbol] ->
	 (obj -> pm (MatchMonad obj [obj])) ->
	 ([obj] -> pm (MatchMonad obj [obj]));
	makeEllipsisPatternFunc' syms func [] = return (return (fmap (\_ -> nilObject) syms));
	makeEllipsisPatternFunc' syms func (a:as) = do
		{
		mm <- func a;
		mms <- makeEllipsisPatternFunc' syms func as;
		fExtract (do
			{
			objs <- mm;
			objrs <- mms;
			return (for (\(obj,objr) -> cons obj objr) (zip objs objrs));
			});
		};

	makeEllipsisPatternFunc ::
		(
		ObjectSubtype r obj obj,
		Build pm r,
		?objType :: Type obj
		) =>
	 [Symbol] ->
	 (obj -> pm (MatchMonad obj [obj])) ->
	 (obj -> pm (MatchMonad obj [obj]));
	makeEllipsisPatternFunc syms func subject = do
		{
		msubjs <- resultFromObject subject;
		case msubjs of
			{
			SuccessResult subjs -> makeEllipsisPatternFunc' syms func subjs;
			ExceptionResult exp -> return (ExceptionResult exp);
			};
		};

	ellipsisSymbol :: Symbol;
	ellipsisSymbol = MkSymbol "...";

	makeObjectPattern ::
		(
		PatternError cm obj,
  		ObjectSubtype r obj obj,
  		ObjectSubtype r obj Symbol,
		Build cm r,
		Build pm r,
		?objType :: Type obj
		) =>
	 [Symbol] ->
	 obj ->
	 cm (SchemePattern pm obj obj);
	makeObjectPattern literals patternObj = do
		{
		mPattern <- resultFromObject patternObj;
		case mPattern of
			{
			SuccessResult (Left (Left ())) -> return (MkPattern [] (\obj -> do
				{
				mobj <- resultFromObject obj;
				return (do
					{
					(_ :: ()) <- mobj;
					return [];
					});
				}));
			SuccessResult (Left (Right (hobj,tobj))) ->  do
				{
				MkPattern hsyms hfunc <- makeObjectPattern literals hobj;
				mellipsis <- resultFromObject tobj;
				case mellipsis of
					{
					SuccessResult (sym,()) | sym == ellipsisSymbol ->
					return (MkPattern hsyms (makeEllipsisPatternFunc hsyms hfunc));
					_ -> do
						{
						MkPattern tsyms tfunc <- makeObjectPattern literals tobj;
						return (MkPattern (hsyms ++ tsyms) (\subj -> do
							{
							msubj <- resultFromObject subj;
							case msubj of
								{
								ExceptionResult exp -> return (ExceptionResult exp);
								SuccessResult (hsubj,tsubj) -> do
									{
									nhres <- hfunc hsubj;
									ntres <- tfunc tsubj;
									return (do
										{
										hres <- nhres;
										tres <- ntres;
										return (hres ++ tres)
										});
									};
								};
							}));
						};
					};
				};
			SuccessResult (Right sym) -> if hasElement sym literals
			 then return (MkPattern [] (\obj -> do
				{
				mobj <- resultFromObject obj;
				return (do
					{
					sym' <- mobj;
					if sym' == sym
					then return []
					else throwSingle (MkMismatch (LiteralExpected sym) obj);
					});
				}))
			 else return (MkPattern [sym] (\obj -> return (return [obj])));
			_ -> throwBadPatternError patternObj;
			}
		};

	makeListPattern ::
		(
 		PatternError cm obj,
		ObjectSubtype r obj obj,
  		ObjectSubtype r obj Symbol,
 		Build cm r,
		Build pm r,
		?objType :: Type obj
		) =>
	 [Symbol] ->
	 obj ->
	 cm (SchemePattern pm obj [obj]);
	makeListPattern literals pattern = fmap
		(\(MkPattern syms func) -> MkPattern syms (\list -> do
			{
			obj <- getObject list;
			func obj;
			}))
		(makeObjectPattern literals pattern);

	patternBind ::
		(
		Monad pm,
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 SchemePattern pm obj subject ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (subject -> pm (MatchMonad obj (m a)));	-- big stack o' monads
	patternBind (MkPattern syms func) bodyExpr = fmap
	 (\valsmobj subject -> fmap (fmap valsmobj) (func subject))
	 (schemeExprAbstractList syms bodyExpr);

	throwMismatchObjError ::
		(
 		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		Build m r,
		MonadThrow obj m,
		MonadIsA m obj (Mismatch obj),
		?objType :: Type obj
		) =>
	 Mismatch obj -> m a;
	throwMismatchObjError mm = do
		{
		mmObj <- getConvert mm;
		throwArgError "pattern-mismatch" [mmObj];
		};

	mustMatch :: (Mismatch obj -> m a) -> MatchMonad obj (m a) -> m a;
	mustMatch _ (SuccessResult ma) = ma;
	mustMatch mismatcher (ExceptionResult mm) = mismatcher mm;

	makeLambda ::
		(
		PatternError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 obj ->
	 SchemeExpression r obj (m a) ->
	 cm (SchemeExpression r obj ([obj] -> m a));
	makeLambda patternObj expr = do
		{
		pattern <- makeListPattern [] patternObj;
		return (fmap (\proc subject -> do
			{
			match <- proc subject;
			mustMatch throwMismatchError match;
			}) (patternBind pattern expr));
		};
	}
