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
	makeObjectPattern,makeListPattern,
	patternZip,patternBind,
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

	patternZip :: (Monad m,Monad n) =>
	 Pattern m n obj subject ->
	 subject ->
	 m (n [(Symbol,obj)]);
	patternZip (MkPattern syms func) subject = do
		{
		nlist <- func subject;
		return (do
			{
			list <- nlist;
			return (zip syms list);
			});
		};

	type SchemePattern pm obj = Pattern pm (MatchMonad obj) obj;

	makeEllipsisPatternFunc' ::
		(
 		SchemeObject r obj,
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
 		SchemeObject r obj,
		ObjectSubtype r obj obj,
		Build pm r,
		?objType :: Type obj
		) =>
	 [Symbol] ->
	 (obj -> pm (MatchMonad obj [obj])) ->
	 (obj -> pm (MatchMonad obj [obj]));
	makeEllipsisPatternFunc syms func subject = do
		{
		msubjs <- fromObject subject;
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
 		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 [Symbol] ->
	 Object r m ->
	 cm (SchemePattern pm (Object r m) (Object r m));
	makeObjectPattern _ NilObject = return (MkPattern [] (\obj -> do
		{
		mobj <- fromObject obj;
		return (do
			{
			(_ :: ()) <- mobj;
			return [];
			});
		}));
	makeObjectPattern literals (SymbolObject sym) | hasElement sym literals =
	 return (MkPattern [] (\obj -> do
		{
		mobj <- fromObject obj;
		return (do
			{
			sym' <- mobj;
			if sym' == sym
			 then return []
			 else throwSingle (MkMismatch (LiteralExpected sym) obj);
			});
		}));
	makeObjectPattern _ (SymbolObject sym) =
	 return (MkPattern [sym] (\obj -> 
	  return (return [obj])
	 ));
	makeObjectPattern literals pobj@(PairObject _ _) = do
		{
		SuccessResult (hobj,tobj) <- fromObject pobj;
		MkPattern hsyms hfunc <- makeObjectPattern literals hobj;
		mellipsis <- fromObject tobj;
		case mellipsis of
			{
			SuccessResult (sym,()) | sym == ellipsisSymbol ->
			 return (MkPattern hsyms (makeEllipsisPatternFunc hsyms hfunc));
			_ -> do
				{
				MkPattern tsyms tfunc <- makeObjectPattern literals tobj;
				return (MkPattern (hsyms ++ tsyms) (\subj -> do
					{
					msubj <- fromObject subj;
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
	makeObjectPattern _ obj = throwArgError "bad-pattern" [obj];

	makeListPattern ::
		(
 		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 [Symbol] ->
	 Object r m ->
	 cm (SchemePattern pm (Object r m) [Object r m]);
	makeListPattern literals pattern = do
		{
		MkPattern syms func <- makeObjectPattern literals pattern;
		return (MkPattern syms (\list -> do
			{
			obj <- getObject list;
			func obj;
			}));
		};

	patternBind ::
		(
		Monad pm,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SchemePattern pm (Object r m) subject ->
	 ObjectSchemeExpression r m ->
	 SchemeExpression r m (subject -> pm (MatchMonad (Object r m) (m (Object r m))));	-- big stack o' monads
	patternBind (MkPattern syms func) bodyExpr = fmap (\valsmobj subject -> do
		{
	 	matchvals <- func subject;
		return (do
			{
	 		vals <- matchvals;
			return (do
				{
--	 			locs <- fExtract (fmap new vals);
--				valsmobj locs;
				valsmobj vals;
				});
			});
		}) (schemeExprAbstractList syms bodyExpr);

	mustMatch :: 
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 MatchMonad (Object r m) (m a) -> m a;
	mustMatch (SuccessResult ma) = ma;
	mustMatch (ExceptionResult mm) = do
		{
		mmObj <- getConvert mm;
		throwArgError "pattern-mismatch" [mmObj];
		};

	makeLambda ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 Object r m ->
	 ObjectSchemeExpression r m ->
	 cm (SchemeExpression r m ([Object r m] -> m (Object r m)));
	makeLambda patternObj expr = do
		{
		pattern <- makeListPattern [] patternObj;
		return (fmap (\proc subject -> do
			{
			match <- proc subject;
			mustMatch match;
			}) (patternBind pattern expr));
		};
	}
