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
	Expected(..),Mismatch,MatchMonad,Pattern(),SchemePattern,
	makeObjectPattern,makeListPattern,
	patternZip,patternBind,
	mustMatch,makeLambda
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Abstract;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
--	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;
	import Data.List(zip);

	data Expected =
	 LiteralExpected Symbol		|
	 NullExpected				|
	 PairTypeExpected			|
	 ListExpected				|
	 VectorTypeExpected			|
	 VectorLengthExpected Int	;
	
	instance Show Expected where
		{
		show (LiteralExpected sym) = show sym;
		show NullExpected = "()";
		show PairTypeExpected = "( . )";
		show ListExpected = "(...)";
		show VectorTypeExpected = "(# ...)";
		show (VectorLengthExpected i) = "(# ...) length " ++ (show i);
		};

	data Mismatch obj = MkMismatch Expected obj;

	mismatch ::
		(
		Monad m,
		MonadSingleThrow (Mismatch obj) n
		) =>
	 Expected -> obj -> m (n a);
	mismatch exp obj = return (throwSingle (MkMismatch exp obj));

	type MatchMonad r m = MonadExceptionResult (Mismatch (Object r m));

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

	type SchemePattern pm r m = Pattern pm (MatchMonad r m) (Object r m);

	makeEllipsisPatternFunc' ::
		(
-- 		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 [Symbol] ->
	 (Object r m -> pm (MatchMonad r m [Object r m])) ->
	 ([Object r m] -> pm (MatchMonad r m [Object r m]));
	makeEllipsisPatternFunc' syms func [] = return (return (fmap (\_ -> NilObject) syms));
	makeEllipsisPatternFunc' syms func (a:as) = do
		{
		mm <- func a;
		mms <- makeEllipsisPatternFunc' syms func as;
		fExtract (foo mm mms);
		};

	foo ::
		(
 --		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 MatchMonad r m [Object r m] ->
	 MatchMonad r m [Object r m] ->
	 MatchMonad r m (pm [Object r m]);
	foo mm mms = do
		{
		objs <- mm;
		objrs <- mms;
		return (fExtract (bar objs objrs));
		};

	bar ::
		(
 --		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 [Object r m] ->
	 [Object r m] ->
	 [pm (Object r m)];
	bar objs objrs = fmap (\(obj,objr) -> getConvert (obj,objr)) (zip objs objrs);

	makeEllipsisPatternFunc ::
		(
-- 		BuildThrow cm (Object r m) r,
		Build pm r,
		?objType :: Type (Object r m)
		) =>
	 [Symbol] ->
	 (Object r m -> pm (MatchMonad r m [Object r m])) ->
	 (Object r m -> pm (MatchMonad r m [Object r m]));
	makeEllipsisPatternFunc syms func subject = do
		{
		msubjs <- getMaybeConvert subject;
		case msubjs of
			{
			Just subjs -> makeEllipsisPatternFunc' syms func subjs;
			Nothing -> mismatch ListExpected subject;
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
	 cm (SchemePattern pm r m (Object r m));
	makeObjectPattern _ NilObject = return (MkPattern [] (\obj -> do
		{
		mobj <- getMaybeConvert obj;
		case mobj of
			{
			Nothing -> mismatch NullExpected obj;
			Just (_ :: ()) -> return (return []);
			};
		}));
	makeObjectPattern literals (SymbolObject sym) | hasElement sym literals =
	 return (MkPattern [] (\obj -> do
		{
		mobj <- getMaybeConvert obj;
		case mobj of
			{
			Just sym' | sym' == sym -> return (return []);
			_ -> mismatch (LiteralExpected sym) obj;
			};
		}));
	makeObjectPattern _ (SymbolObject sym) =
	 return (MkPattern [sym] (\obj -> 
	  return (return [obj])
	 ));
	makeObjectPattern literals pobj@(PairObject _ _) = do
		{
		Just (hobj,tobj) <- getMaybeConvert pobj;
		MkPattern hsyms hfunc <- makeObjectPattern literals hobj;
		mellipsis <- getMaybeConvert tobj;
		case mellipsis of
			{
			Just (sym,()) | sym == ellipsisSymbol ->
			 return (MkPattern hsyms (makeEllipsisPatternFunc hsyms hfunc));
			_ -> do
				{
				MkPattern tsyms tfunc <- makeObjectPattern literals tobj;
				return (MkPattern (hsyms ++ tsyms) (\subj -> do
					{
					msubj <- getMaybeConvert subj;
					case msubj of
						{
						Nothing -> mismatch PairTypeExpected subj;
						Just (hsubj,tsubj) -> do
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
	 cm (SchemePattern pm r m [Object r m]);
	makeListPattern literals pattern = do
		{
		MkPattern syms func <- makeObjectPattern literals pattern;
		return (MkPattern syms (\list -> do
			{
			obj <- getConvert list;
			func obj;
			}));
		};

	patternBind ::
		(
		Monad pm,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SchemePattern pm r m subject ->
	 ObjectSchemeExpression r m ->
	 SchemeExpression r m (subject -> pm (MatchMonad r m (m (Object r m))));	-- big stack o' monads
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
	 MatchMonad r m (m a) -> m a;
	mustMatch (SuccessExceptionResult ma) = ma;
	mustMatch (ExceptionExceptionResult (MkMismatch exp obj)) = do
		{
		expected <- getConvert (show exp);
		throwArgError "pattern-mismatch" [expected,obj];
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
