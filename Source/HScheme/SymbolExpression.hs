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

module Org.Org.Semantic.HScheme.SymbolExpression
	(
	FunctorLambda(..),
	fSubst,fSubstMap,
	fSubstMapSequential,fSubstMapSeparate,fSubstRecursive,

	SymbolExpression,
	runSymbolExpression,runSymbolExpressionF,runSymbolExpressionM
	) where
	{
	import Org.Org.Semantic.HBase;


	-- FunctorLambda

	-- SSA?
	class (Eq sym,FunctorApplyReturn f) =>
	 FunctorLambda sym val f | f -> sym val where
		{
		fSymbol :: sym -> f val;
		fAbstract :: sym -> f a -> f (val -> a);
		};

	fSubst :: (FunctorLambda sym val f) =>
	 sym -> f val -> f a -> f a;
	fSubst sym valueExpr bodyExpr = fApply (fAbstract sym bodyExpr) valueExpr;

	fSubstMap :: (FunctorLambda sym val f) =>
	 ((val -> a) -> b -> a) -> sym -> f b -> f a -> f a;
	fSubstMap map sym valueExpr bodyExpr = liftF2 map (fAbstract sym bodyExpr) valueExpr;

	-- | substs are actually done in reverse order
	fSubstMapSequential :: (FunctorLambda sym val f) =>
	 ((val -> a) -> b -> a) -> [(sym,f b)] -> f a -> f a;
	fSubstMapSequential _ [] bodyExpr = bodyExpr;
	fSubstMapSequential map ((sym,valueExpr):binds) bodyExpr = fSubstMap map sym valueExpr (fSubstMapSequential map binds bodyExpr);

	fSubstMapSeparate :: (FunctorLambda sym val f) =>
	 ((val -> a) -> b -> a) -> [(sym,f b)] -> f a -> f a;
	fSubstMapSeparate = fSubstMapSequential; -- NYI

	data ZeroList a = MkZeroList;
	data NextList t a = MkNextList a (t a);

	instance Functor ZeroList where
		{
		fmap _ _ = MkZeroList;
		};

	instance (Functor t) => Functor (NextList t) where
		{
		fmap map (MkNextList a ta) = MkNextList (map a) (fmap map ta);
		};

	instance ExtractableFunctor ZeroList where
		{
		fExtract _ = return' MkZeroList;
		};

	instance (ExtractableFunctor t) => ExtractableFunctor (NextList t) where
		{
		fExtract (MkNextList ga tga) = liftF2 MkNextList ga (fExtract tga);
		};

	fixFunc :: (Functor t) => t (t a -> a) -> t a;
	fixFunc t = fix (\p -> (fmap (\x -> x p) t));

	data MutualBindings f a = forall t. (ExtractableFunctor t) =>
	 MkMutualBindings (t (f a)) (forall r. f r -> f (t a -> r));

	applyMutualBindings :: (FunctorApplyReturn f) => MutualBindings f a -> f r -> f r;
	applyMutualBindings (MkMutualBindings bindValues abstracter) body =
	 liftF2 (\tad rd -> rd (fixFunc tad)) (fExtract (fmap abstracter bindValues)) (abstracter body);

	makeMutualBindings :: (FunctorLambda sym val f) => [(sym,f val)] -> MutualBindings f val;
	makeMutualBindings [] = MkMutualBindings MkZeroList (fmap (\r MkZeroList -> r));
	makeMutualBindings ((sym,fval):restb) = case (makeMutualBindings restb) of
		{
		(MkMutualBindings bindValues abstracter) -> MkMutualBindings
		 (MkNextList fval bindValues)
		 (\body -> fmap (\atar (MkNextList a ta) -> atar a ta) ((fAbstract sym) (abstracter body)));
		};

	fSubstRecursive :: (FunctorLambda sym val f) => [(sym,f val)] -> f r -> f r;
	fSubstRecursive = applyMutualBindings . makeMutualBindings;


	-- SymbolExpression

	data SymbolExpression sym val a = ClosedSymbolExpression a |
	 	OpenSymbolExpression sym (SymbolExpression sym val (val -> a));

	instance HasReturn (SymbolExpression sym val) where
		{
		return' = ClosedSymbolExpression;
		};

	instance Functor (SymbolExpression sym val) where
		{
		fmap map (ClosedSymbolExpression a) = ClosedSymbolExpression (map a);
		fmap map (OpenSymbolExpression sym sp) = OpenSymbolExpression sym (fmap (\oa -> (map . oa)) sp);
		};

	instance (Eq sym) => FunctorApply (SymbolExpression sym val) where
		{
		fApply (ClosedSymbolExpression ab) rda = fmap ab rda;
		fApply (OpenSymbolExpression sym rdoab) rda = 
			OpenSymbolExpression sym (fApply (fmap (\oab oa o -> oab o (oa o)) rdoab) (fAbstract sym rda));
		};

	instance (Eq sym) => FunctorLambda sym val (SymbolExpression sym val) where
		{
		fSymbol sym = OpenSymbolExpression sym (ClosedSymbolExpression id);

		fAbstract sym (ClosedSymbolExpression a) = ClosedSymbolExpression (const a);
		fAbstract sym (OpenSymbolExpression sym' sp) | sym == sym' = sp;
		fAbstract sym (OpenSymbolExpression sym' sp) = OpenSymbolExpression sym' (fmap (\a val' val -> a val val') (fAbstract sym sp));
{--
		fSubst sym valueExpr (ClosedSymbolExpression a) = ClosedSymbolExpression a;
		fSubst sym valueExpr (OpenSymbolExpression sym' sp) | sym == sym' = ;
		fSubst sym valueExpr (OpenSymbolExpression sym' sp) = OpenSymbolExpression sym' ;
--}		};

	runSymbolExpression ::
	 (sym -> val) ->
	 SymbolExpression sym val a ->
	 a;
	runSymbolExpression resolve (ClosedSymbolExpression a) = a;
	runSymbolExpression resolve (OpenSymbolExpression sym sp) = runSymbolExpression resolve sp (resolve sym);

	runSymbolExpressionF :: (FunctorApplyReturn f) =>
	 (sym -> f val) ->
	 SymbolExpression sym val a ->
	 f a;
	runSymbolExpressionF resolve (ClosedSymbolExpression a) = return' a;
	runSymbolExpressionF resolve (OpenSymbolExpression sym sp) = liftF2
	 (\val oa -> oa val) (resolve sym) (runSymbolExpressionF resolve sp);

	liftM2 :: (Monad m) =>
	 (a -> b -> r) -> (m a -> m b -> m r);
	liftM2 abr ma mb = do
		{
		a <- ma;
		b <- mb;
		return (abr a b);
		};

	runSymbolExpressionM :: (Monad f) =>
	 (sym -> f val) ->
	 SymbolExpression sym val a ->
	 f a;
	runSymbolExpressionM resolve (ClosedSymbolExpression a) = return a;
	runSymbolExpressionM resolve (OpenSymbolExpression sym sp) = liftM2
	 (\val oa -> oa val) (resolve sym) (runSymbolExpressionM resolve sp);
	}
