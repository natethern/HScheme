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

module Org.Org.Semantic.HScheme.Interpret.FunctorLambda
	(
	FunctorLambda(..),
	fSubst,fSubstMap,
	fSubstMapSequential,fSubstMapSeparate,fSubstMapRecursive
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
	 sym -> f val -> f r -> f r;
	fSubst sym valueExpr bodyExpr = fApply (fAbstract sym bodyExpr) valueExpr;

	fSubstMap :: (FunctorLambda sym val f) =>
	 ((val -> r) -> a -> r) -> sym -> f a -> f r -> f r;
	fSubstMap map sym valueExpr bodyExpr = liftF2 map (fAbstract sym bodyExpr) valueExpr;

	-- | substs are actually done in reverse order
	fSubstMapSequential :: (FunctorLambda sym val f) =>
	 ((val -> r) -> a -> r) -> [(sym,f a)] -> f r -> f r;
	fSubstMapSequential _ [] bodyExpr = bodyExpr;
	fSubstMapSequential map ((sym,valueExpr):binds) bodyExpr = fSubstMap map sym valueExpr (fSubstMapSequential map binds bodyExpr);

	fSubstMapSeparate :: (FunctorLambda sym val f) =>
	 ((val -> r) -> a -> r) -> [(sym,f a)] -> f r -> f r;
	fSubstMapSeparate = fSubstMapSequential; -- NYI


	-- List

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


	-- MutualBindings

	data MutualBindings f a v = forall t. (ExtractableFunctor t) =>
	 MkMutualBindings (t (f a)) (forall r. f r -> f (t v -> r));

	applyMutualBindings :: (FunctorApplyReturn f) =>
	 ((val -> r) -> a -> r) -> MutualBindings f a val -> f r -> f r;
	applyMutualBindings map (MkMutualBindings bindValues abstracter) body =
--	 liftF2 (\tad rd -> rd (fixFunc tad)) (fExtract (fmap abstracter bindValues)) (abstracter body);
	 liftF2 (foo1 map) (fExtract (fmap abstracter bindValues)) (abstracter body);

	foo1 :: (Functor t) => ((v -> r) -> a -> r) ->
	 t (t v -> a) -> (t v -> r) -> r;
	foo1 map ttva tvr = error "recursive subst not defined";

{--
abstracter :: forall r. f r -> f (t v -> r)
bindValues :: t (f a)
body :: f r
(fmap abstracter bindValues) :: t (f (t v -> a))
(fExtract (fmap abstracter bindValues)) :: f (t (t v -> a))
(abstracter body) :: f (t v -> r)
foo1 :: t (t v -> a) -> (t v -> r) -> r
map :: (v -> r) -> a -> r
foo2 :: t (t v -> (v -> r) -> r) -> (t v -> r) -> r


v = m loc
a = m obj
--}

	makeMutualBindings :: (FunctorLambda sym val f) =>
	 [(sym,f a)] -> MutualBindings f a val;
	makeMutualBindings [] = MkMutualBindings MkZeroList (fmap (\r MkZeroList -> r));
	makeMutualBindings ((sym,fval):restb) = case (makeMutualBindings restb) of
		{
		(MkMutualBindings bindValues abstracter) -> MkMutualBindings
		 (MkNextList fval bindValues)
		 (\body -> fmap (\atar (MkNextList a ta) -> atar a ta) ((fAbstract sym) (abstracter body)));
		};

	fSubstMapRecursive :: (FunctorLambda sym val f) =>
	 ((val -> r) -> a -> r) -> [(sym,f a)] -> f r -> f r;
	fSubstMapRecursive map bindings = applyMutualBindings map (makeMutualBindings bindings);
	}
