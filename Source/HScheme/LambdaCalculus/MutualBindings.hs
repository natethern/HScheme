-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>

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

module Org.Org.Semantic.HScheme.LambdaCalculus.MutualBindings
	(
	Abstracter(..),
	bindSeparate,bindRecursive
	) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.FixedList;
	import Org.Org.Semantic.HBase;

	data MutualBindings f a v = forall t. (ExtractableFunctor t) =>
	 MkMutualBindings (t (f a)) (forall r. f r -> f (t v -> r));

	newtype Abstracter f v = MkAbstracter {unAbstracter :: forall r. f r -> f (v -> r)};

	-- uses irrefutable patterns (~) here to get mfix to work
	makeMutualBindings :: (Functor f) =>
	 [(Abstracter f v,f a)] -> MutualBindings f a v;
	makeMutualBindings [] = MkMutualBindings MkZeroList (fmap (\r ~MkZeroList -> r));
	makeMutualBindings ((abstract,fval):restb) = case (makeMutualBindings restb) of
		{
		(MkMutualBindings bindValues abstracter) -> MkMutualBindings
		 (MkNextList fval bindValues)
		 (\body -> fmap (\atar ~(MkNextList a ta) -> atar a ta) (unAbstracter abstract (abstracter body)));
		};

	applyMutualBindingsSeparate :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t a -> (t val -> r) -> r) ->
	 MutualBindings f a val -> f r -> f r;
	applyMutualBindingsSeparate separater (MkMutualBindings bindValues abstracter) body =
	 liftF2 separater (fextract bindValues) (abstracter body);

	applyMutualBindingsRecursive :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t (t val -> a) -> (t val -> r) -> r) ->
	 MutualBindings f a val -> f r -> f r;
	applyMutualBindingsRecursive fixer (MkMutualBindings bindValues abstracter) body =
	 liftF2 fixer (fextract (fmap abstracter bindValues)) (abstracter body);

	bindSeparate :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t a -> (t v -> r) -> r) ->
	 [(Abstracter f v,f a)] -> f r -> f r;
	bindSeparate separater bindings = applyMutualBindingsSeparate separater (makeMutualBindings bindings);

	bindRecursive :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t (t v -> a) -> (t v -> r) -> r) ->
	 [(Abstracter f v,f a)] -> f r -> f r;
	bindRecursive fixer bindings = applyMutualBindingsRecursive fixer (makeMutualBindings bindings);
	}
