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

module Org.Org.Semantic.HScheme.Interpret.LambdaExpression
	(
	LambdaExpression(..),RunnableLambdaExpression(..),FreeSymbolLambdaExpression(..),
	exprAbstractMap,
	exprAbstractList,exprAbstractListMap,
	exprAbstractListGuarded,exprAbstractListGuardedMap,
	exprLet,exprLetMap,exprLetSequential,
	exprLetMapSequential,exprLetMapSeparate,exprLetMapRecursive,
	exprConstMapLet
	) where
	{
	import Org.Org.Semantic.HBase;


	-- LambdaExpression

	-- SSA?
	class (Eq sym,FunctorApplyReturn f) =>
	 LambdaExpression sym val f | f -> sym val where
		{
		exprSymbol :: sym -> f val;
		exprAbstract :: sym -> f r -> f (val -> r);
		};

	exprAbstractMap :: (LambdaExpression sym val f) =>
	 ((val -> r) -> a -> r) ->
	 sym -> f r -> f (a -> r);
	exprAbstractMap map sym expr = fmap map (exprAbstract sym expr);

	-- lists always the same size
	exprAbstractList :: (LambdaExpression sym val f) =>
	 [sym] -> f r -> f ([val] -> r);
	exprAbstractList [] fr = fmap (\r [] -> r) fr;
	exprAbstractList (sym:syms) fr =
	 fmap (\valvalsr (val:vals) -> valvalsr val vals) (exprAbstract sym (exprAbstractList syms fr));

	-- lists always the same size
	exprAbstractListMap :: (LambdaExpression sym val f) =>
	 ((val -> r) -> a -> r) ->
	 [sym] -> f r -> f ([a] -> r);
	exprAbstractListMap map [] fr = fmap (\r [] -> r) fr;
	exprAbstractListMap map (sym:syms) fr =
	 fmap (\valasr (a:as) -> map (\val -> valasr val as) a) (exprAbstract sym (exprAbstractListMap map syms fr));

	exprAbstractListGuarded :: (LambdaExpression sym val f) =>
	 r ->
	 ([val] -> r) ->
	 [sym] -> f r -> f ([val] -> r);
	exprAbstractListGuarded tooFewArgs tooManyArgs [] fr = fmap (\r args -> case args of
		{
		[] -> r;
		_ -> tooManyArgs args;
		}) fr;
	exprAbstractListGuarded tooFewArgs tooManyArgs (sym:syms) fr =
	 fmap (\valvalsr args -> case args of
		{
		(val:vals) -> valvalsr val vals;
		[] -> tooFewArgs;
		}) (exprAbstract sym (exprAbstractListGuarded tooFewArgs tooManyArgs syms fr));

	exprAbstractListGuardedMap :: (LambdaExpression sym val f) =>
	 ((val -> r) -> a -> r) ->
	 r ->
	 ([a] -> r) ->
	 [sym] -> f r -> f ([a] -> r);
	exprAbstractListGuardedMap map tooFewArgs tooManyArgs [] fr = fmap (\r args -> case args of
		{
		[] -> r;
		_ -> tooManyArgs args;
		}) fr;
	exprAbstractListGuardedMap map tooFewArgs tooManyArgs (sym:syms) fr =
	 fmap (\valasr args -> case args of
		{
		(a:as) -> map (\val -> valasr val as) a;
		[] -> tooFewArgs;
		}) (exprAbstract sym (exprAbstractListGuardedMap map tooFewArgs tooManyArgs syms fr));

	exprLet :: (LambdaExpression sym val f) =>
	 sym -> f val -> f r -> f r;
	exprLet sym valueExpr bodyExpr = fApply (exprAbstract sym bodyExpr) valueExpr;

	-- | substs are actually done in reverse order
	exprLetSequential :: (LambdaExpression sym val f) =>
	 [(sym,f val)] -> f r -> f r;
	exprLetSequential [] bodyExpr = bodyExpr;
	exprLetSequential ((sym,valueExpr):binds) bodyExpr = exprLet sym valueExpr (exprLetSequential binds bodyExpr);

	exprLetMap :: (LambdaExpression sym val f) =>
	 ((val -> r1) -> a -> r2) ->
	 sym -> f a -> f r1 -> f r2;
	exprLetMap map sym valueExpr bodyExpr = liftF2 map (exprAbstract sym bodyExpr) valueExpr;

	-- | substs are actually done in reverse order
	exprLetMapSequential :: (LambdaExpression sym val f) =>
	 ((val -> r) -> a -> r) -> [(sym,f a)] -> f r -> f r;
	exprLetMapSequential _ [] bodyExpr = bodyExpr;
	exprLetMapSequential map ((sym,valueExpr):binds) bodyExpr = exprLetMap map sym valueExpr (exprLetMapSequential map binds bodyExpr);


	-- RunnableLambdaExpression

	class (LambdaExpression sym val f) =>
	 RunnableLambdaExpression sym val f | f -> sym val where
		{
		runLambda :: (sym -> val) -> f a -> a;
		};


	-- FreeSymbolLambdaExpression

	class (LambdaExpression sym val f) =>
	 FreeSymbolLambdaExpression sym val f | f -> sym val where
		{
		exprFreeSymbols :: f a -> [sym];
		};

	purgeMapList :: (a -> Maybe b) -> [a] -> [b];
	purgeMapList _ [] = [];
	purgeMapList map (a:as) | Just b <- map a = (b:purgeMapList map as);
	purgeMapList map (a:as) = purgeMapList map as;

	exprConstMapLet :: (FreeSymbolLambdaExpression sym val f) =>
	 (sym -> Maybe val) -> f a -> f a;
	exprConstMapLet map expr =
	 exprLetSequential (purgeMapList (\sym -> fmap ((\val -> (sym,return val))) (map sym)) (exprFreeSymbols expr)) expr;


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
		fExtract _ = return MkZeroList;
		};

	instance (ExtractableFunctor t) => ExtractableFunctor (NextList t) where
		{
		fExtract (MkNextList ga tga) = liftF2 MkNextList ga (fExtract tga);
		};


	-- MutualBindings

	data MutualBindings f a v = forall t. (ExtractableFunctor t) =>
	 MkMutualBindings (t (f a)) (forall r. f r -> f (t v -> r));

	-- uses irrefutable patterns (~) here to get mfix to work
	makeMutualBindings :: (LambdaExpression sym val f) =>
	 [(sym,f a)] -> MutualBindings f a val;
	makeMutualBindings [] = MkMutualBindings MkZeroList (fmap (\r ~MkZeroList -> r));
	makeMutualBindings ((sym,fval):restb) = case (makeMutualBindings restb) of
		{
		(MkMutualBindings bindValues abstracter) -> MkMutualBindings
		 (MkNextList fval bindValues)
		 (\body -> fmap (\atar ~(MkNextList a ta) -> atar a ta) ((exprAbstract sym) (abstracter body)));
		};

	applyMutualBindingsSeparate :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t a -> (t val -> r) -> r) ->
	 MutualBindings f a val -> f r -> f r;
	applyMutualBindingsSeparate separater (MkMutualBindings bindValues abstracter) body =
	 liftF2 separater (fExtract bindValues) (abstracter body);

	exprLetMapSeparate :: (LambdaExpression sym val f) =>
	 (forall t. (ExtractableFunctor t) => t a -> (t val -> r) -> r) ->
	 [(sym,f a)] -> f r -> f r;
	exprLetMapSeparate separater bindings = applyMutualBindingsSeparate separater (makeMutualBindings bindings);

	applyMutualBindingsRecursive :: (FunctorApplyReturn f) =>
	 (forall t. (ExtractableFunctor t) => t (t val -> a) -> (t val -> r) -> r) ->
	 MutualBindings f a val -> f r -> f r;
	applyMutualBindingsRecursive fixer (MkMutualBindings bindValues abstracter) body =
	 liftF2 fixer (fExtract (fmap abstracter bindValues)) (abstracter body);

	exprLetMapRecursive :: (LambdaExpression sym val f) =>
	 (forall t. (ExtractableFunctor t) => t (t val -> a) -> (t val -> r) -> r) ->
	 [(sym,f a)] -> f r -> f r;
	exprLetMapRecursive fixer bindings = applyMutualBindingsRecursive fixer (makeMutualBindings bindings);
	}
