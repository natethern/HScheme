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

module Org.Org.Semantic.HScheme.SymbolExpression where
	{
	import Org.Org.Semantic.HBase;
import Control.Monad.Fix(fix);

	-- SSA?
	class (Eq sym,FunctorApplyReturn f) =>
	 FunctorLambda sym val f | f -> sym val where
		{
		fSymbol :: sym -> f val;
		fAbstract :: sym -> f a -> f (val -> a);
		fSubst :: sym -> f val -> f a -> f a;

		fSubst sym valueExpr bodyExpr = fApply (fAbstract sym bodyExpr) valueExpr;
		};

	-- | substs are actually done in reverse order
	fSubstSequential :: (FunctorLambda sym val f) => [(sym,f val)] -> f a -> f a;
	fSubstSequential [] bodyExpr = bodyExpr;
	fSubstSequential ((sym,valueExpr):binds) bodyExpr = fSubst sym valueExpr (fSubstSequential binds bodyExpr);

	fSubstSeparate :: (FunctorLambda sym val f) => [(sym,f val)] -> f a -> f a;
	fSubstSeparate = fSubstSequential; -- NYI

	selfSubst :: (FunctorLambda sym val f) => sym -> f val -> f val;
	selfSubst sym valueExpr = fmap fix (fAbstract sym valueExpr);

{--
	liftF3 :: (FunctorApply f) => (a -> b -> c -> r) -> (f a -> f b -> f c -> f r);
	liftF3 map fa fb fc = fApply (liftF2 map fa fb) fc;

	liftF4 :: (FunctorApply f) => (a -> b -> c -> d -> r) -> (f a -> f b -> f c -> f d -> f r);
	liftF4 map fa fb fc fd = fApply (liftF3 map fa fb fc) fd;

	subst1 :: (FunctorLambda sym val f) => (sym,f val) -> f a -> f a;
	subst1 (s1,f1) fa = liftF2 (\x1 xa -> let
		{
		t1 = x1 t1;
		ta = xa t1;
		} in ta) (fAbstract s1 f1) (fAbstract s1 fa);

	subst2 :: (FunctorLambda sym val f) => ((sym,f val),(sym,f val)) -> f a -> f a;
	subst2 ((s1,f1),(s2,f2)) fa = let
		{
		fv1 = (fAbstract s1 (fAbstract s2 f1));
		fv2 = (fAbstract s1 (fAbstract s2 f2));
		fva = (fAbstract s1 (fAbstract s2 fa));
		} in liftF3 (\x1 x2 xa -> let
		{
		t1 = x1 t1 t2;
		t2 = x2 t1 t2;
		ta = xa t1 t2;
		} in ta) fv1 fv2 fva;
--}
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

	class (ExtractableFunctor t) => LT t where
		{
		};

	instance LT ZeroList where
		{
		};

	instance (LT t) => LT (NextList t) where
		{
		};
{--
	class (ExtractableFunctor t) => LT a r t d | a r t -> d, t d -> r {--, a r d -> t --} where
		{
		ltApply :: d -> t a -> r
		};

	instance LT a r ZeroList r where
		{
		ltApply r _ = r;
		};

	instance (LT a r t d) => LT a r (NextList t) (a -> d) where
		{
		ltApply ad (MkNextList a ta) = ltApply (ad a) ta;
		};
--}

data LTtf f a r = forall t. (LT t) => MkLTtf (t (f (t a -> a))) (f (t a -> r));
--zz

data FMap t f a = MkFMap (forall r. f r -> f (t a -> r));

data MapQ f a = forall t. (LT t) => MkMapQ (t (f a)) (FMap t f a);


appmap1 :: (LT t) => FMap t f a -> t (f a) -> t (f (t a -> a));
appmap1 (MkFMap frfd) tfa = fmap frfd tfa;

appmap2 :: (LT t) => FMap t f a -> f r -> f (t a -> r);
appmap2 (MkFMap frfd) fr = frfd fr;

foo :: MapQ f a -> f r -> LTtf f a r;
foo (MkMapQ tfa frfd) fr = MkLTtf (appmap1 frfd tfa) (appmap2 frfd fr);

	fixFunc :: (Functor t) => t (t a -> a) -> t a;
	fixFunc t = fix (\p -> (fmap (\x -> x p) t));

thing :: (LT t) => t (t a -> a) -> (t a -> r) -> r;
thing tad rd = rd (fixFunc tad);


liftThing :: (FunctorApplyReturn f) => LTtf f val r -> f r;
--liftThing = undefined;
liftThing (MkLTtf tf frd) = liftF2 thing (fExtract tf) frd;

zz :: (LT t) => (a -> t a -> r) -> (NextList t) a -> r;
zz = (\atar (MkNextList a ta) -> atar a ta);

tackOn :: (LT t,Functor f) =>
	(forall r. f r -> f (a -> r)) ->
	FMap t f a ->
	FMap (NextList t) f a;
tackOn fm (MkFMap frftar) = MkFMap (\fr -> fmap zz (fm (frftar fr)))  ;

	data MapV f a = MkMapV (forall r. f r -> f (a -> r));

	xbinds :: (FunctorLambda sym val f) => [(MapV f val,f val)] -> MapQ f val;
	xbinds [] = MkMapQ MkZeroList (MkFMap (fmap (\r MkZeroList -> r)));
	xbinds ((MkMapV fm,fv):rr) = case (xbinds rr) of
		{
		(MkMapQ tfa frftar) -> MkMapQ (MkNextList fv tfa) (tackOn fm frftar);
		};
	
	

	cbinds :: (FunctorLambda sym val f) => [(sym,f val)] -> [(MapV f val,f val)];
	cbinds = fmap (\(sym,fval) -> (MkMapV (fAbstract sym),fval));

	fSubstRecursive :: (FunctorLambda sym val f) => [(sym,f val)] -> f r -> f r;
	fSubstRecursive binds fr = liftThing (foo (xbinds (cbinds binds)) fr);
{--
	subst3 :: (FunctorLambda sym val f) => ((sym,f val),(sym,f val),(sym,f val)) -> f a -> f a;
	subst3 ((s1,f1),(s2,f2),(s3,f3)) fa = liftF4 (\x1 x2 x3 xa -> let
		{
		t1 = x1 t1 t2 t3;
		t2 = x2 t1 t2 t3;
		t3 = x3 t1 t2 t3;
		} in xa t1 t2 t3)
		 (fAbstract s1 (fAbstract s2 (fAbstract s3 f1)))
		 (fAbstract s1 (fAbstract s2 (fAbstract s3 f2)))
		 (fAbstract s1 (fAbstract s2 (fAbstract s3 f3)))
		 (fAbstract s1 (fAbstract s2 (fAbstract s3 fa)));
--}
	fSubstRecursive1 :: (FunctorLambda sym val f) => (sym,f val) -> f a -> f a;
	fSubstRecursive1 (sym,valueExpr) bodyExpr = fSubst sym (selfSubst sym valueExpr) bodyExpr;

	mutualSubst :: (FunctorLambda sym val f) => [(sym,f val)] -> [(sym,f val)];
	-- NYI: this is wrong, it ignores mutual references
	mutualSubst binds = fmap (\(sym,valueExpr) -> (sym,selfSubst sym valueExpr)) binds;

--	fSubstRecursive :: (FunctorLambda sym val f) => [(sym,f val)] -> f a -> f a;
--	fSubstRecursive binds = fSubstSequential (mutualSubst binds);

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

{--
ff :: (Integer -> Integer) -> Integer -> Integer;
ff = \fact x -> if (x == 0) then 1 else (x * (fact (x - 1)));

factLambdaExpr :: SymbolExpression String (Integer -> Integer) ((Integer -> Integer) -> Integer -> Integer);
factLambdaExpr = ClosedSymbolExpression ff;

factExpr :: SymbolExpression String (Integer -> Integer) (Integer -> Integer);
factExpr = OpenSymbolExpression "fact" factLambdaExpr;

factorial :: SymbolExpression String (Integer -> Integer) (Integer -> Integer);
factorial = selfSubst "fact" factExpr;
--factorial = fSubst "fact" factorial factExpr;
--factorial = fApply (fAbstract "fact" factExpr) factorial;
--factorial = fApply factLambdaExpr factorial;
--factorial = fmap ff factorial;
--factorial = let {fact' = ff fact';} in ClosedSymbolExpression fact';




extractClosed :: SymbolExpression sym val a -> a;
extractClosed (ClosedSymbolExpression a) = a;
extractClosed _ = error "open SymbolExpression";

test1 :: Integer;
test1 = extractClosed factorial 7;
--}
	}
