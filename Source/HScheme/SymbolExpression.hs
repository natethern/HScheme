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

	-- SSA?
	class (Eq sym,FunctorApplyReturn f) =>
	 FunctorLambda sym val f | f -> sym val where
		{
		fSymbol :: sym -> f val;
		fAbstract :: sym -> f a -> f (val -> a);
		};

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
		};

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
