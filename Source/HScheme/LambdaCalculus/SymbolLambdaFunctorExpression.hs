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

module Org.Org.Semantic.HScheme.LambdaCalculus.SymbolLambdaFunctorExpression
	(
	SymbolLambdaFunctorExpression
--	,runSymbolLambdaFunctorExpression,runSymbolLambdaFunctorExpressionF
	) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaFunctorExpression;
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	data SymbolLambdaFunctorExpression sym val f a = Closed (f a) |
	 	Open sym (SymbolLambdaFunctorExpression sym val f (val -> a));

	instance (HasReturn f) =>
	 HasReturn (SymbolLambdaFunctorExpression sym val f) where
		{
		return = Closed . return;
		};

	ffmap :: (FunctorApply f) =>
	 f (a -> b) ->
	 SymbolLambdaFunctorExpression sym val f a ->
	 SymbolLambdaFunctorExpression sym val f b;
	ffmap fab (Closed fa) = Closed (fApply fab fa);
	ffmap fab (Open sym exp) = Open sym (ffmap (fmap (.) fab) exp);

	instance (Functor f) =>
	 Functor (SymbolLambdaFunctorExpression sym val f) where
		{
		fmap map (Closed fa) = Closed (fmap map fa);
		fmap map (Open sym sp) = Open sym (fmap (\oa -> (map . oa)) sp);
		};
{-
	instance (Eq sym) =>
	 FunctorApply (SymbolLambdaFunctorExpression sym val f) where
		{
		fApply (Closed fab) rda = ffmap fab rda;
		fApply (Open sym rdoab) rda = 
			Open sym (fApply (fmap (\oab oa o -> oab o (oa o)) rdoab) (fAbstract sym rda));
		};

	instance (Eq sym) => FunctorLambda sym val (SymbolLambdaFunctorExpression sym val f) where
		{
		fSymbol sym = Open sym (Closed id);

		fAbstract sym (Closed a) = Closed (const a);
		fAbstract sym (Open sym' sp) | sym == sym' = sp;
		fAbstract sym (Open sym' sp) = Open sym' (fmap (\a val' val -> a val val') (fAbstract sym sp));
		};

	runSymbolLambdaFunctorExpression ::
	 (sym -> val) ->
	 SymbolLambdaFunctorExpression sym val a ->
	 a;
	runSymbolLambdaFunctorExpression resolve (Closed a) = a;
	runSymbolLambdaFunctorExpression resolve (Open sym sp) =
	 runSymbolLambdaFunctorExpression resolve sp (resolve sym);

	runSymbolLambdaFunctorExpressionF :: (FunctorApplyReturn f) =>
	 (sym -> f val) ->
	 SymbolLambdaFunctorExpression sym val a ->
	 f a;
	runSymbolLambdaFunctorExpressionF resolve (Closed a) = return a;
	runSymbolLambdaFunctorExpressionF resolve (Open sym sp) = liftF2
	 (\val oa -> oa val) (resolve sym) (runSymbolExpressionF resolve sp);
-}
	}
