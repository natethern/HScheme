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

module Org.Org.Semantic.HScheme.LambdaCalculus.SymbolLambdaExpression(SymbolLambdaExpression) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	data SymbolLambdaExpression sym val a = Closed a |
	 	Open sym (SymbolLambdaExpression sym val (val -> a));

	instance HasReturn (SymbolLambdaExpression sym val) where
		{
		return = Closed;
		};

	instance Functor (SymbolLambdaExpression sym val) where
		{
		fmap map (Closed a) = Closed (map a);
		fmap map (Open sym sp) = Open sym (fmap (\oa -> (map . oa)) sp);
		};

	instance (Eq sym) => FunctorApply (SymbolLambdaExpression sym val) where
		{
		fApply (Closed ab) rda = fmap ab rda;
		fApply (Open sym rdoab) rda = 
			Open sym (fApply (fmap (\oab oa o -> oab o (oa o)) rdoab) (exprAbstract sym rda));
		};

	instance (Eq sym) => LambdaExpression sym val (SymbolLambdaExpression sym val) where
		{
		exprSymbol sym = Open sym (Closed id);

		exprAbstract sym (Closed a) = Closed (const a);
		exprAbstract sym (Open sym' sp) | sym == sym' = sp;
		exprAbstract sym (Open sym' sp) = Open sym' (fmap (\a val' val -> a val val') (exprAbstract sym sp));
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (SymbolLambdaExpression sym val) where
		{
		runLambda resolve (Closed a) = a;
		runLambda resolve (Open sym sp) = runLambda resolve sp (resolve sym);
		};

	instance (Eq sym) => FreeSymbolLambdaExpression sym val (SymbolLambdaExpression sym val) where
		{
		exprFreeSymbols (Closed _) = [];
		exprFreeSymbols (Open sym expr) = (sym:exprFreeSymbols expr);
		};
	}
