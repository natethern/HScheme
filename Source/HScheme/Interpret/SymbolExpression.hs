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

module Org.Org.Semantic.HScheme.Interpret.SymbolExpression(SymbolExpression) where
	{
	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HBase;

	data SymbolExpression sym val a = ClosedSymbolExpression a |
	 	OpenSymbolExpression sym (SymbolExpression sym val (val -> a));

	instance HasReturn (SymbolExpression sym val) where
		{
		return = ClosedSymbolExpression;
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
			OpenSymbolExpression sym (fApply (fmap (\oab oa o -> oab o (oa o)) rdoab) (exprAbstract sym rda));
		};

	instance (Eq sym) => LambdaExpression sym val (SymbolExpression sym val) where
		{
		exprSymbol sym = OpenSymbolExpression sym (ClosedSymbolExpression id);

		exprAbstract sym (ClosedSymbolExpression a) = ClosedSymbolExpression (const a);
		exprAbstract sym (OpenSymbolExpression sym' sp) | sym == sym' = sp;
		exprAbstract sym (OpenSymbolExpression sym' sp) = OpenSymbolExpression sym' (fmap (\a val' val -> a val val') (exprAbstract sym sp));
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (SymbolExpression sym val) where
		{
		runLambda resolve (ClosedSymbolExpression a) = a;
		runLambda resolve (OpenSymbolExpression sym sp) = runLambda resolve sp (resolve sym);
		};

	instance (Eq sym) => FreeSymbolLambdaExpression sym val (SymbolExpression sym val) where
		{
		exprFreeSymbols (ClosedSymbolExpression _) = [];
		exprFreeSymbols (OpenSymbolExpression sym expr) = (sym:exprFreeSymbols expr);
		};
	}
