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

module Org.Org.Semantic.HScheme.LambdaCalculus.LookupLambdaExpression(LookupLambdaExpression) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	-- list lengths always the same size
	newtype LookupLambdaExpression sym val a = MkFuncSymbolExpression ((sym -> val) -> a);

	instance HasReturn (LookupLambdaExpression sym val) where
		{
		return a = MkFuncSymbolExpression (const a);
		};

	instance Functor (LookupLambdaExpression sym val) where
		{
		fmap map (MkFuncSymbolExpression func) =
		 {-# SCC "fmap" #-}
		 MkFuncSymbolExpression (map . func);
		};

	instance (Eq sym) => FunctorApply (LookupLambdaExpression sym val) where
		{
		fApply (MkFuncSymbolExpression svab) (MkFuncSymbolExpression sva) = MkFuncSymbolExpression
			(\sv -> svab sv (sva sv));
		};

	instance (Eq sym) => LambdaExpression sym val (LookupLambdaExpression sym val) where
		{
		exprSymbol sym = MkFuncSymbolExpression (\sv -> sv sym);

		exprAbstract abssym (MkFuncSymbolExpression sva) = MkFuncSymbolExpression (\sv v -> sva (\s -> if s == abssym then v else sv s));

--		exprLet abssym (MkFuncSymbolExpression sva) (MkFuncSymbolExpression body)= MkFuncSymbolExpression
--			(\sv -> body (\s -> if s == abssym then (sva sv) else sv s));

		exprLetSym abssym sym (MkFuncSymbolExpression body)= MkFuncSymbolExpression
			(\sv -> body (\s -> sv (if s == abssym then sym else s)));
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (LookupLambdaExpression sym val) where
		{
		runLambda resolve (MkFuncSymbolExpression sva) = sva resolve;
		};
	}
