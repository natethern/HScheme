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

module Org.Org.Semantic.HScheme.Interpret.FuncSymbolExpression(FuncSymbolExpression,TrackingExpression) where
	{
	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HBase;

	-- list lengths always the same size
	newtype FuncSymbolExpression sym val a = MkFuncSymbolExpression ((sym -> val) -> a);

	instance HasReturn (FuncSymbolExpression sym val) where
		{
		return a = MkFuncSymbolExpression (const a);
		};

	instance Functor (FuncSymbolExpression sym val) where
		{
		fmap map (MkFuncSymbolExpression func) =
		 {-# SCC "fmap" #-}
		 MkFuncSymbolExpression (map . func);
		};

	instance (Eq sym) => FunctorApply (FuncSymbolExpression sym val) where
		{
		fApply (MkFuncSymbolExpression svab) (MkFuncSymbolExpression sva) = MkFuncSymbolExpression
			(\sv -> svab sv (sva sv));
		};

	instance (Eq sym) => LambdaExpression sym val (FuncSymbolExpression sym val) where
		{
		exprSymbol sym = MkFuncSymbolExpression (\sv -> sv sym);

		exprAbstract abssym (MkFuncSymbolExpression sva) = MkFuncSymbolExpression (\sv v -> sva (\s -> if s == abssym then v else sv s));
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (FuncSymbolExpression sym val) where
		{
		runLambda resolve (MkFuncSymbolExpression sva) = sva resolve;
		};

	data TrackingExpression sym val a = MkTrackingExpression [sym] (FuncSymbolExpression sym val a);

	instance HasReturn (TrackingExpression sym val) where
		{
		return a = MkTrackingExpression [] (return a);
		};

	instance Functor (TrackingExpression sym val) where
		{
		fmap map (MkTrackingExpression syms expr) = MkTrackingExpression syms (fmap map expr);
		};

	instance (Eq sym) => FunctorApply (TrackingExpression sym val) where
		{
		fApply (MkTrackingExpression syms1 expr1) (MkTrackingExpression syms2 expr2) =
		 MkTrackingExpression (union syms1 syms2) (fApply expr1 expr2);
		};

	instance (Eq sym) => LambdaExpression sym val (TrackingExpression sym val) where
		{
		exprSymbol sym = MkTrackingExpression [sym] (exprSymbol sym);

		exprAbstract abssym (MkTrackingExpression syms expr) = MkTrackingExpression (remove [abssym] syms) (exprAbstract abssym expr);
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (TrackingExpression sym val) where
		{
		runLambda resolve (MkTrackingExpression _ expr) = runLambda resolve expr;
		};

	instance (Eq sym) => FreeSymbolLambdaExpression sym val (TrackingExpression sym val) where
		{
		exprFreeSymbols (MkTrackingExpression syms _) = syms;
		};
	}
