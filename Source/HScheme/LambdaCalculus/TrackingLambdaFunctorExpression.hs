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

module Org.Org.Semantic.HScheme.LambdaCalculus.TrackingLambdaFunctorExpression(TrackingLambdaFunctorExpression) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaFunctorExpression;
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	data TrackingLambdaFunctorExpression sym lambda f a = MkExpression [sym] (f (lambda a));

	instance (HasReturn f,HasReturn lambda) =>
	 HasReturn (TrackingLambdaFunctorExpression sym lambda f) where
		{
		return a = MkExpression [] (return (return a));
		};

	instance (Functor f,Functor lambda) =>
	 Functor (TrackingLambdaFunctorExpression sym lambda f) where
		{
		fmap map (MkExpression syms expr) = MkExpression syms (fmap (fmap map) expr);
		};

	instance (FunctorApply f,Eq sym,FunctorApply lambda) =>
	 FunctorApply (TrackingLambdaFunctorExpression sym lambda f) where
		{
		fapply (MkExpression syms1 expr1) (MkExpression syms2 expr2) =
		 MkExpression (union syms1 syms2) (liftF2 fapply expr1 expr2);
		};

	instance (FunctorApplyReturn f,LambdaExpression sym val lambda) =>
	 LambdaExpression sym val (TrackingLambdaFunctorExpression sym lambda f) where
		{
		exprSymbol sym = MkExpression [sym] (return (exprSymbol sym));

		exprAbstract abssym (MkExpression syms expr) = MkExpression (remove [abssym] syms) (fmap (exprAbstract abssym) expr);
		};

	instance (RunnableLambdaExpression sym val f,RunnableLambdaExpression sym val lambda) =>
	 RunnableLambdaExpression sym val (TrackingLambdaFunctorExpression sym lambda f) where
		{
		runLambda resolve (MkExpression _ expr) = runLambda resolve (runLambda resolve expr);
		};

	-- ?
	instance (FunctorApplyReturn f,LambdaExpression sym val lambda) =>
	 FreeSymbolLambdaExpression sym val (TrackingLambdaFunctorExpression sym lambda f) where
		{
		exprFreeSymbols (MkExpression syms _) = syms;
		};

	exec :: (Monad m) =>
	 m (m a) -> m a;
	exec mma = mma >>= id;

	instance (Eq sym,Monad lambda) =>
	 LambdaFunctorExpression (TrackingLambdaFunctorExpression sym lambda) where
		{
		fexpMerge (MkExpression syms1 (MkExpression syms2 foo)) =
		 MkExpression (union syms1 syms2) (fmap exec foo);
		};
	}
