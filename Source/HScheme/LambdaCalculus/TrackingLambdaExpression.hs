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

module Org.Org.Semantic.HScheme.LambdaCalculus.TrackingLambdaExpression(TrackingLambdaExpression) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	data TrackingLambdaExpression sym lambda a = MkTrackingExpression [sym] (lambda a);

	instance (HasReturn lambda) =>
	 HasReturn (TrackingLambdaExpression sym lambda) where
		{
		return a = MkTrackingExpression [] (return a);
		};

	instance (Functor lambda) =>
	 Functor (TrackingLambdaExpression sym lambda) where
		{
		fmap map (MkTrackingExpression syms expr) = MkTrackingExpression syms (fmap map expr);
		};

	instance (Eq sym,FunctorApply lambda) =>
	 FunctorApply (TrackingLambdaExpression sym lambda) where
		{
		fapply (MkTrackingExpression syms1 expr1) (MkTrackingExpression syms2 expr2) =
		 MkTrackingExpression (union syms1 syms2) (fapply expr1 expr2);
		};

	instance (LambdaExpression sym val lambda) =>
	 LambdaExpression sym val (TrackingLambdaExpression sym lambda) where
		{
		exprSymbol sym = MkTrackingExpression [sym] (exprSymbol sym);

		exprAbstract abssym (MkTrackingExpression syms expr) = MkTrackingExpression (remove [abssym] syms) (exprAbstract abssym expr);
		};

	instance (RunnableLambdaExpression sym val lambda) =>
	 RunnableLambdaExpression sym val (TrackingLambdaExpression sym lambda) where
		{
		runLambda resolve (MkTrackingExpression _ expr) = runLambda resolve expr;
		};

	instance (LambdaExpression sym val lambda) =>
	 FreeSymbolLambdaExpression sym val (TrackingLambdaExpression sym lambda) where
		{
		exprFreeSymbols (MkTrackingExpression syms _) = syms;
		};
	}
