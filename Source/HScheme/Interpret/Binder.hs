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

module Org.Org.Semantic.HScheme.Interpret.Binder
	(
	interactiveBind,schemeExprLetNewRefs,
	setBinder,recursiveBinder
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.TopLevel;
	import Org.Org.Semantic.HScheme.Interpret.Abstract;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	newSymbolBindingExpression :: (Monad m) =>
	 Symbol -> ObjectSchemeExpression r m;
	newSymbolBindingExpression sym =
	 return (return (error ("uninitialised binding: " ++ (show sym))));

	schemeExprLetNewRefs :: (FullScheme m r) =>
	 [Symbol] -> SchemeExpression r m (m a) -> SchemeExpression r m (m a);
	schemeExprLetNewRefs [] bodyexpr = bodyexpr;
	schemeExprLetNewRefs (sym:rest) bodyexpr = 
	 schemeExprLet sym (newSymbolBindingExpression sym) (schemeExprLetNewRefs rest bodyexpr);

	bindsToSetSequence :: (FullScheme m r) =>
	 [(Symbol,ObjectSchemeExpression r m)] ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m a);
	bindsToSetSequence [] bodyexpr = bodyexpr;
	bindsToSetSequence ((sym,bindexpr):rest) bodyexpr = 
	 liftF3 setBindValue (exprSymbol sym) bindexpr (bindsToSetSequence rest bodyexpr) where
		{
		setBindValue ref bindaction bodyaction =
		 {-# SCC "setBindValue" #-}
		 do
			{	-- same as (set! sym expr)
			bindvalue <- bindaction;
			set ref bindvalue;
			bodyaction;
			};
		};

	interactiveBind :: (FullScheme m r) =>
	 [(Symbol,ObjectSchemeExpression r m)] ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m (a,[(Symbol,ObjLocation r m)]));
	interactiveBind binds expr = schemeExprLetNewRefsCollectLocs binds (bindsToSetSequence binds expr) where
		{
		schemeExprLetNewRefsCollectLocs [] bodyexpr = fmap (fmap (\a -> (a,[]))) bodyexpr;
		schemeExprLetNewRefsCollectLocs ((sym,_):rest) bodyexpr = 
		 {-# SCC "schemeExprLetNewRefsCollectLocs" #-}
		 gather sym (schemeExprLocLet sym (newSymbolBindingExpression sym) (schemeExprLetNewRefsCollectLocs rest bodyexpr));

		gather sym = fmap (fmap (\((a,binds),loc) -> (a,(sym,loc):binds)))
		};


	setBinder :: (FullScheme m r) => TopLevelBinder r m;
	setBinder = MkTopLevelBinder
	 (\binds expr -> schemeExprLetNewRefs (fmap fst binds) (bindsToSetSequence binds expr));

	recursiveBinder :: (MonadFix m,Scheme m r) => TopLevelBinder r m;
	recursiveBinder = MkTopLevelBinder schemeExprLetRecursive;
	}
