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
	setBinder,fixedPointBinder
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.TopLevel;
	import Org.Org.Semantic.HScheme.Interpret.Abstract;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	newSymbolBindingExpression :: (Monad m) =>
	 Symbol -> ObjectSchemeExpression r obj m;
	newSymbolBindingExpression sym =
	 return (return (error ("uninitialised binding: " ++ (show sym))));

	schemeExprLetNewRefs ::
		(
		InterpretObject m r obj
		) =>
	 [Symbol] -> SchemeExpression r obj (m a) -> SchemeExpression r obj (m a);
	schemeExprLetNewRefs [] bodyexpr = bodyexpr;
	schemeExprLetNewRefs (sym:syms) bodyexpr = 
	 schemeExprLet sym (newSymbolBindingExpression sym) (schemeExprLetNewRefs syms bodyexpr);

	bindsToSetSequence ::
		(
		InterpretObject m r obj,
		FullBuild m r
		) =>
	 [(Symbol,ObjectSchemeExpression r obj m)] ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m a);
	bindsToSetSequence [] bodyexpr = bodyexpr;
	bindsToSetSequence ((sym,bindexpr):binds) bodyexpr = 
	 liftF3 setBindValue (schemeExprSymbol sym) bindexpr (bindsToSetSequence binds bodyexpr) where
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

	interactiveBind ::
		(
		InterpretObject m r obj,
		FullBuild m r
		) =>
	 [(Symbol,ObjectSchemeExpression r obj m)] ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m (a,[(Symbol,r obj)]));
	interactiveBind binds expr = schemeExprLetNewRefsCollectLocs binds (bindsToSetSequence binds expr) where
		{
		schemeExprLetNewRefsCollectLocs [] bodyexpr = fmap (fmap (\a -> (a,[]))) bodyexpr;
		schemeExprLetNewRefsCollectLocs ((sym,_):rbinds) bodyexpr = 
		 {-# SCC "schemeExprLetNewRefsCollectLocs" #-}
		 gather sym (schemeExprLocLet sym (newSymbolBindingExpression sym) (schemeExprLetNewRefsCollectLocs rbinds bodyexpr));

		gather sym = fmap (fmap (\((a,rbinds),loc) -> (a,(sym,loc):rbinds)))
		};

	fixedPointBinder ::
		(
		MonadFix m,
		InterpretObject m r obj
		) => TopLevelBinder r obj m;
	fixedPointBinder = MkTopLevelBinder schemeExprLetRecursiveFixed;

	setBinder ::
		(
		InterpretObject m r obj,
		FullBuild m r
		) =>
	 TopLevelBinder r obj m;
	setBinder =
	 {-# SCC "setBinder.root" #-}
	 t5 where
		{
		t1 =
		 {-# SCC "setBinder.t1" #-}
		 fmap' fst';
		
		t2 =
		 {-# SCC "setBinder.t2" #-}
		 bindsToSetSequence;
		
		t3 binds =
		 {-# SCC "setBinder.t3" #-}
		 schemeExprLetNewRefs (t1 binds);
		
		t4 binds expr =
		 {-# SCC "setBinder.t4" #-}
		 (t3 binds) (t2 binds expr);
		
		t5 =
		 {-# SCC "setBinder.t5" #-}
		 MkTopLevelBinder t4;

		fst' :: (a,b) -> a;
		fst' = 
		 {-# SCC "setBinder.fst'" #-}
		 fst;

		fmap' :: (Functor f) => (a -> b) -> f a -> f b;
		fmap' = 
		 {-# SCC "setBinder.fmap'" #-}
		 fmap;
		};
	}
