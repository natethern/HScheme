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

module Org.Org.Semantic.HScheme.Interpret.Binder where
	{
	import Org.Org.Semantic.HScheme.Interpret.TopLevel;
	import Org.Org.Semantic.HScheme.Interpret.Abstract;
	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	setBinder :: (FullScheme m r) =>
	 TopLevelBinder r m;
	setBinder = MkTopLevelBinder (\(MkTopLevelCommand expr binds _) ->
	 exprLetBindsNewRef binds (seqSets binds expr)) where
		{
		liftF3' :: (FunctorApply f) =>
		 (a -> b -> c -> r) ->
		 (f a -> f b -> f c -> f r);
		liftF3' func fa fb =
		 {-# SCC "liftF3'" #-}
		 fApply (fApply (fmap func fa) fb);

		seqSets [] bodyexpr = bodyexpr;
		seqSets ((sym,bindexpr):rest) bodyexpr = 
		 {-# SCC "seqSets" #-}
		 liftF3' setBindValue (exprSymbol sym) bindexpr (seqSets rest bodyexpr);

		setBindValue ref bindaction bodyaction =
		 {-# SCC "setBindValue" #-}
		 do
			{	-- same as (set! sym expr)
			bindvalue <- bindaction;
			set ref bindvalue;
			bodyaction;
			};

		exprLetBindsNewRef [] bodyexpr = bodyexpr;
		exprLetBindsNewRef ((sym,_):rest) bodyexpr = 
		 {-# SCC "exprLetBindsNewRef" #-}
		 schemeExprLet sym (return (newSymbolBinding sym)) (exprLetBindsNewRef rest bodyexpr);

		newSymbolBinding :: (Monad m) =>
		 Symbol -> m (Object r m);
		newSymbolBinding sym = 
		 {-# SCC "newSymbolBinding" #-}
		 return (error ("uninitialised binding: " ++ (show sym)));
		};

	recursiveBinder :: (MonadFix m,Scheme m r) =>
	 TopLevelBinder r m;
	recursiveBinder = MkTopLevelBinder (\(MkTopLevelCommand expr binds _) ->
	 schemeExprLetRecursive binds expr);
	}
