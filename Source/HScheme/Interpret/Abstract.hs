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

module Org.Org.Semantic.HScheme.Interpret.Abstract
	(
	schemeExprAbstractList,
	schemeExprLet,
	schemeExprLetSeparate,
	schemeExprLetSequential,
	schemeExprLetRecursive
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	substMap :: (Scheme m r) =>
	 (ObjLocation r m -> m a) -> Object r m -> m a;
	substMap va value = do
		{
		loc <- new value;
		va loc;
		};

	mSubstMap :: (Scheme m r) =>
	 (ObjLocation r m -> m a) -> m (Object r m) -> m a;
	mSubstMap va mvalue = do
		{
	 	value <- mvalue;
		substMap va value;
		};

	schemeExprAbstractList ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 [Symbol] -> SchemeExpression r m (m a) ->
	 SchemeExpression r m ([Object r m] -> (m a));
	schemeExprAbstractList = exprAbstractListGuardedMap substMap (throwSimpleError "too-few-args") (\args -> throwArgError "too-many-args" undefined);

	schemeExprLet :: (Scheme m r) =>
	 Symbol -> ObjectSchemeExpression r m ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m a);
	schemeExprLet = exprLetMap mSubstMap;

	schemeExprLetSeparate :: (Scheme m r) =>
	 [(Symbol,ObjectSchemeExpression r m)] ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m a);
	schemeExprLetSeparate = exprLetMapSeparate separater where
		{
		separater :: (ExtractableFunctor t,Scheme m r) =>
		 t (m (Object r m)) -> (t (ObjLocation r m) -> m a) -> m a;
		separater bindExprs absBody = do
			{
			objs <- fExtract bindExprs;
			locs <- fExtract (fmap new objs);
			absBody locs;
			};
		};

	schemeExprLetSequential :: (Scheme m r) =>
	 [(Symbol,ObjectSchemeExpression r m)] ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m a);
	schemeExprLetSequential = exprLetMapSequential mSubstMap;

	schemeExprLetRecursive :: (MonadFix m,Scheme m r) =>
	 [(Symbol,ObjectSchemeExpression r m)] ->
	 SchemeExpression r m (m a) ->
	 SchemeExpression r m (m a);
	schemeExprLetRecursive = exprLetMapRecursive fixer where
		{
--		fixFunctor :: (Functor t) => t (t a -> a) -> t a;
--		fixFunctor t = fix (\p -> (fmap (\x -> x p) t));

		mfixExFunctor :: (MonadFix m,ExtractableFunctor t) =>
		 t (t a -> m a) -> m (t a);
		mfixExFunctor t = mfix (\p -> fExtract (fmap (\x -> x p) t));

		fixer :: (MonadCreatable m r,MonadFix m,ExtractableFunctor t) =>
		 t (t (r v) -> m v) ->
		 (t (r v) -> m a) ->
		 m a;
		fixer bindsT bodyT = (mfixExFunctor (fmap (\tlocmobj tloc -> do
			{
			obj <- tlocmobj tloc;
			new obj;
			}) bindsT)) >>= bodyT;
		};
	}
