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
	schemeExprRefAbstractList,
	schemeExprLet,
	schemeExprLetSeparate,
	schemeExprLetSequential,
	schemeExprLetRecursive,
	schemeExprLocLet
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.LambdaCalculus;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	substMap :: (Build m r) =>
	 (r obj -> m a) -> obj -> m a;
	substMap va value = do
		{
		loc <- new value;
		va loc;
		};

	mSubstMap :: (Build m r) =>
	 (r obj -> m a) -> m obj -> m a;
	mSubstMap va mvalue = do
		{
	 	value <- mvalue;
		substMap va value;
		};

	mSubstLocMap :: (Build m r) =>
	 (r obj -> m a) -> m obj -> m (a,r obj);
	mSubstLocMap va mvalue = do
		{
	 	value <- mvalue;
		loc <- new value;
		result <- va loc;
		return (result,loc);
		};

	schemeExprAbstractList ::
		(
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 [Symbol] -> SchemeExpression r obj (m a) ->
	 SchemeExpression r obj ([obj] -> (m a));
	schemeExprAbstractList =
	 exprAbstractListGuardedMap substMap throwTooFewArgumentsError throwTooManyArgumentsError;

	schemeExprRefAbstractList ::
		(
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 [Symbol] -> SchemeExpression r obj (m a) ->
	 SchemeExpression r obj ([r obj] -> (m a));
	schemeExprRefAbstractList = exprAbstractListGuarded
	 throwTooFewArgumentsError
	 (\refs -> do
		{
		objs <- for get refs;
		throwTooManyArgumentsError objs;
		});

	schemeExprLet ::
		(
		InterpretObject m r obj
		) =>
	 Symbol -> ObjectSchemeExpression r obj m ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m a);
	schemeExprLet = exprLetMap mSubstMap;

	schemeExprLocLet ::
		(
		InterpretObject m r obj
		) =>
	 Symbol -> ObjectSchemeExpression r obj m ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m (a,r obj));
	schemeExprLocLet = exprLetMap mSubstLocMap;

	schemeExprLetSeparate ::
		(
		InterpretObject m r obj
		) =>
	 [(Symbol,ObjectSchemeExpression r obj m)] ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m a);
	schemeExprLetSeparate = exprLetMapSeparate separater where
		{
		separater :: (ExtractableFunctor t,InterpretObject m r obj) =>
		 t (m obj) -> (t (r obj) -> m a) -> m a;
		separater bindExprs absBody = do
			{
			objs <- fExtract bindExprs;
			locs <- fExtract (fmap new objs);
			absBody locs;
			};
		};

	schemeExprLetSequential ::
		(
		InterpretObject m r obj
		) =>
	 [(Symbol,ObjectSchemeExpression r obj m)] ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m a);
	schemeExprLetSequential = exprLetMapSequential mSubstMap;

	schemeExprLetRecursive ::
		(
		MonadFix m,
		InterpretObject m r obj
		) =>
	 [(Symbol,ObjectSchemeExpression r obj m)] ->
	 SchemeExpression r obj (m a) ->
	 SchemeExpression r obj (m a);
	schemeExprLetRecursive = exprLetMapRecursive valueFixer where
		{
--		fixFunctor :: (Functor t) => t (t a -> a) -> t a;
--		fixFunctor t = fix (\p -> (fmap (\x -> x p) t));

		mfixExFunctor :: (MonadFix m,ExtractableFunctor t) =>
		 t (t a -> m a) -> m (t a);
		mfixExFunctor t = mfix (\p -> for (\x -> x p) t);

		refFixer :: (MonadCreatable m r,MonadFix m,ExtractableFunctor t) =>
		 t (t (r v) -> m v) ->
		 (t (r v) -> m a) ->
		 m a;
		refFixer bindsT bodyT = (mfixExFunctor (fmap (\tlocmobj tloc -> do
			{
			obj <- tlocmobj tloc;
			new obj;
			}) bindsT)) >>= bodyT;

		valueFixer :: (MonadCreatable m r,MonadFix m,ExtractableFunctor t) =>
		 t (t (r v) -> m v) ->
		 (t (r v) -> m a) ->
		 m a;
		valueFixer bindsT bodyT = do
			{
			tval <- mfixExFunctor (fmap (\tlocmobj tval -> do
				{
				tloc <- for new tval;
				tlocmobj tloc;
				}) bindsT);
			tloc <- for new tval;
			bodyT tloc;
			};
		};
	}
