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

module Org.Org.Semantic.HScheme.MacroLib.Macros where
	{
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	addM ::
	 (a -> x) ->
	 (b -> x) ->
	 (Either a b -> x);
	addM ax bx (Left a) = ax a;
	addM ax bx (Right b) = bx b;


	-- 4.1.2 Literal Expressions

	quoteM :: (Build cm r,Monad m,?objType :: Type (Object r m)) =>
	 (Object r m,()) ->
	 cm (ListSchemeExpression r m);
	quoteM (q,()) = return (return (return [q]));


	-- 4.1.4 Procedures

	lambdaMS ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 Object r m ->
	 [Object r m] ->
	 cm (SchemeExpression r m (Object r m));
	lambdaMS params bodyObjs = do
		{
		bodyExpr <- bodyM bodyObjs;
		procExpr <- makeLambda params bodyExpr;
		return (fmap ProcedureObject procExpr);
		};

	lambdaM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Object r m,[Object r m]) ->
	 cm (ListSchemeExpression r m);
	lambdaM (params,bodyObjs) = do
		{
		objExpr <- lambdaMS params bodyObjs;
		return (fmap (\obj -> return [obj]) objExpr);
		};


	-- 4.1.5 Conditionals

	ifM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Object r m,(Object r m,Maybe (Object r m))) ->
	 cm (ListSchemeExpression r m);
	ifM (condObj,(thenObj,mElseObj)) = do
		{
		condExpr <- assembleSingleExpression condObj;
		thenExpr <- assembleExpression thenObj;
		elseExpr <- case mElseObj of
			{
			Nothing -> return (return (return []));
			Just elseObj -> assembleExpression elseObj;
			};
		return (liftF3 (\mcond mthen melse -> do
			{
			condObject <- mcond;
			if (convert condObject)
			 then mthen
			 else melse;
			}) condExpr thenExpr elseExpr);
		};


	-- 4.1.6 Assignments

	setBangM ::
		(
		BuildThrow cm (Object r m) r,
		FullScheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (ListSchemeExpression r m);
	setBangM (sym,(obj,())) = do
		{
		expr <- assembleSingleExpression obj;
		return (liftF2 (\loc mval -> do
			{
			val <- mval;
			set loc val;
			return [];
			}) (exprSymbol sym) expr);
		};


	-- 4.2.2 Binding constructs

	assembleBinds ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 [(Symbol,(Object r m,()))] ->
	 cm [(Symbol,ObjectSchemeExpression r m)];
	assembleBinds [] = return [];
	assembleBinds ((sym,(obj,())):bs) = do
		{
		expr <- assembleSingleExpression obj;
		bcs <- assembleBinds bs;
		return ((sym,expr):bcs);
		};

	letSequentialM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ListSchemeExpression r m);
	letSequentialM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetSequential binds body);
		};

	letSeparateM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ListSchemeExpression r m);
	letSeparateM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetSeparate binds body);
		};

	letRecursiveM ::
		(
		MonadFix m,
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ListSchemeExpression r m);
	letRecursiveM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetRecursive binds body);
		};


	-- 4.2.3 Sequencing

	bodyM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ListSchemeExpression r m);
	bodyM = assembleTopLevelExpressions (return []) id (>>);

	bodyListM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ListSchemeExpression r m);
	bodyListM obj = fmap (fmap (\mlist -> do
		{
		list <- mlist;
		object <- getObject list;
		return [object];
		})) (assembleTopLevelExpressionsList obj);

	bodyValuesM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ListSchemeExpression r m);
	bodyValuesM = assembleTopLevelExpressionsList;


	-- 4.2.4 Iteration

	namedLetM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		MonadFix m,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Symbol,([(Symbol,(Object r m,()))],[Object r m])) ->
	 cm (ListSchemeExpression r m);
	namedLetM (var,(bindlist,bodyObj)) = do
		{
		bodyExpr <- bodyM bodyObj;
		bindvals <- fExtract (fmap (assembleSingleExpression . fst . snd) bindlist);
		return
		 (
		 let
			{
			exp = schemeExprAbstractList (fmap fst bindlist) bodyExpr;
			} in
		 schemeExprLetRecursive [(var,fmap (return . ProcedureObject) exp)] (liftF2 (\objsf mobjs -> do
			{
			objs <- fExtract mobjs;
			objsf objs;
			}) exp (fExtract bindvals))
		 );
		};


	-- 5.1 Programs

	beginT ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (TopLevelObjectCommand r m);
	beginT = begin (return []) id (>>);


	-- 5.2 Definitions

	pureDefine ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 Symbol -> ObjectSchemeExpression r m -> TopLevelObjectCommand r m;
	pureDefine sym valExpr = MkTopLevelCommand (return (return [])) [(sym,valExpr)] [];

	fullDefine ::
		(
		FullScheme m r,
		?objType :: Type (Object r m)
		) =>
	 Symbol -> ObjectSchemeExpression r m-> TopLevelObjectCommand r m;
	fullDefine sym valExpr = MkTopLevelCommand 
	 (liftF2 (\loc mval -> do
		{
		val <- mval;
		set loc val;
		return [];
		}) (exprSymbol sym) valExpr)
	 [(sym,return (return (error "unassigned symbol")))]
	 [];

	defineT ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Symbol -> ObjectSchemeExpression r m -> TopLevelObjectCommand r m) -> 
	 Either (Symbol,(Object r m,())) ((Symbol,Object r m),[Object r m]) -> cm (TopLevelObjectCommand r m);
	defineT define (Left (sym,(valObj,()))) = do
		{
		valExpr <- assembleSingleExpression valObj;
		return (define sym valExpr);
		};
	defineT define (Right ((sym,params),bodyObjs)) = do
		{
		valExpr <- lambdaMS params bodyObjs;
		return (define sym (fmap return valExpr));
		};
	}
