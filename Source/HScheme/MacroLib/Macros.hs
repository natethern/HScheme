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

	quoteM :: (Build cm r,Monad m,?objType :: Type obj) =>
	 (obj,()) ->
	 cm (ListSchemeExpression r obj m);
	quoteM (q,()) = return (return (return [q]));


	-- 4.1.4 Procedures

	lambdaMS ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj ->
	 [obj] ->
	 cm (ObjectSchemeExpression r obj m);
	lambdaMS params bodyObjs = do
		{
		bodyExpr <- bodyM bodyObjs;
		procExpr <- makeLambda params bodyExpr;
		return (fmap getObject procExpr);
		};

	lambdaM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (obj,[obj]) ->
	 cm (ListSchemeExpression r obj m);
	lambdaM (params,bodyObjs) = do
		{
		objExpr <- lambdaMS params bodyObjs;
		return (fmap (fmap (\obj -> [obj])) objExpr);
		};


	-- 4.1.5 Conditionals

	ifM ::
		(
		AssembleError cm obj,
		IsA Bool obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (obj,(obj,Maybe obj)) ->
	 cm (ListSchemeExpression r obj m);
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
		AssembleError cm obj,
		Build cm r,
		FullBuild m r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (Symbol,(obj,())) -> cm (ListSchemeExpression r obj m);
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
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 [(Symbol,(obj,()))] ->
	 cm [(Symbol,ObjectSchemeExpression r obj m)];
	assembleBinds [] = return [];
	assembleBinds ((sym,(obj,())):bs) = do
		{
		expr <- assembleSingleExpression obj;
		bcs <- assembleBinds bs;
		return ((sym,expr):bcs);
		};

	letSequentialM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 ([(Symbol,(obj,()))],[obj]) ->
	 cm (ListSchemeExpression r obj m);
	letSequentialM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetSequential binds body);
		};

	letSeparateM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 ([(Symbol,(obj,()))],[obj]) ->
	 cm (ListSchemeExpression r obj m);
	letSeparateM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetSeparate binds body);
		};

	letRecursiveM ::
		(
		AssembleError cm obj,
		MonadFix m,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 ([(Symbol,(obj,()))],[obj]) ->
	 cm (ListSchemeExpression r obj m);
	letRecursiveM (bindList,bodyObj) = do
		{
		binds <- assembleBinds bindList;
		body <- bodyM bodyObj;
		return (schemeExprLetRecursive binds body);
		};


	-- 4.2.3 Sequencing

	bodyM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 [obj] ->
	 cm (ListSchemeExpression r obj m);
	bodyM = assembleTopLevelExpressions (return []) id (>>) nothing;

	bodyListM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 [obj] ->
	 cm (ListSchemeExpression r obj m);
	bodyListM objs = fmap (fmap (\mlist -> do
		{
		list <- mlist;
		object <- getObject list;
		return [object];
		})) (assembleTopLevelExpressionsList nothing objs);

	bodyValuesM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 [obj] ->
	 cm (ListSchemeExpression r obj m);
	bodyValuesM = assembleTopLevelExpressionsList nothing;


	-- 4.2.4 Iteration

	namedLetM ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		MonadFix m,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (Symbol,([(Symbol,(obj,()))],[obj])) ->
	 cm (ListSchemeExpression r obj m);
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
		 schemeExprLetRecursive [(var,fmap getObject exp)] (liftF2 (\objsf mobjs -> do
			{
			objs <- fExtract mobjs;
			objsf objs;
			}) exp (fExtract bindvals))
		 );
		};


	-- 5.1 Programs

	beginT ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 [obj] ->
	 cm (TopLevelListCommand r obj m);
	beginT = begin (return []) id (>>);


	-- 5.2 Definitions

	pureDefine ::
		(
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 Symbol -> ObjectSchemeExpression r obj m -> TopLevelListCommand r obj m;
	pureDefine sym valExpr = MkTopLevelCommand (return (return [])) [(sym,valExpr)] [];

	fullDefine ::
		(
		FullBuild m r,
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 Symbol -> ObjectSchemeExpression r obj m-> TopLevelListCommand r obj m;
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
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (Symbol -> ObjectSchemeExpression r obj m -> TopLevelListCommand r obj m) -> 
	 Either (Symbol,(obj,())) ((Symbol,obj),[obj]) -> cm (TopLevelListCommand r obj m);
	defineT define (Left (sym,(valObj,()))) = do
		{
		valExpr <- assembleSingleExpression valObj;
		return (define sym valExpr);
		};
	defineT define (Right ((sym,params),bodyObjs)) = do
		{
		valExpr <- lambdaMS params bodyObjs;
		return (define sym valExpr);
		};
	}
