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
	 cm (ObjectSchemeExpression r m);
	quoteM (q,()) = return (return (return q));


	-- 4.1.4 Procedures

	lambdaM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m,[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
	lambdaM (params,bodyObj) = do
		{
		body <- bodyM bodyObj;
		proc <- makeLambda params body;
		return (fmap (return . ProcedureObject) proc);
		};


	-- 4.1.5 Conditionals

	ifM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m,(Object r m,Maybe (Object r m))) ->
	 cm (ObjectSchemeExpression r m);
	ifM (condObj,(thenObj,mElseObj)) = do
		{
		condExpr <- assembleExpression condObj;
		thenExpr <- assembleExpression thenObj;
		elseExpr <- case mElseObj of
			{
			Nothing -> return (return (return nullObject));
			Just elseObj -> assembleExpression elseObj;
			};
		return (liftF3 (\mcond mthen melse -> do
			{
			condObject <- mcond;
			cond <- getConvert condObject;
			if cond
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
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (ObjectSchemeExpression r m);
	setBangM (sym,(obj,())) = do
		{
		expr <- assembleExpression obj;
		return (liftF2 (\loc mval -> do
			{
			val <- mval;
			set loc val;
			return nullObject;
			}) (exprSymbol sym) expr);
		};


	-- 4.2.2 Binding constructs

	assembleBinds ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [(Symbol,(Object r m,()))] ->
	 cm [(Symbol,ObjectSchemeExpression r m)];
	assembleBinds [] = return [];
	assembleBinds ((sym,(obj,())):bs) = do
		{
		expr <- assembleExpression obj;
		bcs <- assembleBinds bs;
		return ((sym,expr):bcs);
		};

	letSequentialM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
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
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
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
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) ->
	 cm (ObjectSchemeExpression r m);
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
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ObjectSchemeExpression r m);
	bodyM = assembleTopLevelExpressions (return nullObject) id (>>);

	bodyListM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (ObjectSchemeExpression r m);
	bodyListM obj = fmap' (fmap (\mlist -> mlist >>= getConvert)) (assembleTopLevelExpressionsList obj) where
		{
		fmap' :: (Monad f) => (a -> b) -> (f a -> f b);
		fmap' map fa = fa >>= (return . map);
		};


	-- 4.2.4 Iteration

	namedLetM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		MonadFix m,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,([(Symbol,(Object r m,()))],[Object r m])) ->
	 cm (ObjectSchemeExpression r m);
	namedLetM (var,(bindlist,bodyObj)) = do
		{
		bodyExpr <- bodyM bodyObj;
		bindvals <- fExtract (fmap (assembleExpression . fst . snd) bindlist);
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
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (TopLevelObjectCommand r m);
	beginT = begin (return nullObject) id (>>);


	-- 5.2 Definitions

	pureDefineT ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand r m);
	pureDefineT (sym,(val,())) = do
		{
		valExpr <- assembleExpression val;
		return (MkTopLevelCommand (return (return nullObject)) [(sym,valExpr)] []);
		};

	fullDefineT ::
		(
		BuildThrow cm (Object r m) r,
		FullScheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand r m);
	fullDefineT (sym,(valObj,())) = do
		{
		valExpr <- assembleExpression valObj;
		return (MkTopLevelCommand 
		 (liftF2 (\loc mval -> do
		 	{
		 	val <- mval;
		 	set loc val;
		 	return nullObject;
		 	}) (exprSymbol sym) valExpr)
		 [(sym,return (return (error "unassigned symbol")))] []);
		};
	}
