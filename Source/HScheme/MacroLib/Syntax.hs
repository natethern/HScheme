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

module Org.Org.Semantic.HScheme.MacroLib.Syntax
	(
	appendTwo,
	MapObjects(..),SyntaxError(..),
	defineSyntaxT,caseMatchM
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

--	data Bind sym a = MkBind sym a;
	type Bind = (,);

	type Binding = Bind Symbol;

	appendTwo ::
		(
		ProcedureError cm obj,
		Build cm r,
		ObjectSubtype r obj obj,
		?objType :: Type obj
		) =>
	 obj -> obj -> cm obj;
	appendTwo a b = do
		{
		epn <- fromObject a;
		case epn of
			{
			Left () -> return b;
			Right (head,tail) -> do
				{
				tb <- appendTwo tail b;
				cons head tb;
				};
			};
		};

	class MapObjects r obj | obj -> r where
		{
		internalMapWithEllipsis :: forall cm. (Build cm r,ProcedureError cm obj,?objType :: Type obj) =>
		 (obj -> cm (Maybe obj)) -> obj -> cm obj;
		};

	substituteSymbol ::
		(
		ObjectSubtype r obj Symbol,
		ProcedureError cm obj,
		Build cm r,
		?objType :: Type obj
		) =>
	 [Binding obj] -> Symbol -> cm obj;
	substituteSymbol [] sym = getObject sym;
	substituteSymbol (((,) patvar sub):subs) sym =
	 if sym == patvar
	 then return sub
	 else substituteSymbol subs sym;

	substitute ::
		(
		ProcedureError cm obj,
		Build cm r,
		MapObjects r obj,
		ObjectSubtype r obj Symbol,
		?objType :: Type obj
		) =>
	 [Binding obj] -> obj -> cm obj;
	substitute [] = return;
	substitute subs = internalMapWithEllipsis (\obj -> do
		{
		msym <- resultFromObject obj;
		case msym of
			{
			SuccessResult sym -> fmap Just (substituteSymbol subs sym);
			_ -> return Nothing;
			};
		});

	tryEach :: (Monad m) =>
	 m a -> [m (Result ex (m a))] -> m a;		
	tryEach none [] = none;
	tryEach none (action:rest) = do
		{
		rma <- action;
		case rma of
			{
			SuccessResult ma -> ma;
			_ -> tryEach none rest;
			};
		};

	caseMatchM ::
		(
		Build cm r,
		AssembleError cm obj,
		InterpretObject m r obj,
		PatternError m obj,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (obj,([Symbol],[(obj,(obj,()))])) ->
	 cm (ListSchemeExpression r obj m);
	caseMatchM (argExprObj,(literals,rules)) = do
		{
		argExpr <- assembleSingleExpression argExprObj;
		sExprs <- for (\(patternObj,(bodyObj,())) -> do
			{
			pattern <- makeObjectPattern literals patternObj;
			bodyExpr <- assembleExpression bodyObj;
			return (patternBind pattern bodyExpr);
			}) rules;
		return (liftF2 (\marg pcases -> do
			{
			arg <- marg;
			tryEach 
			 (throwPatternNotMatchedError [arg])
			 (fmap (\pcase -> pcase arg) pcases);
			}) argExpr (fExtract sExprs));
		};

	syntaxRulesM ::
		(
		MapObjects r obj,
		Build cm r,
		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		?objType :: Type obj
		) =>
	 ([Symbol],[((Symbol,obj),(obj,()))]) -> cm (Syntax r obj);
	syntaxRulesM (literals,rules) = return (MkSyntax (transform rules)) where
		{
		transform ::
			(
			MapObjects r obj,
			Build cm r,
			PatternError cm obj, 
			ObjectSubtype r obj obj,
			ObjectSubtype r obj Symbol,
			?objType :: Type obj
			) =>
		 [((Symbol,obj),(obj,()))] ->
		 Type (r ()) -> 
		 [obj] ->
		 cm obj;
		transform rules t args = tryEach
		 (throwPatternNotMatchedError args)
		 (fmap (\((_,patternObj),(template,_)) -> do
			{
			pattern <- makeListPattern literals patternObj;
			nlist <- patternZip pattern args;
			return (fmap (\subs -> substitute subs template) nlist);
			}) rules)
		};

	class (ProcedureError cm obj) =>
	 SyntaxError cm obj where
		{
		throwUndefinedSyntaxError :: forall a. (?objType :: Type obj) =>
		 Symbol -> cm a;
		throwUnknownSyntaxConstructorError :: forall a. (?objType :: Type obj) =>
		 Symbol -> cm a;
		};

	compileSyntax ::
		(
		MapObjects r obj,
		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		SyntaxError cm obj,
		Build cm r,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj)
		) =>
	 obj -> cm (Syntax r obj);
	compileSyntax obj = do
		{
		choice <- fromObject obj;
		case choice of
			{
			Left sym -> case getBinding ?syntacticbindings sym of
				{
				Just syntax -> return syntax;
				Nothing -> throwUndefinedSyntaxError sym;
				};
			Right (hsym,tail) -> case hsym of
				{
				MkSymbol "syntax-rules" -> do
					{
					args <- fromObject tail;
					syntaxRulesM args;
					};
				_ -> throwUnknownSyntaxConstructorError hsym;
				};
			};
		};

	defineSyntaxT ::
		(
		ObjectSubtype r obj Symbol,
		ObjectSubtype r obj obj,
		MapObjects r obj,
		SyntaxError cm obj,
		Build cm r,
		Monad m,
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (Symbol,(obj,())) -> cm (TopLevelListCommand r obj m);
	defineSyntaxT (sym,(obj,())) = do
		{
		syntax <- let {?objType = MkType} in compileSyntax obj;
		return (MkTopLevelCommand (return (return [])) []
		 [(sym,syntax)]);
		};
	}
