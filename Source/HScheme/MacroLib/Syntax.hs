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
	defineSyntaxT,caseMatchM
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

--	data Bind sym a = MkBind sym a;
	type Bind = (,);

	type Binding r m = Bind Symbol (Object r m);

	substitute :: (Build cm r) =>
	 [Binding r m] -> Object r m -> cm (Object r m);
	substitute [] x = return x;
	substitute subs (PairObject hloc tloc) = do
		{
		head <- get hloc;
		head' <- substitute subs head;
		hloc' <- new head';
		tail <- get tloc;
		tail' <- substitute subs tail;
		tloc' <- new tail';
		return (PairObject hloc' tloc')
		};
	substitute subs (VectorObject arr) = do
		{
		list' <- subList (toList arr);
		return (VectorObject (fromList list'));
		} where
		{
		subList [] = return [];
		subList (loc:locs) = do
			{
			head <- get loc;
			head' <- substitute subs head;
			loc' <- new head';
			locs' <- subList locs;
			return (loc':locs');
			};
		};
	substitute (((,) patvar sub):subs) (SymbolObject name) =
	 if name == patvar
	 then return sub
	 else substitute subs (SymbolObject name);
	substitute _ x = return x;

	caseMatchM ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Object r m,([Symbol],[(Object r m,(Object r m,()))])) ->
	 cm (ListSchemeExpression r m);
	caseMatchM (argExprObj,(literals,rules)) = do
		{
		argExpr <- assembleSingleExpression argExprObj;
		sExprs <- fExtract (fmap (\(patternObj,(bodyObj,())) -> do
			{
			pattern <- makeObjectPattern literals patternObj;
			bodyExpr <- assembleExpression bodyObj;
			return (patternBind pattern bodyExpr);
			}) rules);
		return (liftF2 (\marg pcases -> do
			{
			arg <- marg;
			matchCases arg pcases;
			}) argExpr (fExtract sExprs));
		} where
		{
		matchCases ::
			(
			BuildThrow cm (Object r m) r,
			?objType :: Type (Object r m)
			) =>
		 Object r m ->
		 [Object r m -> cm (Result ex (cm a))] ->
		 cm a;		
		matchCases arg [] = throwArgError "no-match" [arg];
		matchCases arg (pcase:rest) = do
			{
			nmobj <- pcase arg;
			case nmobj of
				{
				SuccessResult mobj -> mobj;
				_ -> matchCases arg rest;
				};
			};
		};

	syntaxRulesM ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 ([Symbol],[((Symbol,Object r m),(Object r m,()))]) -> cm (Syntax r (Object r m));
	syntaxRulesM (literals,rules) = return (MkSyntax (transform rules)) where
		{
		transform ::
			(
			BuildThrow cm (Object r m) r,
			?objType :: Type (Object r m)
			) =>
		 [((Symbol,Object r m),(Object r m,()))] ->
		 Type (r ()) -> 
		 [Object r m] ->
		 cm (Object r m);
		transform [] _ _ = throwSimpleError "no-match";
		transform (((_,patternObj),(template,_)):rs) t args = do
			{
			pattern <- makeListPattern literals patternObj;
			nlist <- patternZip pattern args;
			case nlist of
				{
				SuccessResult subs -> substitute subs template;
				_ -> transform rs t args;
				}
			};
		};

	compileSyntax ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m))
		) =>
	 Object r m -> cm (Syntax r (Object r m));
	compileSyntax (SymbolObject sym) = case getBinding ?syntacticbindings sym of
		{
		Just syntax -> return syntax;
		Nothing -> throwArgError "undefined-syntax" [SymbolObject sym];
		};
	compileSyntax obj@(PairObject head tail) = do
		{
		h <- get head;
		case h of
			{
			SymbolObject (MkSymbol "syntax-rules") -> do
				{
				t <- get tail;
				margs <- fromObject t;
				case margs of
					{
					ExceptionResult mm -> do
						{
						mmObj <- getConvert mm;
						throwArgError "bad-syntax-rules-syntax" [t,mmObj];
						};
					SuccessResult args -> syntaxRulesM args;
					};
				};
			SymbolObject _ -> throwArgError "undefined-syntax-maker" [h];
			_ -> throwArgError "form-not-syntax" [obj];
			};
		};
	compileSyntax obj = throwArgError "form-not-syntax" [obj];

	defineSyntaxT ::
		(
		BuildThrow cm (Object r m) r,
		Monad m,
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelListCommand r m);
	defineSyntaxT (sym,(obj,())) = do
		{
		syntax <- let {?objType = MkType} in compileSyntax obj;
		return (MkTopLevelCommand (return (return [])) []
		 [(sym,syntax)]);
		};
	}
