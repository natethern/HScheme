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

module Org.Org.Semantic.HScheme.Syntax where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	data Bind sym a = MkBind sym a;

	addBindings :: [Bind sym a] -> Binds sym a -> Binds sym a;
	addBindings [] binds = binds;
	addBindings ((MkBind sym a):binds) bindings =
	 addBindings binds (addBinding sym a bindings);

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
	substitute ((MkBind patvar sub):subs) (SymbolObject name) =
	 if name == patvar
	 then return sub
	 else substitute subs (SymbolObject name);
	substitute _ x = return x;

	matchBinding :: (Build cm r) =>
	 [Symbol] -> Object r m -> Object r m -> cm (Maybe [Binding r m]);
	matchBinding literals NilObject NilObject = return (Just []);
	matchBinding literals NilObject _ = return Nothing;
	matchBinding literals (PairObject phl ptl) (PairObject ahl atl) = do
		{
		ph <- get phl;
		ah <- get ahl;
		mhmatch <- matchBinding literals ph ah;
		case mhmatch of
			{
			Nothing -> return Nothing;
			Just hmatch -> do
				{
				pt <- get ptl;
				at <- get atl;
				mtmatch <- matchBinding literals pt at;
				case mtmatch of
					{
					Nothing -> return Nothing;
					Just tmatch -> return (Just (hmatch ++ tmatch));
					};
				};
			};
		};
	matchBinding literals (PairObject _ _) _ = return Nothing;
	matchBinding literals (SymbolObject name) args =
	 if hasElement name literals
	 then case args of
	 	{
	 	SymbolObject name' | name == name' -> return (Just []);
	 	_ -> return Nothing;
	 	}
	 else return (Just [MkBind name args]);
	matchBinding literals _ _ = return Nothing;
	
	matchBindings :: (Build cm r) =>
	 [Symbol] -> Object r m -> [Object r m] -> cm (Maybe [Binding r m]);
	matchBindings literals NilObject [] = return (Just []);
	matchBindings literals (PairObject phl ptl) (a1:as) = do
		{
		ph <- get phl;
		mmatch1 <- matchBinding literals ph a1;
		case mmatch1 of
			{
			Nothing -> return Nothing;
			Just match1 -> do
				{
				pt <- get ptl;
				mmatchs <- matchBindings literals pt as;
				case mmatchs of
					{
					Nothing -> return Nothing;
					Just matchs -> return (Just (match1 ++ matchs));
					};
				};
			};
		};
	matchBindings literals (SymbolObject name) args =
	 if hasElement name literals
	 then return Nothing
	 else do
		{
		argList <- getConvert args;
		return (Just [MkBind name argList]);
		};
	matchBindings literals _ _ = return Nothing;

	caseMatch :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 (Object r m,([Symbol],[(Object r m,(Object r m,()))])) -> cm ([Binding r m],Object r m);
	caseMatch (arg,(literals,[])) = throwArgError "no-match" [arg];
	caseMatch (arg,(literals,((pattern,(expr,())):rs))) = do
		{
		msubs <- matchBinding literals pattern arg;
		case msubs of
			{
			Nothing -> caseMatch (arg,(literals,rs));
			Just subs -> return (subs,expr);
			};
		};
{--
	caseMatchM :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,([Symbol],[(Object r m,(Object r m,()))])) -> m (Object r m);
	caseMatchM (argExpr,(literals,cases)) = do
		{
		arg <- evaluateObject argExpr;
		(subs,expr) <- caseMatch (arg,(literals,cases));
		let {?bindings = addBindings subs ?bindings;} in evaluateObject expr;
		};
--}
	syntaxRulesM :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 ([Symbol],[((Symbol,Object r m),(Object r m,()))]) -> cm (Syntax cm r m);
	syntaxRulesM (literals,rules) = return (\args -> let
		{
		transform [] = throwSimpleError "no-match";
		transform (((_,pattern),(template,())):rs) = do
			{
			msubs <- matchBindings literals pattern args;
			case msubs of
				{
				Nothing -> transform rs;
				Just subs -> substitute subs template;
				};
			};
		} in transform rules);


	compileSyntax ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m)
		) =>
	 Object r m -> cm (Syntax cm r m);
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
				margs <- getMaybeConvert t;
				case margs of
					{
					Nothing -> throwArgError "bad-syntax-rules-syntax" [t];
					Just args -> syntaxRulesM args;
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
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Symbol,(Object r m,())) -> cm (TopLevelObjectCommand cm r m);
	defineSyntaxT (sym,(obj,())) = do
		{
		syntax <- let {?objType = Type} in compileSyntax obj;
		return (MkTopLevelExpression (return' (return nullObject)) []
		 [(sym,syntax)]);
		};
	}
