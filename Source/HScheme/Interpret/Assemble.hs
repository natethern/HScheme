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

module Org.Org.Semantic.HScheme.Interpret.Assemble
	(
	SchemeExpression,ObjectSchemeExpression,
	Macro(..),
	assembleExpression
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.SymbolExpression;
	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	type SchemeExpression r m = SymbolExpression Symbol (m (ObjLocation r m));

	type ObjectSchemeExpression r m = SchemeExpression r m (m (Object r m));

	newtype Macro cm r m = MkMacro
		((
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] -> cm (ObjectSchemeExpression r m));

	-- as fProductList
	execList :: (Monad m) =>
	 [m a] -> m [a];
	execList [] = return [];
	execList (ma:mas) = do
		{
		a <- ma;
		as <- execList mas;
		return (a:as);
		};

	doApply ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 m (Object r m) ->
	 m [Object r m] ->
	 m (Object r m);
	doApply mf margs = do
		{
		f <- mf;
		case f of
			{
			ProcedureObject proc -> do
				{
				args <- margs;
				proc args;
				};
			_ -> throwArgError "bad-apply-form" [f];
			};
		};

	makeApply ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 ObjectSchemeExpression r m ->
	 [ObjectSchemeExpression r m] ->
	 ObjectSchemeExpression r m;
	makeApply f args = fApply (fmap doApply f) (fmap execList (fExtract args));

	assembleApplyExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> [Object r m] -> cm (ObjectSchemeExpression r m);
	assembleApplyExpression f arglist = do
		{
		fe <- assembleExpression f;
		ae <- sinkList assembleExpression arglist;
		return (makeApply fe ae);
		};

	assembleExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm (ObjectSchemeExpression r m);
	assembleExpression (SymbolObject sym) = return (fmap (\mloc -> do
		{
		loc <- mloc;
		get loc;
		}) (fSymbol sym));
	assembleExpression (PairObject head tail) = do
		{
		h <- get head;
		t <- get tail;
		marglist <- getMaybeConvert t;
		case marglist of
			{
			Nothing -> throwArgError "bad-argument-list" [t];
			Just arglist -> case h of
				{
				SymbolObject sym -> case getBinding ?syntacticbindings sym of
					{
					Just (MkSyntax syntax) -> do
						{
						obj <- syntax Type arglist;
						assembleExpression obj;
						};
					Nothing -> case getBinding ?macrobindings sym of
						{
						Just (MkMacro macro) -> macro arglist;
						Nothing -> assembleApplyExpression h arglist;
						};
					};
				_ -> assembleApplyExpression h arglist;
				};
			};
		};
	assembleExpression a = case a of
		{
		BooleanObject _ -> return (return' (return a));
		NumberObject _ -> return (return' (return a));
		CharObject _ -> return (return' (return a));
		StringObject _ -> return (return' (return a));
		ByteArrayObject _ -> return (return' (return a));
		_ -> throwArgError "cant-evaluate-form" [a];
		};
	}
