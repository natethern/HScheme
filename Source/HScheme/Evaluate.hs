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

module Org.Org.Semantic.HScheme.Evaluate where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;


	-- 6.5 Eval

	isNil :: Object r m -> Bool;
	isNil NilObject = True;
	isNil _ = True;

	getSymbolBinding ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 Symbol -> m (ObjLocation r m);
	getSymbolBinding sym = case (getBinding ?bindings sym) of
		{
		Just loc -> return loc;
		Nothing -> throwArgError "unbound-symbol" (tt ?bindings [SymbolObject sym]);
		} where
		{
		tt :: Bindings r m -> [Object r m] -> [Object r m];
		tt _ a = a;
		};

	evaluate ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 Object r m -> m (Object r m);

	evaluate (SymbolObject sym) = do
		{
		loc <- getSymbolBinding sym;
		get loc;
		};

	evaluate (PairObject head tail) = do
		{
		h <- get head;
		t <- get tail;
		f <- evaluate h;
		applyEval f t;
		};

	evaluate a = case a of
		{
		BooleanObject _ -> return a;
		NumberObject _ -> return a;
		CharObject _ -> return a;
		StringObject _ -> return a;
		ByteArrayObject _ -> return a;
		VectorObject _ -> return a;
		_ -> throwArgError "cant-evaluate-form" [a];
		};

	evaluateP ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 (Object r m,Maybe (Bindings r m)) -> m (Object r m);
	evaluateP (obj,Just bindings) = let  {?bindings = bindings} in evaluate obj;
	evaluateP (obj,Nothing) = evaluate obj;

	evalList ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> m [Object r m];
	evalList NilObject = return [];
	evalList (PairObject al dl) = do
		{
		a <- get al;
		ae <- evaluate a;
		d <- get dl;
		de <- evalList d;
		return (ae:de);
		};
	evalList obj = throwArgError "bad-procedure-call-form" [obj];

	macroToList ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> m [Object r m];
	macroToList NilObject = return [];
	macroToList (PairObject al dl) = do
		{
		a <- get al;
		d <- get dl;
		de <- macroToList d;
		return (a:de);
		};
	macroToList obj = throwArgError "bad-macro-call-form" [obj];

	applyEval ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> Object r m -> m (Object r m);
	applyEval (SyntaxObject f) arglist = do
		{
		args <- macroToList arglist;
		res <- f args;
		evaluate res;
		};
	applyEval (ProcedureObject f) arglist = do
		{
		result <- evalList arglist;
		f ?bindings result;
		};
	applyEval (MacroObject f) arglist = do
		{
--		args <- macroToList arglist;
		f ?bindings arglist;
		};
	applyEval obj _ = throwArgError "bad-apply-form" [obj];

	currentEnvironmentP ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 () -> m (Bindings r m);
	currentEnvironmentP () = return ?bindings;
	}
