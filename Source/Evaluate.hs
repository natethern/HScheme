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

module Evaluate where
	{
	import Object;
	import Type;
	
	isNil :: Object r m -> Bool;
	isNil NilObject = True;
	isNil _ = True;

	evaluate ::
		(
		Scheme x m r,
		?bindings :: Bindings r m
		) =>
	 Object r m -> m (Object r m);

	evaluate (SymbolObject a) = case (getBinding ?bindings a) of
		{
		Just loc -> getLocation loc;
		Nothing -> fail (a++" not defined");
		};

	evaluate (PairObject head tail) = do
		{
		h <- getLocation head;
		t <- getLocation tail;
		f <- evaluate h;
		applyEval f t;
		};

	evaluate a = case a of
		{
		BooleanObject _ -> return a;
		NumberObject _ -> return a;
		CharObject _ -> return a;
		StringObject _ -> return a;
		VectorObject _ -> return a;
		_ -> fail "unrecognised expression form";
		};
	
	evaluateS ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> (Object r m,Maybe (Bindings r m)) -> m (Object r m);
	evaluateS Type (obj,Just bindings) = evaluate obj with {?bindings = bindings};
	evaluateS Type (obj,Nothing) = evaluate obj;
	
	evalList ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> m [Object r m];
	evalList NilObject = return [];
	evalList (PairObject al dl) = do
		{
		a <- getLocation al;
		ae <- evaluate a;
		d <- getLocation dl;
		de <- evalList d;
		return (ae:de);
		};
	evalList _ = fail "procedure args not a list";
	
	macroToList ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> m [Object r m];
	macroToList NilObject = return [];
	macroToList (PairObject al dl) = do
		{
		a <- getLocation al;
		d <- getLocation dl;
		de <- macroToList d;
		return (a:de);
		};
	macroToList _ = fail "macro args not a list";
	
	applyEval ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Object r m -> Object r m -> m (Object r m);
	applyEval (ProcedureObject f) arglist = do
		{
		args <- evalList arglist;
		f ?bindings args;
		};
	applyEval (SyntaxObject f) arglist = do
		{
		args <- macroToList arglist;
		res <- f args;
		evaluate res;
		};
	applyEval (MacroObject f) arglist = do
		{
		args <- macroToList arglist;
		f args;
		};
	applyEval _ _ = fail "wrong type to apply";
{--
	defineEvaluate ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> Object r m -> m (Bindings r m,Object r m);
	

	defineEvaluate (PairObject head tail) = do
		{
		h <- getLocation head;
		t <- getLocation tail;
		f <- evaluate h;
		applyEval f t;
		};
	
	defineEvaluate bindings a = do
		{
		case a of
			{
			(PairObject head tail) -> do
				{
				h <- getLocation head;
				case h of
					{
					SymbolObject "define" ->
				t <- getLocation tail;
				f <- evaluate h;
				applyEval f t;
				};
			_ -> do
				{
				r <- evaluate a with {?bindings = bindings};
				return (bindings,r);
				};
			};
		};
--}
	}
