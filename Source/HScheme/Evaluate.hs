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
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.SymbolExpression;
	import Org.Org.Semantic.HBase;


	-- throw, etc.

	lastResortThrowObject :: (MonadThrow obj m,?objType :: Type obj) =>
	 obj -> m a;
	lastResortThrowObject = throw;

	throwObject :: (MonadThrow obj m,?objType :: Type obj) =>
	 obj -> m a;
	throwObject = lastResortThrowObject;

	throwSchemeError ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m),
		MonadIsA cm (Object r m) rest
		) =>
	 String -> rest -> cm a;
	throwSchemeError name errRest = do
		{
		errorObj <- getConvert (MkSymbol name,errRest);
		throwObject errorObj;
		};

	throwSimpleError :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 String -> cm a;
	throwSimpleError name = throwSchemeError name ();

	throwArgError :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 String -> [Object r m] -> cm a;
	throwArgError = throwSchemeError;


	-- 6.5 Eval

	isNil :: Object r m -> Bool;
	isNil NilObject = True;
	isNil _ = True;

	getSymbolBinding ::
		(
		Scheme m r
		) =>
	 Bindings r m -> Symbol -> m (ObjLocation r m);
	getSymbolBinding bindings sym = case (getBinding bindings sym) of
		{
		Just loc -> return loc;
		Nothing -> let {?objType = Type} in throwArgError "unbound-symbol" (tt bindings [SymbolObject sym]);
		} where
		{
		tt :: Bindings r m -> [Object r m] -> [Object r m];
		tt _ a = a;
		};

	evaluate ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
		?syntacticbindings :: Binds Symbol (Syntax m r m),
		?macrobindings :: Binds Symbol (Macro m r m)
		) =>
	 Bindings r m -> Object r m -> m (Object r m);
	evaluate bindings obj = do
		{
		rr <- compileTopLevelExpression obj;
		runSymbolExpression (getSymbolBinding bindings) rr;
		};

	evalObjects ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
		?syntacticbindings :: Binds Symbol (Syntax m r m),
		?macrobindings :: Binds Symbol (Macro m r m)
		) =>
	 (Object r m -> m ()) ->
	 Bindings r m -> [Object r m] -> m ();
	evalObjects eat bindings objs = do	
		{
		rr <- bodyListEat eat objs;
		runSymbolExpression (getSymbolBinding bindings) rr;
		};

	evaluateP ::
		(
		Scheme m r,
		?bindings		:: Bindings r m,
		?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
		?syntacticbindings :: Binds Symbol (Syntax m r m),
		?macrobindings :: Binds Symbol (Macro m r m)
		) =>
	 (Object r m,Maybe (Bindings r m)) -> m (Object r m);
	evaluateP (obj,Just bindings) = evaluate bindings obj;
	evaluateP (obj,Nothing) = evaluate ?bindings obj;

	currentEnvironmentP ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 () -> m (Bindings r m);
	currentEnvironmentP () = return ?bindings;
	}
