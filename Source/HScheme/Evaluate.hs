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

	isNil :: Object r m -> Bool;
	isNil NilObject = True;
	isNil _ = False;

	getLoc ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 (Symbol -> Maybe (ObjLocation r m)) -> Symbol -> m (ObjLocation r m);
	getLoc getter sym = case (getter sym) of
		{
		Just loc -> return loc;
		Nothing -> throwArgError "unbound-symbol" ([SymbolObject sym]);
		};
{--
	getSymbolBinding ::
		(
		Scheme m r
		) =>
	 Bindings r m -> Symbol -> m (ObjLocation r m);
	getSymbolBinding bindings sym = getLoc (getBinding bindings);
--}
	interpretTopLevelExpression ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m (Object r m));
	interpretTopLevelExpression obj = let {?objType = Type} in do
		{
		rr <- assembleTopLevelExpression obj;
		return (\lookup -> runSymbolExpression (getLoc lookup) rr);
		};

	interpretTopLevelExpressionsEat ::
		(
		Build cm r,
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 [Object r m] -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m ());
	interpretTopLevelExpressionsEat eat objs = let {?objType = Type} in do	
		{
		rr <- assembleTopLevelExpressionsEat eat objs;
		return (\lookup -> runSymbolExpression (getLoc lookup) rr);
		};


	-- 6.5 Eval

	evaluateObject ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
		?syntacticbindings :: Binds Symbol (Syntax m r m),
		?macrobindings :: Binds Symbol (Macro m r m)
		) =>
	 Object r m -> (Symbol -> Maybe (ObjLocation r m)) -> m (Object r m);
	evaluateObject obj lookup = do
		{
		mobj <- interpretTopLevelExpression obj;
		mobj lookup;
		};

	evaluateP ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
		?syntacticbindings :: Binds Symbol (Syntax m r m),
		?macrobindings :: Binds Symbol (Macro m r m)
		) =>
	 (Object r m,(Bindings r m,())) -> m (Object r m);
	evaluateP (obj,(bindings,())) = evaluateObject obj (getBinding bindings);

	currentEnvironmentP ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 () -> m (Bindings r m);
	currentEnvironmentP () = return ?bindings;
	}
