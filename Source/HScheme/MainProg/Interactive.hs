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

module Org.Org.Semantic.HScheme.MainProg.Interactive
	(interact,interactWithExit) where
	{
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	runTopLevelInEnvironment ::
		(
		FullScheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m)
		) =>
	 Environment r (Object r m) -> TopLevelCommand r m (m a) -> m (Environment r (Object r m),a);
	runTopLevelInEnvironment
	 (MkEnvironment synbindings runbindings) (MkTopLevelCommand expr newbinds newsyntaxes) = do
		{
		let {boundExpr = exprConstMapLet (getBinding runbindings) (interactiveBind newbinds expr)};
		(result,allnewbinds) <- runLambda (\_ -> error "undefined symbol") (schemeExprLetNewRefs (exprFreeSymbols boundExpr) boundExpr);
		return (MkEnvironment (newBindings synbindings newsyntaxes) (newBindings runbindings allnewbinds),result);
		};

	runObjectInEnvironment ::
		(
		FullScheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m)
		) =>
	 Environment r (Object r m) -> Object r m -> m (Environment r (Object r m),Object r m);
	runObjectInEnvironment env obj =  do
		{
		tlCommand <- let {?syntacticbindings = envSyn env} in assembleTopLevelObjectCommand obj;
		runTopLevelInEnvironment env tlCommand;
		};

	opWriteLnFlush :: (Monad m) =>
	 OutputPort Word8 m -> String -> m ();
	opWriteLnFlush port str = do
		{
		opWriteList port (encodeUTF8 (str ++ "\n"));
		opFlush port;
		};

	printResult ::
		(
		Build cm r,
		?objType :: Type (Object r m),
		?system :: System cm
		) =>
	 Object r m -> cm ();
	printResult obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteLnFlush (fsiCurrentOutputPort ?system) str;
		};

	reportError ::
		(
		Build cm r,
		?objType :: Type (Object r m),
		?system :: System cm
		) =>
	 Object r m -> cm ();
	reportError errObj = do
		{
		text <- toString errObj;
		opWriteLnFlush (fsiCurrentErrorPort ?system) ("error: "++text);
		};

	interactiveLoop ::
		(
		FullScheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m),
		?system :: System m
		) =>
	 Environment r (Object r m) -> m ();
	interactiveLoop environment = do
		{
		let
			{
			input = trapEOT (parseUTF8Char (ipRead (fsiCurrentInputPort ?system)))
			};
		menvironment' <- catch (catchBottom (do
			{
			opWriteList (fsiCurrentOutputPort ?system) (encodeUTF8 "hscheme> ");
			opFlush (fsiCurrentOutputPort ?system);
			mobject <- parseFromCharSource input;
			case mobject of
				{
				Nothing -> return Nothing;
				Just obj -> do
					{
					(environment',result) <- runObjectInEnvironment environment obj;
					printResult result;
					return (Just environment');
					};
				};
			}) (\ex -> do
				{
				runParser input restOfLineParse;
				errObj <- getObject (MkSymbol "failure",MkSList (show ex));
				reportError errObj;
				return (Just environment);
				})
			)
			(\errObj -> do
			{
			runParser input restOfLineParse;
			reportError errObj;
			return (Just environment);
			});

		case menvironment' of
			{
			Just environment' -> interactiveLoop environment';
			Nothing -> return ();
			};
		};

	envProc ::
		(
		FullScheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m)
		) =>
	 TopLevelCommand r m (m a) -> Environment r (Object r m) -> m (Environment r (Object r m));
	envProc command env = do
		{
		(env',_) <- runTopLevelInEnvironment env command;
		return env';
		};

	interact ::
		(
		FullScheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m),
		?system :: System m
		) =>
	 SymbolBindings (ObjLocation r m) ->
	 [TopLevelObjectCommand r m] ->
	 m ();
	interact bindings commands =
	 catch (do
		{
		env' <- concatenateList (fmap envProc commands) (MkEnvironment ?syntacticbindings bindings);
		interactiveLoop env';
		})
	 (\errObj -> do
		{
		reportError errObj;
		});

	interactWithExit ::
		(
		FullScheme m r,
		MonadCont m,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: Symbol -> Maybe (Macro m r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m),
		?system :: System m
		) =>
	 SymbolBindings (ObjLocation r m) ->
	 [TopLevelObjectCommand r m] ->
	 m ();
	interactWithExit rootBindings commands = callCC (\exitFunc -> do
		{
		bindings <- concatenateList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;
		env' <- catch
		 (concatenateList (fmap envProc commands) (MkEnvironment ?syntacticbindings bindings))
		 (\errObj -> do
			{
			reportError errObj;
			exitFunc ();
			});
		interactiveLoop env';
		});
	}
