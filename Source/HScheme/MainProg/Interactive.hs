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
	import Org.Org.Semantic.HScheme.LambdaCalculus;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	runTopLevelInEnvironment ::
		(
		FullBuild m r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m)
		) =>
	 Environment r obj m -> TopLevelCommand r obj m (m a) -> m (Environment r obj m,a);
	runTopLevelInEnvironment
	 (MkEnvironment synbindings runbindings) (MkTopLevelCommand expr newbinds newsyntaxes) = do
		{
		let {boundExpr = exprConstMapLet (getBinding runbindings) (interactiveBind newbinds expr)};
		(result,allnewbinds) <- runLambdaExpression (\_ -> error "undefined symbol") (schemeExprLetNewRefs (exprFreeSymbols boundExpr) boundExpr);
		return (MkEnvironment (newBindings synbindings newsyntaxes) (newBindings runbindings allnewbinds),result);
		};

	runObjectInEnvironment ::
		(
		FullBuild m r,
		AssembleError m obj,
		InterpretObject m r obj,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m)
		) =>
	 Environment r obj m -> obj -> m (Environment r obj m,[obj]);
	runObjectInEnvironment env obj =  do
		{
		tlCommand <- let {?syntacticbindings = envSyn env} in assembleTopLevelListCommand obj;
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
		ToString cm obj,
		?objType :: Type obj,
		?system :: System cm
		) =>
	 [obj] -> cm ();
	printResult [] = return ();
	printResult (obj:objs) =  do
		{
		str <- toString obj;
		opWriteLnFlush (fsiCurrentOutputPort ?system) str;
		printResult objs;
		};

	reportError ::
		(
		ToString cm obj,
		?objType :: Type obj,
		?system :: System cm
		) =>
	 obj -> cm ();
	reportError errObj = do
		{
		text <- toString errObj;
		opWriteLnFlush (fsiCurrentErrorPort ?system) ("error: "++text);
		};

	interactiveLoop ::
		(
		ToString m obj,
		AssembleError m obj,
		InterpretObject m r obj,
		ParserError m obj,
		ParseObject r obj,
		FullBuild m r,
		MonadBottom m,
		MonadException obj m,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m),
		?system :: System m
		) =>
	 Environment r obj m -> m ();
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
				parseRestOfLine input;
				errObj <- getObject (MkSymbol "failure",MkSList (show ex));
				reportError errObj;
				return (Just environment);
				})
			)
			(\errObj -> do
			{
			parseRestOfLine input;
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
		InterpretObject m r obj,
		FullBuild m r,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m)
		) =>
	 TopLevelCommand r obj m (m a) -> Environment r obj m -> m (Environment r obj m);
	envProc command env = do
		{
		(env',_) <- runTopLevelInEnvironment env command;
		return env';
		};

	interact ::
		(
		AssembleError m obj,
		ParserError m obj,
		ParseObject r obj,
		ToString m obj,
		InterpretObject m r obj,
		FullBuild m r,
		MonadBottom m,
		MonadException obj m,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m),
		?system :: System m
		) =>
	 SymbolBindings (r obj) ->
	 [TopLevelListCommand r obj m] ->
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
		AssembleError m obj,
		ParserError m obj,
		ParseObject r obj,
		ToString m obj,
		InterpretObject m r obj,
		FullBuild m r,
		MonadCont m,
		MonadBottom m,
		MonadException obj m,
		?objType :: Type obj,
		?macrobindings :: Symbol -> Maybe (Macro m r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m),
		?system :: System m
		) =>
	 SymbolBindings (r obj) ->
	 [TopLevelListCommand r obj m] ->
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
