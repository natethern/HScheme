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

	interpretInteractive ::
		(
		FullScheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Bindings Symbol (Macro m r m),
		?toplevelbindings :: Bindings Symbol (TopLevelMacro m r m)
		) =>
	 Environment r (Object r m) -> Object r m -> m (Environment r (Object r m),Object r m);
	interpretInteractive (MkEnvironment synbindings runbindings) obj = let
		{
		?syntacticbindings = synbindings;
		} in do
		{
		(MkTopLevelCommand expr newbinds newsyntaxes) <- assembleTopLevelObjectCommand obj;
		let {boundExpr = exprConstMapLet (getBinding runbindings) (interactiveBind newbinds expr)};
		(result,allnewbinds) <- runLambda (\_ -> error "undefined symbol") (schemeExprLetNewRefs (exprFreeSymbols boundExpr) boundExpr);
		return (MkEnvironment (newBindings synbindings newsyntaxes) (newBindings runbindings allnewbinds),result);
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
		?macrobindings :: SymbolBindings (Macro m r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro m r m),
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
					(environment',result) <- interpretInteractive environment obj;
					printResult result;
					return (Just environment');
					};
				};
			}) (\ex -> do
				{
				runParser input restOfLineParse;
				errObj <- getConvert (MkSymbol "failure",MkSList (show ex));
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

	interact ::
		(
		FullScheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro m r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro m r m),
		?system :: System m
		) =>
	 SymbolBindings (ObjLocation r m) ->
	 String -> m ();
	interact bindings filename =
	 catch (do
		{
--		bindings' <- psiLoadBindings (fsiPure ?system) bindings filename;
		interactiveLoop (MkEnvironment ?syntacticbindings bindings);
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
		?macrobindings :: SymbolBindings (Macro m r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro m r m),
		?system :: System m
		) =>
	 SymbolBindings (ObjLocation r m) ->
	 String ->
	 m ();
	interactWithExit rootBindings filename = callCC (\exitFunc -> do
		{
		bindings <- concatenateList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;
		bindings' <-
		 catch (
		 return bindings
--		 psiLoadBindings (fsiPure ?system) bindings filename
		 )
		 (\errObj -> do
			{
			reportError errObj;
			exitFunc ();
			});
		interactiveLoop (MkEnvironment ?syntacticbindings bindings');
		});
	}
