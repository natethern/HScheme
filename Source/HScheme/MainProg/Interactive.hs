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

module Org.Org.Semantic.HScheme.MainProg.Interactive where
	{
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	reportError :: (Scheme m r,?objType :: Type (Object r m)) =>
	 OutputPort Word8 m -> Object r m -> m ();
	reportError errPort errObj = do
		{
		text <- toString errObj;
		opWriteList errPort (encodeUTF8 ("error: "++text++"\n"));
		opFlush errPort;
		};

	interactiveLoop ::
		(
		Scheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?system :: System cm
		) =>
	  SymbolBindings (ObjLocation r m) -> m ();
	interactiveLoop bindings = do
		{
{--
		let
			{
			input = let {?bindings=bindings} in
			 trapEOT (parseUTF8Char (ipRead (fsiCurrentInputPort ?system)))
			};
		mbindings' <- catch (catchBottom (do
			{
			opWriteList (fsiCurrentOutputPort ?system) (encodeUTF8 "hscheme> ");
			opFlush (fsiCurrentOutputPort ?system);
			mobject <- parseFromCharSource input;
			case mobject of
				{
				Nothing -> return Nothing;
				Just obj -> do
					{
					bindings' <- printeval (fsiCurrentOutputPort ?system) bindings obj;
					return (Just bindings');
					};
				};
			}) (\ex -> do
				{
				runParser input restOfLineParse;
				errObj <- getConvert (MkSymbol "failure",MkSList (show ex));
				reportError (fsiCurrentErrorPort ?system) errObj;
				return (Just bindings);
				})
			)
			(\errObj -> do
			{
			runParser input restOfLineParse;
			reportError (fsiCurrentErrorPort ?system) errObj;
			return (Just bindings);
			});

		case mbindings' of
			{
			Just bindings' -> interactiveLoop bindings';
			Nothing -> return ();
			};
--} return ();
		};

	interact ::
		(
		Scheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?system :: System cm
		) =>
	 SymbolBindings (ObjLocation r m) ->
	 String ->
	 m ();
	interact bindings filename =
{--
	 catch (do
		{
		bindings' <- psiLoadBindings (fsiPure ?system) bindings filename;
		interactiveLoop bindings';
		})
		(\errObj -> do
		{
		reportError (fsiCurrentErrorPort ?system) errObj;
		});
--} return ();

	interactWithExit ::
		(
		Scheme m r,
		MonadCont m,
		MonadBottom m,
		MonadException (Object r m) m,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?system :: System cm
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
{--
		 catch (psiLoadBindings (fsiPure ?system) bindings filename)
			(\errObj -> do
			{
			reportError (fsiCurrentErrorPort ?system) errObj;
			exitFunc ();
			});
--} return bindings;
		interactiveLoop bindings';
		});
	}
