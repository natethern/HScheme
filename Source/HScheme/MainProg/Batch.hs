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

module Org.Org.Semantic.HScheme.MainProg.Batch
	(
	RunnableScheme(..),
	runProgramBindings,
	runProgramBindingsWithExit
	) where
	{
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class
		(
		Build cm r,
		Scheme m r,
		MonadException (Object r m) cm
		) =>
	 RunnableScheme cm m r where
		{
		rsRunInterp ::
		 (Object r m -> cm ()) ->
		 (forall a. (ArgumentList m m r a) => ((Symbol -> Maybe (ObjLocation r m)) -> m a) -> m a) ->
		 cm ((Symbol -> Maybe (ObjLocation r m)) -> m [Object r m]) ->
		 ((Object r m -> m ()) -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m ())) ->
		 cm ();
		};
		

{--		
rsRunInterp outproc mrun interpList interpEat = do
	{
	program <- interpList;
	results <- rsRun (mrun program);
	sinkList outproc results;
	return ();
	}

rsRunInterp outproc mrun interpList interpEat = do
	{
	program <- interpEat (rsLift . outproc);
	rsRun (mrun program);
	}
--}

	instance
		(
		MonadGettableReference m r,
		MonadCreatable m r,
		MonadException (Object r m) m
		) =>
	 RunnableScheme m m r where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat outproc;
			mrun program;
			}
		};


	instance
		(
		BuildThrow IO (Object r Identity) r,
		MonadException (Object r Identity) IO,
		BuildThrow Identity (Object r Identity) r
		) =>
	 RunnableScheme IO Identity r where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpList;
			results <- return (unIdentity (mrun program));
			sinkList outproc results;
			return ();
			}
		};


	readFiles ::
		(
		Monad cm,
		?load :: String -> cm [Object r m]
		) =>
	 [String] -> cm [Object r m];
	readFiles [] = return [];
	readFiles (name:names) = do
		{
		objs1 <- ?load name;
		objsr <- readFiles names;
		return (objs1 ++ objsr);
		};

	runProgram ::
		(
		MonadFix m,FunctorApplyReturn m,
		RunnableScheme cm m r,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?load :: String -> cm [Object r m]
		) =>
	 (forall a. (ArgumentList m m r a) => ((Symbol -> Maybe (ObjLocation r m)) -> m a) -> m a) ->
	 (Object r m -> cm ()) ->
	 (Object r m -> cm ()) ->
	 [String] ->
	 cm ();
	runProgram mrun outproc failproc filenames =
	 catch (do
		{
		objects <- readFiles filenames;
		
		rsRunInterp outproc mrun
		 (interpretTopLevelExpressionsList objects)
		 (\proc -> interpretTopLevelExpressionsEat proc objects);
		})
		failproc;

	runProgramBindings ::
		(
		MonadFix m,FunctorApplyReturn m,
		RunnableScheme cm m r,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?load :: String -> cm [Object r m]
		) =>
	 (Object r m -> cm ()) ->
	 (Object r m -> cm ()) ->
	 [String] ->
	 SymbolBindings (ObjLocation r m) ->
	 cm ();
	runProgramBindings outproc failproc filenames bindings =
	 runProgram mrun outproc failproc filenames where
		{
		mrun lm = lm (getBinding bindings);
		};

	runProgramBindingsWithExit ::
		(
		MonadFix m,FunctorApplyReturn m,
		RunnableScheme cm m r,
		MonadCont m,
		?objType :: Type (Object r m),
		?macrobindings :: SymbolBindings (Macro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?load :: String -> cm [Object r m]
		) =>
	 (Object r m -> cm ()) ->
	 (Object r m -> cm ()) ->
	 [String] ->
	 SymbolBindings (ObjLocation r m) ->
	 cm ();
	runProgramBindingsWithExit outproc failproc filenames rootBindings =
	 runProgram mrun outproc failproc filenames where
		{
		mrun lm = callCC (\exitFunc -> do
			{
			bindings <- concatenateList
				[
				addProcBinding "exit" (exitFuncProc exitFunc)
				] rootBindings;		
			lm (getBinding bindings);
			});
		};
	}
