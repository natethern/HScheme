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
	Runnable(..),
	RunnableScheme(..),
	runObjects,
	runObjectsWithExit,
	printResult
	) where
	{
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class
		(
		Monad m,
		Monad cm
		) =>
	 Runnable cm m where
		{
		rsRun :: m () -> cm ();
		};

	class
		(
		Build cm r,
		Runnable cm m
		) =>
	 RunnableScheme cm m r obj where
		{
		rsRunInterp ::
		 (obj -> cm ()) ->
		 (forall a. (ArgumentList r obj a) => ((Symbol -> Maybe (r obj)) -> m a) -> m a) ->
		 cm ((Symbol -> Maybe (r obj)) -> m [obj]) ->
		 ((obj -> m ()) -> cm ((Symbol -> Maybe (r obj)) -> m ())) ->
		 cm ();
		};

	instance (Monad m) =>
	 Runnable m m where
		{
		rsRun = id;
		};

	instance
		(
		Build m r,
		Monad m,
		ListObject r obj
		) =>
	 RunnableScheme m m r obj where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat outproc;
			mrun program;
			};
		};

	instance Runnable IO Identity where
		{
		rsRun _ = return ();
		};

	instance
		(
		ObjectSubtype r obj obj,
		Build IO r
		) =>
	 RunnableScheme IO Identity r obj where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpList;
			results <- return (unIdentity (mrun program));
			forDo outproc results;
			return ();
			};
		};

	printResult ::
		(
		ToString IO obj,
		?objType :: Type obj,
		?stdout :: FlushSink IO Word8
		) =>
	 obj -> IO ();
	printResult obj = do
		{
		str <- toString obj;
		fsSinkList ?stdout (encodeUTF8 (str ++ "\n"));
		};

	evaluateObjects ::
		(
		InterpretObject m r obj,
		AssembleError cm obj,
		RunnableScheme cm m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m)
		) =>
	 (forall a. (ArgumentList r obj a) => ((Symbol -> Maybe (r obj)) -> m a) -> m a) ->
	 (obj -> cm ()) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 cm ();
	evaluateObjects mrun outproc command objects = rsRunInterp outproc mrun
	  (interpretTopLevelExpressionsList command objects)
	  (\proc -> interpretTopLevelExpressionsEat proc command objects);

	runObjects ::
		(
		InterpretObject m r obj,
		AssembleError cm obj,
		RunnableScheme cm m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m)
		) =>
	 (obj -> cm ()) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 SymbolBindings (r obj) ->
	 cm ();
	runObjects outproc command objects bindings =
	 evaluateObjects mrun outproc command objects where
		{
		mrun lm = lm (getBinding bindings);
		};

	runObjectsWithExit ::
		(
		InterpretObject m r obj,
		AssembleError cm obj,
		RunnableScheme cm m r obj,
		MonadCont m,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m)
		) =>
	 (obj -> cm ()) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 SymbolBindings (r obj) ->
	 cm ();
	runObjectsWithExit outproc command objects rootBindings =
	 evaluateObjects mrun outproc command objects where
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
