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
	runProgram,
	runProgramBindings,runProgramBindingsWithExit
	) where
	{
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	printResult :: (Build cm r) =>
	 OutputPort Word8 cm -> Object r m -> cm ();
	printResult output obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteList output (encodeUTF8 (str ++ "\n"));
		};

	reportError :: (Build cm r,?objType :: Type (Object r m)) =>
	 OutputPort Word8 cm -> Object r m -> cm ();
	reportError errPort errObj = do
		{
		text <- toString errObj;
		opWriteList errPort (encodeUTF8 ("error: "++text++"\n"));
		opFlush errPort;
		};

	class
		(
		Build cm r,
		Scheme m r,
		MonadException (Object r m) cm
		) =>
	 RunnableScheme cm m r where
		{
		rsRun :: (?objType :: Type (Object r m)) =>
		 m () -> cm ();
		rsLift :: forall a. (?objType :: Type (Object r m)) =>
		 cm a -> m a;
		};

	runProgram ::
		(
		RunnableScheme cm m r,
		?objType :: Type (Object r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?system :: FullSystemInterface cm m r
		) =>
	 (((Symbol -> Maybe (ObjLocation r m)) -> m ()) -> m ()) ->
	 cm () ->
	 [String] ->
	 cm ();
	runProgram mrun failproc filenames =
	 catch (do
		{
		objects <- readFiles (fsiOpenInputFile ?system) filenames;
		program <- interpretTopLevelExpressionsEat (\obj -> rsLift (printResult (fsiCurrentOutputPort ?system) obj)) objects;
		rsRun (mrun program);
		})
		(\errObj -> do
		{
		reportError (fsiCurrentErrorPort ?system) errObj;
		failproc;
		});

	runProgramBindings ::
		(
		RunnableScheme cm m r,
		?objType :: Type (Object r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?system :: FullSystemInterface cm m r
		) =>
	 cm () ->
	 [String] ->
	 Bindings r m ->
	 cm ();
	runProgramBindings failproc filenames bindings =
	 runProgram mrun failproc filenames where
		{
		mrun lm = lm (getBinding bindings);
		};

	runProgramBindingsWithExit ::
		(
		RunnableScheme cm m r,
		MonadCont m,
		?objType :: Type (Object r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?system :: FullSystemInterface cm m r
		) =>
	 cm () ->
	 [String] ->
	 Bindings r m ->
	 cm ();
	runProgramBindingsWithExit failproc filenames rootBindings =
	 runProgram mrun failproc filenames where
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
