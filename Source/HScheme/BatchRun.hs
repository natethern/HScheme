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

module Org.Org.Semantic.HScheme.BatchRun(runProgram,runProgramWithExit) where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.SystemInterface;
	import Org.Org.Semantic.HScheme.SExpParser;
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.PortProcedures;
	import Org.Org.Semantic.HScheme.Procedures;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	reportError :: (Scheme m r,?refType :: Type (r ())) =>
	 OutputPort Word8 m -> Object r m -> m ();
	reportError errPort errObj = do
		{
		text <- toString errObj;
		opWriteList errPort (encodeUTF8 ("error: "++text++"\n"));
		opFlush errPort;
		};

	runProgram ::
		(
		Scheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?refType :: Type (r ()),
		?macrobindings :: Binds Symbol (Macro r m),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro r m),
		?system :: FullSystemInterface m r
		) =>
	 m () ->
	 [String] ->
	 Bindings r m ->
	 m ();
	runProgram failproc filenames bindings =
	 catch (do
		{
		objects <- readFiles (fsiOpenInputFile ?system) filenames; 		
		evalObjects (printResult (fsiCurrentOutputPort ?system)) bindings objects;
		})
		(\errObj -> do
		{
		reportError (fsiCurrentErrorPort ?system) errObj;
		failproc;
		});

	runProgramWithExit ::
		(
		Scheme m r,
		MonadCont m,
		MonadBottom m,
		MonadException (Object r m) m,
		?refType :: Type (r ()),
		?macrobindings :: Binds Symbol (Macro r m),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro r m),
		?system :: FullSystemInterface m r
		) =>
	 m () ->
	 [String] ->
	 Bindings r m ->
	 m ();
	runProgramWithExit failproc filenames rootBindings = callCC (\exitFunc -> do
		{
		bindings <- concatenateList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;		
		catch
		 	(do
		 	{
			objects <- readFiles (fsiOpenInputFile ?system) filenames; 		
			evalObjects (printResult (fsiCurrentOutputPort ?system)) bindings objects;
		 	})
			(\errObj -> do
			{
			reportError (fsiCurrentErrorPort ?system) errObj;
			failproc;
--			exitFunc ();
			});
		});
	}
