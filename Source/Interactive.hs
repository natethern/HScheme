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

module Interactive where
	{
	import SystemInterface;
	import FMapBindings;
	import FullStandardBindings;
	import SExpParser;
	import StandardBindings;
	import Bindings;
	import Procedures;
	import Conversions;
	import Object;
	import InputPortParser;
	import Port;
	import HBase;

	reportError :: (Scheme x m r) =>
	 Type (r ()) -> OutputPort Char m -> x -> m ();
	reportError t errPort error = do
		{
		errObj <- getConvert error;
		(MkStringType errText) <- toStringP t (errObj,());
		opWriteStrLn errPort ("error: "++errText);
		opFlush errPort;
		};

	interactiveLoop :: (Scheme x m r,
		MonadBottom m,IsA x Exception
		) =>
	 Type (r ()) -> FullSystemInterface m r -> Bindings r m -> m ();
	interactiveLoop t fsi bindings = do
		{
		let {input = trapEOT (fsiCurrentInputPort fsi);};
		mbindings' <- catchSingle (catchBottom (do {
			opWriteList (fsiCurrentOutputPort fsi) "hscheme> ";
			opFlush (fsiCurrentOutputPort fsi);
			mobject <- portRead input;
			case mobject of
				{
				Nothing -> return Nothing;
				Just obj -> do
					{
					bindings' <- printeval (fsiCurrentOutputPort fsi) bindings obj;
					return (Just bindings');
					};
				};
			}) (\ex -> throwSingle (convert ex))
			)
			(\error -> do
			{
			runParser input restOfLineParser;
			reportError t (fsiCurrentErrorPort fsi) error;
			return (Just bindings);
			});

		case mbindings' of
			{
			Just bindings' -> interactiveLoop t fsi bindings';
			Nothing -> return ();
			};
		};

	interact ::
		(
		Scheme x m r,
		MonadBottom m,
		IsA x Exception
		) =>
	 Type (r ()) ->
	 FullSystemInterface m r ->
	 Bindings r m ->
	 String ->
	 m ();
	interact t fsi rootBindings filename = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;
		bindings' <- catchSingle (psiLoadBindings (fsiPure fsi) bindings filename)
			(\error -> do
			{
			reportError t (fsiCurrentErrorPort fsi) error;
			exitFunc ();
			return undefined;
			});
		interactiveLoop t fsi bindings';
		});

	pureInteract ::
		(
		Scheme x m r,
		MonadBottom m,
		IsA x Exception
		) =>
	 FullSystemInterface m r -> m ();
	pureInteract fsi = do
		{
		bindings <- chainList
			[
			monadicStdBindings,
			pureSystemBindings (fsiPure fsi)
			] emptyBindings;
		interact Type fsi bindings  "Prelude.pure.scm";
		};

	fullInteract ::
		(
		FullScheme x m r,
		MonadBottom m,
		IsA x Exception
		) =>
	 FullSystemInterface m r -> m ();
	fullInteract fsi = do
		{
		bindings <- chainList
			[
			fullStdBindings,
			fullSystemBindings fsi
			] emptyBindings;
		interact Type fsi bindings  "Prelude.full.scm";
		};
	}
