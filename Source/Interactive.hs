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
	import Subtype;
	import Type;
	import MonadError;
	import MonadCont;

	reportError :: (Scheme x m r) =>
	 Type (r ()) -> OutputPort Char m -> x -> m ();
	reportError t errPort error = do
		{
		errObj <- getConvert error;
		(MkStringType errText) <- toStringP t (errObj,());
		opWriteStrLn errPort ("error: "++errText);
		};

	interactiveLoop :: (Scheme x m r) =>
	 Type (r ()) -> FullSystemInterface m r -> Bindings r m -> m ();
	interactiveLoop t fsi bindings = do
		{
		let {input = trapEOT (fsiCurrentInputPort fsi);};
		mbindings' <- catchError (do
			{
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
			})
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

	pureInteract :: (Scheme x m r) =>
	 Type (r ()) -> FullSystemInterface m r -> m ();
	pureInteract t fsi = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
			addProcBinding "exit" (exitFuncProc exitFunc),
			monadicStdBindings,
			pureSystemBindings (fsiPure fsi)
			] emptyBindings;
		bindings' <- catchError (psiLoadBindings (fsiPure fsi) bindings "Prelude.pure.scm")
			(\error -> do
			{
			reportError t (fsiCurrentErrorPort fsi) error;
			exitFunc ();
			return undefined;
			});
		interactiveLoop t fsi bindings';
		});

	fullInteract :: (FullScheme x m r) =>
	 Type (r ()) -> FullSystemInterface m r -> m ();
	fullInteract t fsi = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
		 	addProcBinding "exit" (exitFuncProc exitFunc),
			fullStdBindings,
			fullSystemBindings fsi
		 	] emptyBindings;
		bindings' <- catchError (psiLoadBindings (fsiPure fsi) bindings "Prelude.full.scm")
			(\error -> do
			{
			reportError t (fsiCurrentErrorPort fsi) error;
			exitFunc ();
			return undefined;
			});
		interactiveLoop t fsi bindings';
		});
	}
