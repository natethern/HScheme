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
	import FMapBindings;
	import IOBindings;
	import FullStandardBindings;
	import SExpParser;
	import StandardBindings;
	import Bindings;
	import PortProcedures;
	import Procedures;
	import TopLevel;
	import Evaluate;
	import Conversions;
	import Object;
	import InputPortParser;
	import Port;
	import LiftedMonad;
	import Subtype;
	import Type;
	import IO;
	import MonadError;
	import MonadCont;

	printeval :: (Scheme x m r) =>
	 OutputPort Char m -> Bindings r m -> [Object r m] -> m (Bindings r m);
	printeval port bindings [] = return bindings;
	printeval port bindings (obj:objs) = do
		{
		(newBindings,result) <- topLevelEvaluate bindings obj;
		str <- toString result;
		opWriteStrLn port str;
		printeval port newBindings objs;	
		};

	toList :: Maybe a -> [a];
	toList Nothing = [];
	toList (Just a) = [a];

	reportError :: (Scheme x m r) =>
	 Type (r ()) -> OutputPort Char m -> x -> m ();
	reportError t errPort error = do
		{
		errObj <- getConvert error;
		(MkStringType errText) <- toStringS t (errObj,());
		opWriteStrLn errPort ("error: "++errText);
		};

	interactiveLoop :: (SemiLiftedMonad IO m,Scheme x m r) =>
	 Type (r ()) -> InputPort Char m -> Bindings r m -> m a;
	interactiveLoop t reader bindings = do
		{
		newBindings <- catchError (do
			{
			opWriteList stdOutputPort "hscheme> ";
			opFlush stdOutputPort;
			mobject <- runParser reader expressionP;
			printeval stdOutputPort bindings (toList mobject);
			}) (\error -> do
			{
			runParser reader restOfLineP;
			reportError t stdErrorPort error;
			return bindings;
			});
		interactiveLoop t reader newBindings;
		};

	pureInteract ::
		(
		SemiLiftedMonad IO m,
		Scheme x m r
		) =>
	 Type (r ()) -> m ();
	pureInteract t = callCC (\exitFunc -> do
		{
		bindings <- chain
		 (addProcBinding "exit" (exitFuncProc exitFunc))
		 monadicStdBindings emptyBindings;
		port <- openInputFileS t (MkStringType "Prelude.pure.scm",());
		objs <- runParser port  (expressionsP t);
		inputPortCloseS t (port,());
		bindings' <- printeval nullOutputPort bindings objs;
		interactiveLoop t stdInputPort bindings';
		});

	fullInteract ::
		(
		SemiLiftedMonad IO m,
		FullScheme x m r
		) =>
	 Type (r ()) -> m ();
	fullInteract t = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
			fullStdBindings,
			ioBindings,
		 	addProcBinding "exit" (exitFuncProc exitFunc)
		 	] emptyBindings;
		port <- openInputFileS t (MkStringType "Prelude.pure.scm",());
		objs <- runParser port  (expressionsP t);
		inputPortCloseS t (port,());
		bindings' <- printeval nullOutputPort bindings objs;
		interactiveLoop t stdInputPort bindings';
		});
	}
