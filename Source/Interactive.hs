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

	printeval :: (SemiLiftedMonad IO m,Scheme x m r) =>
	 Bindings r m -> [Object r m] -> m (Bindings r m);
	printeval bindings [] = return bindings;
	printeval bindings (obj:objs) = do
		{
		(newBindings,result) <- defineEvaluate bindings obj;
		str <- toString result;
		call (putStrLn str);
		printeval newBindings objs;	
		};

	toList :: Maybe a -> [a];
	toList Nothing = [];
	toList (Just a) = [a];

	interactiveLoop :: (SemiLiftedMonad IO m,Scheme x m r) =>
	 Type (r ()) -> InputPort Char m -> Bindings r m -> m a;
	interactiveLoop t reader bindings = do
		{
		newBindings <- catchError (do
			{
			ref <- call (do
				{
				putStr "hscheme> ";
				hFlush stdout;
				});
			mobject <- runParser reader expressionP;
			printeval bindings (toList mobject);
			}) (\error -> do
			{
			runParser reader restOfLineP;
			errObj <- getConvert error;
			(MkStringType errText) <- toStringS t (errObj,());
			call (putStrLn ("error: "++errText));
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
		bindings' <- printeval bindings objs;
		interactiveLoop t stdinPort bindings';
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
		bindings' <- printeval bindings objs;
		interactiveLoop t stdinPort bindings';
		});
	}
