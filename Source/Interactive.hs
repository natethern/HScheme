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
	import FullStandardBindings;
	import SExpParser;
	import Bindings;
	import StandardBindings;
	import Procedures;
	import Evaluate;
	import Conversions;
	import Object;
	import Parser;
	import LiftedMonad;
	import FMapBindings;
	import MonadError;
	import MonadCont;
	import IO;
	import Subtype;
	import Type;

	clearToReturn :: (Monad m) => m (Maybe Char) -> m ();
	clearToReturn reader = do
		{
		mc <- reader;
		case mc of
			{
			Nothing -> return ();
			(Just '\n') -> return ();
			_ -> clearToReturn reader;
			};
		};

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
	 Type (r ()) -> m (Maybe Char) -> Bindings r m -> m a;
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
			clearToReturn reader;
			errObj <- getConvert error;
			(MkStringType errText) <- toStringS t (errObj,());
			call (putStrLn ("error: "++errText));
			return bindings;
			});
		interactiveLoop t reader newBindings;
		};

	readString :: (SemiLiftedMonad IO m) =>
	 m (Maybe Char);
	readString = do
		{
		c <- call getChar;
		return (Just c);
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
		 stdBindings emptyBindings;		
		interactiveLoop t readString bindings;
		});

	fullInteract ::
		(
		SemiLiftedMonad IO m,
		FullScheme x m r
		) =>
	 Type (r ()) -> m ();
	fullInteract t = callCC (\exitFunc -> do
		{
		bindings <- chain
		 (addProcBinding "exit" (exitFuncProc exitFunc))
		 fullStdBindings emptyBindings;		
		interactiveLoop t readString bindings;
		});
	}
