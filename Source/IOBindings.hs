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

module IOBindings where
	{
	import FullStandardBindings;
	import Bindings;
	import Conversions;
	import Object;
	import LiftedMonad;
	import IO;
	import Type;

	handleInputPort :: (SemiLiftedMonad IO m) => Handle -> InputPort m;
	handleInputPort h = MkInputPort
		{
		ipRead = call (do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hGetChar h;
				return (Just c);
				};
			}),
		ipPeek = call (do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hLookAhead h;
				return (Just c);
				};
			}),
		ipReady	= call (hReady h),
		ipClose = call (hClose h)
		};

	handleOutputPort :: (SemiLiftedMonad IO m) => Handle -> OutputPort m;
	handleOutputPort h = MkOutputPort
		{
		opWrite = \mc -> call (case mc of
			{
			Nothing -> hClose h;
			Just c -> hPutChar h c;
			})
		};

	eofObject :: (Scheme x m r) => Object r m;
	eofObject = nullObject;

	stdinS :: (Scheme x m r,SemiLiftedMonad IO m) =>
	 Type (r ()) -> () -> m (InputPort m);
	stdinS Type () = return (handleInputPort stdin);

	stdoutS :: (Scheme x m r,SemiLiftedMonad IO m) =>
	 Type (r ()) -> () -> m (OutputPort m);
	stdoutS Type () = return (handleOutputPort stdout);

	ioBindings :: (Scheme x m r,SemiLiftedMonad IO m) => Bindings r m -> m (Bindings r m);
	ioBindings = chainList
		[
		addProcBinding	"current-input-port"	stdinS,
		addProcBinding	"current-output-port"	stdoutS
		];
	}
