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
	import SystemInterface;
	import Object;
	import Port;
	import HBase;
	import IO;

	handleInputPort :: (SemiLiftedMonad IO m) => Handle -> InputPort Char m;
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

	handleOutputPort :: (SemiLiftedMonad IO m) => Handle -> OutputPort Char m;
	handleOutputPort h = MkOutputPort
		{
		opWrite = \mc -> call (case mc of
			{
			Nothing -> hClose h;
			Just c -> hPutChar h c;
			}),
		opFlush = call (hFlush h)
		};

	stdInputPort :: (SemiLiftedMonad IO m) => InputPort Char m;
	stdInputPort = handleInputPort stdin;

	stdOutputPort :: (SemiLiftedMonad IO m) => OutputPort Char m;
	stdOutputPort = handleOutputPort stdout;

	stdErrorPort :: (SemiLiftedMonad IO m) => OutputPort Char m;
	stdErrorPort = handleOutputPort stderr;

	openInputFile :: (SemiLiftedMonad IO m) =>
	 String -> m (InputPort Char m);
	openInputFile name = do
		{
		h <- call (openFile name ReadMode);
		return (handleInputPort h);
		};

	openOutputFile :: (SemiLiftedMonad IO m) =>
	 String -> m (OutputPort Char m);
	openOutputFile name = do
		{
		h <- call (openFile name WriteMode);
		return (handleOutputPort h);
		};

	ioFullSystemInterface :: (Scheme x m r,SemiLiftedMonad IO m) =>
	 FullSystemInterface m r;
	ioFullSystemInterface = MkFullSystemInterface
		{
		fsiPure = MkPureSystemInterface (loadBindingsWithProcs openInputFile stdOutputPort),
		fsiCurrentInputPort		= stdInputPort,
		fsiCurrentOutputPort	= stdOutputPort,
		fsiCurrentErrorPort		= stdErrorPort,
		fsiOpenInputFile		= openInputFile,
		fsiOpenOutputFile		= openOutputFile
		};
	}
