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

module Org.Org.Semantic.HScheme.IOBindings where
	{
	import Org.Org.Semantic.HScheme.SystemInterface;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;
	import IO;

	handleInputPort :: (LiftedMonad IO m) => Handle -> InputPort Char m;
	handleInputPort h = MkInputPort
		{
		ipRead = lift (do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hGetChar h;
				return (Just c);
				};
			}),
		ipPeek = lift (do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hLookAhead h;
				return (Just c);
				};
			}),
		ipReady	= lift (hReady h),
		ipClose = lift (hClose h)
		};

	handleOutputPort :: (LiftedMonad IO m) => Handle -> OutputPort Char m;
	handleOutputPort h = MkOutputPort
		{
		opWrite = \mc -> lift (case mc of
			{
			Nothing -> hClose h;
			Just c -> hPutChar h c;
			}),
		opFlush = lift (hFlush h)
		};

	stdInputPort :: (LiftedMonad IO m) => InputPort Char m;
	stdInputPort = handleInputPort stdin;

	stdOutputPort :: (LiftedMonad IO m) => OutputPort Char m;
	stdOutputPort = handleOutputPort stdout;

	stdErrorPort :: (LiftedMonad IO m) => OutputPort Char m;
	stdErrorPort = handleOutputPort stderr;

	openInputFile :: (LiftedMonad IO m) =>
	 String -> m (InputPort Char m);
	openInputFile name = do
		{
		h <- lift (openFile name ReadMode);
		return (handleInputPort h);
		};

	openOutputFile :: (LiftedMonad IO m) =>
	 String -> m (OutputPort Char m);
	openOutputFile name = do
		{
		h <- lift (openFile name WriteMode);
		return (handleOutputPort h);
		};

	ioPureSystemInterface :: (Scheme x m r,LiftedMonad IO m) =>
	 PureSystemInterface m r;
	ioPureSystemInterface = MkPureSystemInterface
	 (loadBindingsWithProcs openInputFile stdOutputPort);

	ioFullSystemInterface :: (Scheme x m r,LiftedMonad IO m) =>
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
