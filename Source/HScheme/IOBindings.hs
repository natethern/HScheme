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

	handleInputPort :: Handle -> InputPort Char IO;
	handleInputPort h = MkInputPort
		{
		ipRead = do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hGetChar h;
				return (Just c);
				};
			},
		ipPeek = do
			{
			eof <- hIsEOF h;
			if (eof) then return Nothing else do
				{
				c <- hLookAhead h;
				return (Just c);
				};
			},
		ipReady	= hReady h,
		ipClose = hClose h
		};

	remonadInputPort :: (Monad m,Monad n) =>
	 (forall a. m a -> n a) -> InputPort c m -> InputPort c n;
	remonadInputPort remonad (MkInputPort rd pk rdy cl) = MkInputPort
	 (remonad rd) (remonad pk) (remonad rdy) (remonad cl);

	handleOutputPort :: Handle -> OutputPort Char IO;
	handleOutputPort h = MkOutputPort
		{
		opWrite = \mc -> case mc of
			{
			Nothing -> hClose h;
			Just c -> hPutChar h c;
			},
		opFlush = hFlush h
		};

	remonadOutputPort :: (Monad m,Monad n) =>
	 (forall a. m a -> n a) -> OutputPort c m -> OutputPort c n;
	remonadOutputPort remonad (MkOutputPort w f) = MkOutputPort
	 (\mc -> remonad (w mc)) (remonad f);

	stdInputPort :: InputPort Char IO;
	stdInputPort = handleInputPort stdin;

	stdOutputPort :: OutputPort Char IO;
	stdOutputPort = handleOutputPort stdout;

	stdErrorPort :: OutputPort Char IO;
	stdErrorPort = handleOutputPort stderr;

	openInputFile :: (Monad m) =>
	 (forall a. IO a -> m a) -> String -> m (InputPort Char m);
	openInputFile remonad name = do
		{
		h <- remonad (openFile name ReadMode);
		return (remonadInputPort remonad (handleInputPort h));
		};

	nameInPath :: String -> String -> String;
	nameInPath path name@('/':_) = name;
	nameInPath path name = path ++ "/" ++ name;

	openFileReadWithPaths :: [String] -> String -> IO Handle;
	openFileReadWithPaths [] name = fail ("file "++(show name)++" not found in paths");
	openFileReadWithPaths (path:paths) name = catchSingle
		(openFile (nameInPath path name) ReadMode)
		(\_ -> openFileReadWithPaths paths name);

	openInputFileWithPaths :: (Monad m) =>
	 (forall a. IO a -> m a) -> [String] -> String -> m (InputPort Char m);
	openInputFileWithPaths remonad paths name = do
		{
		h <- remonad (openFileReadWithPaths paths name);
		return (remonadInputPort remonad (handleInputPort h));
		};

	openOutputFile :: (Monad m) =>
	 (forall a. IO a -> m a) -> String -> m (OutputPort Char m);
	openOutputFile remonad name = do
		{
		h <- remonad (openFile name WriteMode);
		return (remonadOutputPort remonad (handleOutputPort h));
		};

	ioPureSystemInterface :: (Scheme m r,Monad m) =>
	 (forall a. IO a -> m a) -> [String] -> PureSystemInterface m r;
	ioPureSystemInterface remonad loadpaths = MkPureSystemInterface
	 (loadBindingsWithProcs (openInputFileWithPaths remonad loadpaths) (remonadOutputPort remonad stdOutputPort));

	ioQuietPureSystemInterface :: (Scheme m r,Monad m) =>
	 (forall a. IO a -> m a) -> [String] -> PureSystemInterface m r;
	ioQuietPureSystemInterface remonad loadpaths = MkPureSystemInterface
	 (loadBindingsWithProcs (openInputFileWithPaths remonad loadpaths) (remonadOutputPort remonad stdErrorPort));

	ioFullSystemInterface :: (Scheme m r,Monad m) =>
	 (forall a. IO a -> m a) -> [String] -> FullSystemInterface m r;
	ioFullSystemInterface remonad loadpaths = MkFullSystemInterface
		{
		fsiPure = ioPureSystemInterface remonad loadpaths,
		fsiCurrentInputPort		= remonadInputPort remonad stdInputPort,
		fsiCurrentOutputPort	= remonadOutputPort remonad stdOutputPort,
		fsiCurrentErrorPort		= remonadOutputPort remonad stdErrorPort,
		fsiOpenInputFile		= openInputFile remonad,
		fsiOpenOutputFile		= openOutputFile remonad
		};
	}
