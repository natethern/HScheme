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

module Port where
	{
	import HBase;

	data (Monad m) => InputPort c m = MkInputPort
		{
		ipRead	:: m (Maybe c),
		ipPeek	:: m (Maybe c),
		ipReady	:: m Bool,
		ipClose	:: m ()
		};

	data (Monad m) => OutputPort c m = MkOutputPort
		{
		opWrite	:: Maybe c -> m (),
		opFlush :: m ()
		};

	opWriteOne :: (Monad m) => OutputPort c m -> c -> m ();
	opWriteOne port c = opWrite port (Just c);

	opWriteList :: (Monad m) => OutputPort c m -> [c] -> m ();
	opWriteList port [] = return ();
	opWriteList port (c:cs) = do
		{
		opWriteOne port c;
		opWriteList port cs;
		};

	opWriteStrLn :: (Monad m) => OutputPort Char m -> String -> m ();
	opWriteStrLn port s = do
		{
		opWriteList port s;
		opWriteOne port '\n';
		};

	opClose :: (Monad m) => OutputPort c m -> m ();
	opClose port = opWrite port Nothing;

	-- ends immediately
	nullInputPort :: (Monad m) => InputPort c m;
	nullInputPort = MkInputPort
		{
		ipRead = return Nothing,
		ipPeek = return Nothing,
		ipReady = return True,
		ipClose = return ()
		};
	
	-- does nothing
	nullOutputPort :: (Monad m) => OutputPort c m;
	nullOutputPort = MkOutputPort
		{
		opWrite = \_ -> return (),
		opFlush = return ()
		};
	
	nothingEOT :: (Monad m) => Maybe Char -> m (Maybe Char);
	nothingEOT (Just '\EOT') = return Nothing;
	nothingEOT x = return x;
	
	trapEOT :: (Monad m) => InputPort Char m -> InputPort Char m;
	trapEOT port = MkInputPort
		{
		ipRead = (ipRead port) >>= nothingEOT,
		ipPeek = (ipPeek port) >>= nothingEOT,
		ipReady = ipReady port,
		ipClose = ipClose port
		};
	}
