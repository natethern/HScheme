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

module Org.Org.Semantic.HScheme.Port where
	{
	import Org.Org.Semantic.HBase;

	type InputPort c m = Closeable m (PeekSource m c);
	ipClose = clClose;
	ipRead = psSource . clItem;
	ipPeek = psPeek . clItem;
	ipReady = psReady . clItem;
	ipReadAll :: (Monad m) =>
	 Closeable m (PeekSource m c) -> m [c];
	ipReadAll = getPeekSourceContents . clItem;

	type OutputPort c m = Closeable m (FlushSink m c);
	opClose = clClose;
	opWrite = fsSink . clItem;
	opFlush = fsFlush . clItem;
	opWriteOne :: (Monad m) =>
	 Closeable m (FlushSink m c) -> c -> m ();
	opWriteOne = fsSinkOne . clItem;
	opWriteList :: (Monad m) =>
	 Closeable m (FlushSink m c) -> [c] -> m ();
	opWriteList = fsSinkList . clItem;
	opWriteStr :: (Monad m) =>
	 Closeable m (FlushSink m c) -> [c] -> m ();
	opWriteStr = opWriteList;

	opWriteStrLn :: (Monad m) =>
	 Closeable m (FlushSink m Char) -> [Char] -> m ();
	opWriteStrLn op s = do
		{
		opWriteStr op s;
		opWriteOne op '\n';
		};

{--
	data (Monad m) => InputPort c m = MkInputPort
		{
		ipRead	:: m (Maybe c),
		ipPeek	:: m (Maybe c),
		ipReady	:: m Bool,
		ipClose	:: m ()
		};

	ipReadAll :: (Monad m) =>
	 InputPort c m -> m [c];
	ipReadAll ip = accumulateSource (ipRead ip);

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

	opWriteStr :: (Monad m) => OutputPort Char m -> String -> m ();
	opWriteStr = opWriteList;

	opWriteStrLn :: (Monad m) => OutputPort Char m -> String -> m ();
	opWriteStrLn port s = do
		{
		opWriteStr port s;
		opWriteOne port '\n';
		};

	opClose :: (Monad m) => OutputPort c m -> m ();
	opClose port = opWrite port Nothing;
--}
	}
