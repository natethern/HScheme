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

module Org.Org.Semantic.HScheme.Core.Port where
	{
	import Org.Org.Semantic.HBase;

	type InputPort c m = Closeable m (PeekSource m (Maybe c));
	ipClose = clClose;
	ipRead = psSource . clItem;
	ipPeek = psPeek . clItem;
	ipReady = psReady . clItem;
	ipReadAll :: (Monad m) =>
	 Closeable m (PeekSource m (Maybe c)) -> m [c];
	ipReadAll = getMaybePeekSourceContents . clItem;

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

	data System m = MkSystem
		{
		fsiCurrentInputPort		:: InputPort Word8 m,
		fsiCurrentOutputPort	:: OutputPort Word8 m,
		fsiCurrentErrorPort		:: OutputPort Word8 m,
		fsiOpenInputFile		:: String -> m (InputPort Word8 m),
		fsiOpenOutputFile		:: String -> m (OutputPort Word8 m)
		};
	}
