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

	ipClose :: (Monad m) =>
	 InputPort c m -> m ();
	ipClose = clClose;

	ipRead :: (Monad m) =>
	 InputPort c m -> m (Maybe c);
	ipRead = psSource . clItem;

	ipPeek :: (Monad m) =>
	 InputPort c m -> m (Maybe c);
	ipPeek = psPeek . clItem;

	ipReady :: (Monad m) =>
	 InputPort c m -> m Bool;
	ipReady = psReady . clItem;

	ipReadAll :: (Monad m) =>
	 InputPort c m -> m [c];
	ipReadAll = getMaybePeekSourceContents . clItem;

	type OutputPort c m = Closeable m (FlushSink m c);

	opClose :: (Monad m) =>
	 OutputPort c m -> m ();
	opClose = clClose;

	opWrite :: (Monad m) =>
	 OutputPort c m -> Maybe c -> m ();
	opWrite = fsSink . clItem;

	opFlush :: (Monad m) =>
	 OutputPort c m -> m ();
	opFlush = fsFlush . clItem;

	opWriteOne :: (Monad m) =>
	 OutputPort c m -> c -> m ();
	opWriteOne = fsSinkOne . clItem;

	opWriteList :: (Monad m) =>
	 OutputPort c m -> [c] -> m ();
	opWriteList = fsSinkList . clItem;

	opWriteStr :: (Monad m) =>
	 OutputPort c m -> [c] -> m ();
	opWriteStr = opWriteList;

	opWriteStrLn :: (Monad m) =>
	 OutputPort Char m -> [Char] -> m ();
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
