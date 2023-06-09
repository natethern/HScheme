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

module Org.Org.Semantic.HScheme.Imperative.IOSystem where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	handleInputPort :: Handle -> InputPort Word8 IO;
	handleInputPort h = MkCloseable (closeSession h) (handlePeekSource h);

	remonadInputPort :: (Monad m,Monad n) =>
	 (forall a. m a -> n a) -> InputPort c m -> InputPort c n;
	remonadInputPort remonad ip = fmap (remonadPeekSource remonad) (remonadCloseable remonad ip);

	handleOutputPort :: Handle -> OutputPort Word8 IO;
	handleOutputPort h = MkCloseable (closeSession h) (handleFlushSink h);

	remonadOutputPort :: (Monad m,Monad n) =>
	 (forall a. m a -> n a) -> OutputPort c m -> OutputPort c n;
	remonadOutputPort remonad op = fmap (remonadFlushSink remonad) (remonadCloseable remonad op);

	stdInputPort :: (?stdin :: PeekSource IO (Maybe Word8)) =>
	 InputPort Word8 IO;
	stdInputPort = nullCloseable ?stdin;

	stdOutputPort ::  (?stdout :: FlushSink IO Word8) =>
	 OutputPort Word8 IO;
	stdOutputPort = nullCloseable ?stdout;

	stdErrorPort ::  (?stderr :: FlushSink IO Word8) =>
	 OutputPort Word8 IO;
	stdErrorPort = nullCloseable ?stderr;

	openInputFile :: (Monad m) =>
	 (forall a. IO a -> m a) -> String -> m (InputPort Word8 m);
	openInputFile remonad name = do
		{
		h <- remonad (openFileRead name);
		return (remonadInputPort remonad (handleInputPort h));
		};

	openOutputFile :: (Monad m) =>
	 (forall a. IO a -> m a) -> String -> m (OutputPort Word8 m);
	openOutputFile remonad name = do
		{
		h <- remonad (openFileReadWrite name);
		return (remonadOutputPort remonad (handleOutputPort h));
		};

	ioSystem ::
		(
		Monad m,
		?stdin :: PeekSource IO (Maybe Word8),
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 (forall a. IO a -> m a) -> System m;
	ioSystem remonad = MkSystem
		{
		fsiCurrentInputPort		= remonadInputPort remonad stdInputPort,
		fsiCurrentOutputPort	= remonadOutputPort remonad stdOutputPort,
		fsiCurrentErrorPort		= remonadOutputPort remonad stdErrorPort,
		fsiOpenInputFile		= openInputFile remonad,
		fsiOpenOutputFile		= openOutputFile remonad
		};
	}
