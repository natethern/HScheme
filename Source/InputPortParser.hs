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

module InputPortParser where
	{
	import Port;
	import HBase;
	
	data Parser c m a = MkParser {unParser :: InputPort c m -> m a};
	
	instance (Monad m) => Monad (Parser c m) where
		{
		return a = MkParser(\_ -> return a);
		
		(MkParser ma) >>= mbf = MkParser(\source -> do
			{
			a <- ma source;
			unParser (mbf a) source;
			});
		
		fail s = MkParser (\_ -> fail s);
		};
	
	instance (Monad m) => SemiLiftedMonad m (Parser c m) where
		{
		call ma = MkParser (\_ -> ma);
		};

	mlift :: (Monad m) => m a -> Parser c m a;
	mlift = call;

	runParser :: (Monad m) => InputPort c m -> Parser c m a -> m a;
	runParser source (MkParser parser) = parser source;

	currentC :: (Monad m) => Parser c m (Maybe c);
	currentC = MkParser ipPeek;
	
	nextC :: (Monad m) => Parser c m ();
	nextC = MkParser (\port -> do
		{
		ipRead port;
		return ();
		});
	}
