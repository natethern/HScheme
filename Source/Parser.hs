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

module Parser where
	{
	data Parser c m a = MkParser (m c -> c -> m (a,c));
	
	unParser (MkParser f) = f;
	
	instance (Monad m) => Monad (Parser c m) where
		{
		return a = MkParser(\_ c -> return (a,c));
		
		(MkParser ma) >>= mbf = MkParser(\source c -> do
			{
			(a,c') <- ma source c;
			unParser (mbf a) source c';
			});
		
		fail s = MkParser (\_ _ -> fail s);
		};
	
	mlift :: (Monad m) => m a -> Parser c m a;
	mlift ma = MkParser (\_ c -> do
		{
		a <- ma;
		return (a,c);
		});
	
	runParser :: (Monad m) => m c -> Parser c m a -> m a;
	runParser source (MkParser parser) = do
		{
		c <- source;
		(a,mc) <- parser source c;
		return a;
		};
	
	currentC :: (Monad m) => Parser c m c;
	currentC = MkParser (\_ c -> return (c,c));
	
	nextC :: (Monad m) => Parser (Maybe c) m ();
	nextC = MkParser (\source c -> do
		{
		case c of
			{
			Just _ -> do
				{
				c' <- source;
				return ((),c');
				};
			Nothing -> return ((),c);
			};
		});
	}
