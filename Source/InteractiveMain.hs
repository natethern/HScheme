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

module Main where
	{
	import Org.Org.Semantic.HScheme.IOBindings;
	import Org.Org.Semantic.HScheme.SystemInterface;
	import Org.Org.Semantic.HScheme.Interactive;
	import Org.Org.Semantic.HScheme.SchemeCPS;
	import Org.Org.Semantic.HBase;

	type M r = SchemeCPS r (IO ());

	type Interact r = FullSystemInterface (M r) r -> (M r) ();

	instance MonadCreatable IO Constant where
		{
		new a = return (MkConstant a);
		};

	instance MonadGettableReference IO Constant where
		{
		get (MkConstant a) = return a;
		};

	getFull :: IO Bool;
	getFull = return True;

	doInteract :: 
		(
		MonadGettableReference IO r,
		MonadCreatable IO r
		) =>
	 (FullSystemInterface (M r) r -> (M r) ()) -> IO ();
	doInteract interact = doMonadContinuationPass
	 (\_ -> return "error in catch code!")
	 (interact ioFullSystemInterface);

	main :: IO ();
	main = do
		{
		full <- getFull;
		if full
		 then doInteract (fullInteract :: Interact IORef)
		 else doInteract (pureInteract :: Interact Constant);
		};
	}
