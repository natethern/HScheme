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
	import Org.Org.Semantic.HScheme;
	import Org.Org.Semantic.HBase;

	type CPS r = SchemeCPS r (IO ());

	instance Show (SchemeCPSError IORef (IO ())) where
		{
		show (StringError s) = s;
		show (ExceptionError ex) = show ex;
		show (ObjError ex) = "Scheme object error";
		};

	type Interact m r = FullSystemInterface m r -> m ();

	data SchemeFlavour = FullFlavour | PureFlavour | StrictPureFlavour;

	getFlavour :: IO SchemeFlavour;
	getFlavour = return StrictPureFlavour;

	cpsInteract :: 
		(
		MonadGettableReference IO r,
		MonadCreatable IO r
		) =>
	 (Interact (CPS r) r) -> IO ();
	cpsInteract interact = runContinuationPass
	 (\_ -> fail "error in catch code!")
	 return
	 (interact (ioFullSystemInterface lift));

	main :: IO ();
	main = do
		{
		flavour <- getFlavour;
		case flavour of
			{
			FullFlavour -> cpsInteract (fullInteract :: Interact (CPS IORef) IORef);
			PureFlavour -> (pureInteract :: Interact IO (Constant IO)) (ioFullSystemInterface id);
			StrictPureFlavour -> (strictPureInteract :: Interact IO (Constant IO)) (ioFullSystemInterface id);
			};
		};


{-- for profiling
	rep :: Integer -> IO () -> IO ();
	rep 0 f = return ();
	rep n f = do
		{
		f;
		rep (n-1) f;
		};

	main :: IO ();
	main = rep 100 main';
--}
	}
