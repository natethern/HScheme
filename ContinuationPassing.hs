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

module ContinuationPassing where
	{
	import LiftedMonad;
	import MonadError;
	import MonadCont;
	
	newtype CPS ex p a = MkCPS {unCPS :: (ex -> p) -> (a -> p) -> p};
	
	instance (Error ex) => Monad (CPS ex p) where
		{
		return a = MkCPS (\_ cont -> cont a);
		
		(MkCPS ma) >>= bf = MkCPS (\fcont cont -> ma fcont (\a -> unCPS (bf a) fcont cont));
		
		fail s = MkCPS (\fcont _ -> fcont (strMsg s));
		};
	
	instance (Monad m,Error ex) => SemiLiftedMonad m (CPS ex (m a)) where
		{
		call ioa = MkCPS (\_ cont -> ioa >>= cont);
		};

	instance (Error ex) => MonadError ex (CPS ex p) where
		{
		throwError ex = MkCPS (\fcont _ -> fcont ex);
		catchError (MkCPS f) catchClause = MkCPS (\fcont cont -> f (\ex -> unCPS (catchClause ex) fcont cont) cont);
		};

{-- can this be done?	
	instance (Monad m) => LiftedMonad m (CPS (m a)) where
		{
		callWithMap	:: ((CPS (m a) c -> m c) -> m b) -> CPS (m a) b;
		callWithMap f = MkCPS (\cont -> do
			{
			b <- f (\(MkCPS cps) -> ?);
			cont b;
			});
		};
--}
	instance (Error ex) => MonadCont (CPS ex p) where
		{
		callCC foo = MkCPS (\fcont cont -> unCPS (foo (\a -> MkCPS (\_ _ -> (cont a)))) fcont cont);
		};
	
	runCPS :: (ex -> p) -> (a -> p) -> CPS ex p a -> p;
	runCPS fcont cont cps = unCPS cps fcont cont;
	
	doCPS :: CPS ex p p -> p;
	doCPS = runCPS (const undefined) id;
	
	doMonadCPS :: (Monad m) => (ex -> m String) -> CPS ex (m a) a -> m a;
	doMonadCPS reportError = runCPS (\ex -> (reportError ex) >>= fail) return;

{--
	import IOExts;

	test1 = doCPS (peirceM (\continuation -> do
		{
		continuation 1;
		continuation 2;
		continuation 3;
		})); -- returns 1
	
	test2 :: CPS (IO ()) ();
	test2 = do
		{
		liftM (putStr "aa\n");
		ref <- liftM (newIORef Nothing);
		liftM (putStr "bb\n");
		flag <- peirceM (\c -> do
			{
			liftM (putStr "cc\n");
			liftM (writeIORef ref (Just c));
			liftM (putStr "dd\n");
			c True;
			liftM (putStr "ee\n");
			return False;
			});
		liftM (putStr "ff\n");
		if (flag) then do
			{
			liftM (putStr "gg\n");
			Just c' <- liftM (readIORef ref);
			liftM (putStr "hh\n");
			c' True;
			liftM (putStr "ii\n");
			}
		 else do
			{
			liftM (putStr "jj\n");
			}
		};
--}
	}
