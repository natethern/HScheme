-- This is written in Haskell.
{--
Truth -- general data-model framework in Haskell
Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>

This file is part of Truth.

Truth is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Truth is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Truth; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module MoreRef where
	{
	import Ref;
	import LiftedMonad;
	import ST;
	import IOExts;

	remonadRef :: (Monad m1,Monad m2) => (forall a. m1 a -> m2 a) -> Ref m1 b -> Ref m2 b;
	remonadRef map ref = MkRef
		{
		getRef = map (get ref),
		setRef = \a -> map (set ref a)
		};

	liftRef :: (SemiLiftedMonad m1 m2) => Ref m1 a -> Ref m2 a;
	liftRef = remonadRef call;

	class (MonadReference m r) => MonadCreatableReference m r where
		{
		newReference   :: a -> m (r a);
		};

	class (MonadCreatableReference m r) => MonadStandardReference m r | m -> r;

	newStandardReference :: (MonadStandardReference m r) => a -> m (r a);
	newStandardReference = newReference;

	class (MonadReference m r) => MonadEqualReference m r where
		{
		getEqualReference :: r a -> r a -> m Bool;
		};

	newRef :: (MonadStandardReference m r) => a -> m (Ref m a);
	newRef a = do
		{
		r <- newStandardReference a;
		return (toRef r);
		};

	instance MonadReference IO IORef where
		{
		get = readIORef;
		set = writeIORef;
		};

	instance MonadCreatableReference IO IORef where
		{
		newReference = newIORef;
		};

	instance MonadStandardReference IO IORef;

	instance MonadEqualReference IO IORef where
		{
		getEqualReference a b = return (a == b);
		};

	instance MonadReference (ST s) (STRef s) where
		{
		get = readSTRef;
		set = writeSTRef;
		};

	instance MonadCreatableReference (ST s) (STRef s) where
		{
		newReference = newSTRef;
		};

	instance MonadStandardReference (ST s) (STRef s);

	instance MonadEqualReference (ST s) (STRef s) where
		{
		getEqualReference a b = return (a == b);
		};
	}
