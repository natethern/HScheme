-- This is written in Haskell.
{--
JVM-Bridge -- bridge from FP languages and others to the Java VM
Copyright (C) 2001 Ashley Yakeley <ashley@semantic.org>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module LiftedMonad where
	{
	class (Monad l,Monad m) => SemiLiftedMonad l m where
		{
		call	:: l a -> m a;
		};
	
	class (SemiLiftedMonad l m) => LiftedMonad l m where
		{
		callWithMap	:: ((m a -> l a) -> l b) -> m b;
--		callWithMap	:: ((forall a. (m a -> l a)) -> l b) -> m b; need GHC 5.03 for rank 3
		};
{--	
	instance (Monad m) => SemiLiftedMonad m m where
		{
		call = id;
		};

	instance (Monad m) => LiftedMonad m m where
		{
		callWithMap f = f id;
		};
--}	
	instance SemiLiftedMonad IO IO where
		{
		call = id;
		};

	instance LiftedMonad IO IO where
		{
		callWithMap f = f id;
		};
{--	
	testRank3 :: (LiftedMonad IO m) => m a -> m b -> m (a,b);
	testRank3 ma mb = callWithMap (\map -> do
		{
		a <- map ma;
		b <- map mb;
		return (a,b);
		});
--}
	}
