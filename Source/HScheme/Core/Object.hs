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

module Org.Org.Semantic.HScheme.Core.Object where
	{
	import Org.Org.Semantic.HScheme.Core.Binding;
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HBase;

	type SymbolBindings = Bindings Symbol;

	type SRefArray r a = ArrayList (r a);

	newtype SRefList r a = MkSRefList {unSRefList :: [r a]};

	sameList :: (a -> b -> Bool) -> [a] -> [b] -> Bool;
	sameList _ [] [] = True;
	sameList same (a:as) (b:bs) | same a b = sameList same as bs;
	sameList _ _ _ = False;

	msameList :: (Monad m) => 
	 (a -> b -> m Bool) -> [a] -> [b] -> m Bool;
	msameList _ [] [] = return True;
	msameList same (a:as) (b:bs) = do
		{
		s <- same a b;
		if s then (msameList same as bs) else (return False);
		};
	msameList _ _ _ = return False;

	getSRefArrayList :: (Build cm r) =>
	 SRefArray r a -> cm [a];
	getSRefArrayList rarray = for get (toList rarray);

	makeSRefArray :: (Build cm r) =>
	 [a] -> cm (SRefArray r a);
	makeSRefArray list = fmap fromList (for new list);

	sameSRefArray :: (Build m r) => 
	 (a -> b -> m Bool) -> SRefArray r a -> SRefArray r b -> m Bool;
	sameSRefArray same arra arrb = do
		{
		la <- getSRefArrayList arra;
		lb <- getSRefArrayList arrb;
		msameList same la lb;
		};

	newtype SList a = MkSList {unSList :: [a]};

	data VoidObjType = MkVoidObjType;

	class Eq1 r where
		{
		eq1 :: forall a. (Eq a) => r a -> r a -> Bool;
		};

	instance Eq1 (Constant m) where
		{
		eq1 = (==);
		};

	instance Eq1 IORef where
		{
		eq1 = (==);
		};
	}
