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

module Org.Org.Semantic.HScheme.Equality where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Numerics;
	import Org.Org.Semantic.HBase;
	
	-- 6.1 Equivalence Predicates
	sameList	:: (Scheme m r) => 
	 Type (r ()) -> (a -> b -> m Bool) -> [a] -> [b] -> m Bool;
	sameList _ _ [] [] = return True;
	sameList t same (a:as) (b:bs) = do
		{
		s <- same a b;
		if s then (sameList t same as bs) else (return False);
		};
	sameList _ _ _ _ = return False;

	sameSList	:: (Scheme m r) => 
	 Type (r ()) -> (a -> b -> m Bool) -> SList a -> SList b -> m Bool;
	sameSList t same (MkSList a) (MkSList b) = sameList t same a b;

	sameSRefArray	:: (Scheme m r) => 
	 Type (r ()) -> (a -> b -> m Bool) -> SRefArray r a -> SRefArray r b -> m Bool;
	sameSRefArray t same arra arrb = do
		{
		la <- getConvert arra;
		lb <- getConvert arrb;
		sameSList t same la lb;
		};

	sameRefList	:: (Scheme m r) => 
	 (a -> b -> m Bool) -> [r a] -> [r b] -> m Bool;
	sameRefList _ [] [] = return True;
	sameRefList same (a:as) (b:bs) = do
		{
		ac <- get a;
		bc <- get b;
		s <- same ac bc;
		if s then (sameRefList same as bs) else (return False);
		};
	sameRefList _ _ _ = return False;

	equal :: (Scheme m r) => 
	 Type (r ()) -> Object r m -> Object r m -> m Bool;
	equal _ NilObject NilObject = (return True);
	equal _ (BooleanObject a) (BooleanObject b) = return (a == b);
	equal _ (SymbolObject a) (SymbolObject b) = return (a == b);
	equal _ (CharObject a) (CharObject b) = return (a == b);
	equal _ (NumberObject a) (NumberObject b) = return (equalNumber a b);
	equal t (PairObject ah at) (PairObject bh bt) = do
		{
		ahc <- get ah;
		bhc <- get bh;
		s <- equal t ahc bhc;
		if s then do
			{
			atc <- get at;
			btc <- get bt;
			equal t atc btc;
			} else (return False);
		};
	equal t (VectorObject a) (VectorObject b) = sameSRefArray t (equal t) a b;
	equal t (ByteArrayObject a) (ByteArrayObject b) = sameSRefArray t (\a b -> return (a == b)) a b;
	equal t (StringObject a) (StringObject b) = sameSRefArray t (\a b -> return (a == b)) a b;
	equal t (ValuesObject a) (ValuesObject b) = sameList t (equal t) a b;
	equal _ _ _ = return False;

	eqvRefList	:: (FullScheme m r) => 
	 [r a] -> [r a] -> m Bool;
	eqvRefList [] [] = return True;
	eqvRefList (a:as) (b:bs) = do
		{
		s <- getEqualReference a b;
		if s then (eqvRefList as bs) else (return False);
		};
	eqvRefList _ _ = return False;

	eqv :: (FullScheme m r) => 
	 Type (r ()) -> Object r m -> Object r m -> m Bool;
	eqv t (NumberObject a) (NumberObject b) = return (eqvNumber a b);
	eqv t (PairObject ah at) (PairObject bh bt) = do
		{
		hs <- getEqualReference ah bh;
		if hs then (getEqualReference at bt) else (return False);
		};
	eqv t (ByteArrayObject a) (ByteArrayObject b) = eqvRefList (toList a) (toList b);
	eqv t (StringObject a) (StringObject b) = eqvRefList (toList a) (toList b);
	eqv t (VectorObject a) (VectorObject b) = eqvRefList (toList a) (toList b);
	eqv t (ValuesObject a) (ValuesObject b) = sameList t (eqv t) a b;
	eqv t a b = equal t a b;

	eq :: (FullScheme m r) => 
	 Type (r ()) -> Object r m -> Object r m -> m Bool;
	eq _ (NumberObject a) (NumberObject b) = return (eqNumber a b);
	eq t (ValuesObject a) (ValuesObject b) = sameList t (eq t) a b;
	eq t a b = eqv t a b;
	
	eqP ::  (FullScheme m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	eqP t (a,(b,())) = eq t a b;
	
	eqvP ::  (FullScheme m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	eqvP t (a,(b,())) = eqv t a b;
	
	equalP ::  (Scheme m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	equalP t (a,(b,())) = equal t a b;
	}
