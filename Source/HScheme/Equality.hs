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
	sameList	:: (Scheme m r,?refType :: Type (r ())) => 
	 (a -> b -> m Bool) -> [a] -> [b] -> m Bool;
	sameList _ [] [] = return True;
	sameList same (a:as) (b:bs) = do
		{
		s <- same a b;
		if s then (sameList same as bs) else (return False);
		};
	sameList _ _ _ = return False;

	sameSList	:: (Scheme m r,?refType :: Type (r ())) => 
	 (a -> b -> m Bool) -> SList a -> SList b -> m Bool;
	sameSList same (MkSList a) (MkSList b) = sameList same a b;

	sameSRefArray	:: (Scheme m r,?refType :: Type (r ())) => 
	 (a -> b -> m Bool) -> SRefArray r a -> SRefArray r b -> m Bool;
	sameSRefArray same arra arrb = do
		{
		la <- getConvert arra;
		lb <- getConvert arrb;
		sameSList same la lb;
		};

	equal :: (Scheme m r,?refType :: Type (r ())) => 
	 Object r m -> Object r m -> m Bool;
	equal NilObject NilObject = (return True);
	equal (BooleanObject a) (BooleanObject b) = return (a == b);
	equal (SymbolObject a) (SymbolObject b) = return (a == b);
	equal (CharObject a) (CharObject b) = return (a == b);
	equal (NumberObject a) (NumberObject b) = return (equalNumber a b);
	equal (PairObject ah at) (PairObject bh bt) = do
		{
		ahc <- get ah;
		bhc <- get bh;
		s <- equal ahc bhc;
		if s then do
			{
			atc <- get at;
			btc <- get bt;
			equal atc btc;
			} else (return False);
		};
	equal (VectorObject a) (VectorObject b) = sameSRefArray equal a b;
	equal (ByteArrayObject a) (ByteArrayObject b) = sameSRefArray (\a b -> return (a == b)) a b;
	equal (StringObject a) (StringObject b) = sameSRefArray (\a b -> return (a == b)) a b;
	equal (ValuesObject a) (ValuesObject b) = sameList equal a b;
	equal _ _ = return False;

	eqvRefList	:: (FullScheme m r,?refType :: Type (r ())) => 
	 [r a] -> [r a] -> m Bool;
	eqvRefList [] [] = return True;
	eqvRefList (a:as) (b:bs) = do
		{
		s <- getEqualReference a b;
		if s then (eqvRefList as bs) else (return False);
		};
	eqvRefList _ _ = return False;

	eqv :: (FullScheme m r,?refType :: Type (r ())) => 
	 Object r m -> Object r m -> m Bool;
	eqv (NumberObject a) (NumberObject b) = return (eqvNumber a b);
	eqv (PairObject ah at) (PairObject bh bt) = do
		{
		hs <- getEqualReference ah bh;
		if hs then (getEqualReference at bt) else (return False);
		};
	eqv (ByteArrayObject a) (ByteArrayObject b) = eqvRefList (toList a) (toList b);
	eqv (StringObject a) (StringObject b) = eqvRefList (toList a) (toList b);
	eqv (VectorObject a) (VectorObject b) = eqvRefList (toList a) (toList b);
	eqv (ValuesObject a) (ValuesObject b) = sameList eqv a b;
	eqv a b = equal a b;

	eq :: (FullScheme m r,?refType :: Type (r ())) => 
	 Object r m -> Object r m -> m Bool;
	eq (NumberObject a) (NumberObject b) = return (eqNumber a b);
	eq (ValuesObject a) (ValuesObject b) = sameList eq a b;
	eq a b = eqv a b;

	eqP ::  (FullScheme m r,?refType :: Type (r ())) => 
	 (Object r m,(Object r m,())) -> m Bool;
	eqP (a,(b,())) = eq a b;

	eqvP ::  (FullScheme m r,?refType :: Type (r ())) => 
	 (Object r m,(Object r m,())) -> m Bool;
	eqvP (a,(b,())) = eqv a b;

	equalP ::  (Scheme m r,?refType :: Type (r ())) => 
	 (Object r m,(Object r m,())) -> m Bool;
	equalP (a,(b,())) = equal a b;
	}
