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

module Equality where
	{
	import Object;
	import Numerics;
	import Type;
	
	-- 6.1 Equivalence Predicates
	sameList	:: (Scheme x m r) => 
	 (Object r m -> Object r m -> m Bool) -> [Object r m] -> [Object r m] -> m Bool;
	sameList _ [] [] = return True;
	sameList same (a:as) (b:bs) = do
		{
		s <- same a b;
		if s then (sameList same as bs) else (return False);
		};
	sameList _ _ _ = return False;
	
	sameRefList	:: (Scheme x m r) => 
	 (a -> b -> m Bool) -> [r a] -> [r b] -> m Bool;
	sameRefList _ [] [] = return True;
	sameRefList same (a:as) (b:bs) = do
		{
		ac <- getLocation a;
		bc <- getLocation b;
		s <- same ac bc;
		if s then (sameRefList same as bs) else (return False);
		};
	sameRefList _ _ _ = return False;
	
	equal :: (Scheme x m r) => 
	 Object r m -> Object r m -> m Bool;
	equal NilObject NilObject = (return True);
	equal (BooleanObject a) (BooleanObject b) = return (a == b);
	equal (SymbolObject a) (SymbolObject b) = return (a == b);
	equal (CharObject a) (CharObject b) = return (a == b);
	equal (NumberObject a) (NumberObject b) = return (equalNumber a b);
	equal (PairObject ah at) (PairObject bh bt) = do
		{
		ahc <- getLocation ah;
		bhc <- getLocation bh;
		s <- equal ahc bhc;
		if s then do
			{
			atc <- getLocation at;
			btc <- getLocation bt;
			equal atc btc;
			} else (return False);
		};
	equal (VectorObject a) (VectorObject b) = sameRefList equal a b;
	equal (StringObject a) (StringObject b) = sameRefList (\a b -> return (a == b)) a b;
	equal (ValuesObject a) (ValuesObject b) = sameList equal a b;
	equal _ _ = return False;
	
	eqvRefList	:: (FullScheme x m r) => 
	 [r a] -> [r a] -> m Bool;
	eqvRefList [] [] = return True;
	eqvRefList (a:as) (b:bs) = do
		{
		s <- sameLocation a b;
		if s then (eqvRefList as bs) else (return False);
		};
	eqvRefList _ _ = return False;
	
	eqv :: (FullScheme x m r) => 
	 Object r m -> Object r m -> m Bool;
	eqv (NumberObject a) (NumberObject b) = return (eqvNumber a b);
	eqv (PairObject ah at) (PairObject bh bt) = do
		{
		hs <- sameLocation ah bh;
		if hs then (sameLocation at bt) else (return False);
		};
	eqv (StringObject a) (StringObject b) = eqvRefList a b;
	eqv (VectorObject a) (VectorObject b) = eqvRefList a b;
	eqv (ValuesObject a) (ValuesObject b) = sameList eqv a b;
	eqv a b = equal a b;
	
	eq :: (FullScheme x m r) => 
	 Object r m -> Object r m -> m Bool;
	eq (NumberObject a) (NumberObject b) = return (eqNumber a b);
	eq (ValuesObject a) (ValuesObject b) = sameList eq a b;
	eq a b = eqv a b;
	
	eqP ::  (FullScheme x m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	eqP Type (a,(b,())) = eq a b;
	
	eqvP ::  (FullScheme x m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	eqvP Type (a,(b,())) = eqv a b;
	
	equalP ::  (Scheme x m r) => 
	 Type (r ()) -> (Object r m,(Object r m,())) -> m Bool;
	equalP Type (a,(b,())) = equal a b;
	}
