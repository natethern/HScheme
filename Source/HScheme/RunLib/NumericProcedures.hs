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

module Org.Org.Semantic.HScheme.RunLib.NumericProcedures where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	isNumberP :: (ObjectSubtype r obj Number,Build m r,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isNumberP (obj,()) = getObjectIs (MkType :: Type Number) obj;

	isExactP :: (ObjectSubtype r obj Number,Build m r,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isExactP (obj,()) = testObject (return . isExactN) obj;

	isInexactP :: (ObjectSubtype r obj Number,Build m r,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isInexactP (obj,()) = testObject (return . not . isExactN) obj;

	realPartP :: (Monad m,?objType :: Type obj) =>
	 (Number,()) -> m Number;
	realPartP (n,()) = return ((realPart n) :+ 0);

	imagPartP :: (Monad m,?objType :: Type obj) =>
	 (Number,()) -> m Number;
	imagPartP (n,()) = return ((imagPart n) :+ 0);

	isZeroP :: (Monad m,?objType :: Type obj) =>
	 (Number,()) -> m Bool;
	isZeroP (n,()) = return (isZero n);

	rationalizeExact :: EIReal -> EIReal -> Either Bool Rational;
	rationalizeExact x y = case simplestRational (x - y) (x + y) of
		{
		Just r -> Right r;
		Nothing -> Left False;
		};

	mapRight :: (a -> b) -> Either x a -> Either x b;
	mapRight _ (Left x) = Left x;
	mapRight f (Right a) = Right (f a);

	rationalizeExactP :: (Monad m) =>
	 (EIReal,(EIReal,())) -> m (Either Bool Rational);
	rationalizeExactP (x,(y,_)) = return (rationalizeExact x y);

	rationalizeP :: (Monad m) =>
	 (EIReal,(EIReal,())) -> m (Either Bool EIReal);
	rationalizeP (x@(ExactReal _),(y@(ExactReal _),_)) = return (mapRight convert (rationalizeExact x y));
	rationalizeP (x,(y,_)) = return (mapRight (InexactReal . approximate) (rationalizeExact x y));

	eirMod :: EIReal -> EIReal -> EIReal;
	eirMod n d = case maybeModulo d n of
		{
		Just a -> a;
		Nothing -> nan;
		};

	inverterFoldingLP :: (Monad m,?objType :: Type obj) =>
	 (Number -> Number -> Number) ->
	 Number ->
	 (Number,[Number]) -> m Number;
	inverterFoldingLP op a (n,[]) = return (op a n);
	inverterFoldingLP op _ (n,ns) = return (foldl op n ns);

	subtractP :: (Monad m,?objType :: Type obj) =>
	 (Number,[Number]) -> m Number;
	subtractP = inverterFoldingLP (-) 0;
	
	divideP :: (Monad m,?objType :: Type obj) =>
	 (Number,[Number]) -> m Number;
	divideP = inverterFoldingLP (/) 1;
	}
