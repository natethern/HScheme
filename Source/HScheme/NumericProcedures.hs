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

module Org.Org.Semantic.HScheme.NumericProcedures where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Numerics;
	import Org.Org.Semantic.HBase;

	isNumberP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isNumberP (NumberObject _,()) = return True;
	isNumberP (_,()) = return False;

	isExactP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isExactP (NumberObject n,()) = return (isExactN n);
	isExactP (_,()) = return False;

	isInexactP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isInexactP (NumberObject n,()) = return (not (isExactN n));
	isInexactP (_,()) = return False;

	realPartP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number,()) -> m Number;
	realPartP (n,()) = return ((realPart n) :+ 0);

	imagPartP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number,()) -> m Number;
	imagPartP (n,()) = return ((imagPart n) :+ 0);

	isZeroP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number,()) -> m Bool;
	isZeroP (n,()) = return (n == 0);

	unaryP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number -> Number) ->
	 (Number,()) -> m Number;
	unaryP op (a,()) = return (op a);

	binaryP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number -> Number -> Number) ->
	 (Number,(Number,())) -> m Number;
	binaryP op (a,(b,())) = return (op a b);

	foldingLP :: (Scheme m r,?refType :: Type (r ())) =>
	 (a -> Number -> a) ->
	 a ->
	 [Number] -> m a;
	foldingLP op a ns = return (foldl op a ns);

	inverterFoldingLP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number -> Number -> Number) ->
	 Number ->
	 (Number,[Number]) -> m Number;
	inverterFoldingLP op a (n,[]) = return (op a n);
	inverterFoldingLP op _ (n,ns) = return (foldl op n ns);
	
	subtractP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number,[Number]) -> m Number;
	subtractP = inverterFoldingLP (-) 0;
{--	
	divideP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Number,[Number]) -> m Number;
	divideP = inverterFoldingLP (/) 1;
--}	}
