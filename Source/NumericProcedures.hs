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

module NumericProcedures where
	{
	import Conversions;
	import Object;
	import Numerics;
	import Type;

	unaryS :: (Scheme x m r) =>
	 (Number -> Number) ->
	 Type (r ()) -> (Number,()) -> m Number;
	unaryS op Type (a,()) = return (op a);

	binaryS :: (Scheme x m r) =>
	 (Number -> Number -> Number) ->
	 Type (r ()) -> (Number,(Number,())) -> m Number;
	binaryS op Type (a,(b,())) = return (op a b);

	foldingLS :: (Scheme x m r) =>
	 (Number -> Number -> Number) ->
	 Number ->
	 Type (r ()) -> [Number] -> m Number;
	foldingLS op a Type ns = return (foldl op a ns);

	inverterFoldingLS :: (Scheme x m r) =>
	 (Number -> Number -> Number) ->
	 Number ->
	 Type (r ()) -> (Number,[Number]) -> m Number;
	inverterFoldingLS op a Type (n,[]) = return (op a n);
	inverterFoldingLS op _ Type (n,ns) = return (foldl op n ns);
	
	subtractS :: (Scheme x m r) =>
	 Type (r ()) -> (Number,[Number]) -> m Number;
	subtractS = inverterFoldingLS (-) 0;
	
	divideS :: (Scheme x m r) =>
	 Type (r ()) -> (Number,[Number]) -> m Number;
	divideS = inverterFoldingLS (/) 1;
	}
