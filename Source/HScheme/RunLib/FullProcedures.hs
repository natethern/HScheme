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

module Org.Org.Semantic.HScheme.RunLib.FullProcedures where
	{
	import Org.Org.Semantic.HScheme.RunLib.Procedures;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	-- 6.3.2 Pairs and Lists
	setCarPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,(Object r m,())) -> m ();
	setCarPN ((PairObject carLoc _),(obj,_)) = set carLoc obj;
	setCarPN (p,(obj,_)) = throwArgError "wrong-type-arg" [p];

	setCdrPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,(Object r m,())) -> m ();
	setCdrPN ((PairObject _ cdrLoc),(obj,_)) = set cdrLoc obj;
	setCdrPN (p,(obj,())) = throwArgError "wrong-type-arg" [p];

	-- 6.3.5 Strings
	arraySetPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r a,(Integer,(a,()))) -> m ();
	arraySetPN (arr,(i,(a,_))) = do
		{
		r <- getArrayRef i arr;
		set r a;
		};

	stringSetPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Char,(Integer,(Char,()))) -> m ();
	stringSetPN = arraySetPN;

	-- 6.3.5 Strings
	byteArraySetPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Word8,(Integer,(Word8,()))) -> m ();
	byteArraySetPN = arraySetPN;

	-- 6.3.6 Vectors
	vectorSetPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r (Object r m),(Integer,(Object r m,()))) -> m ();
	vectorSetPN = arraySetPN;

	vectorFillPN :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r (Object r m),(Object r m,())) -> m ();
	vectorFillPN (arr,(obj,_)) = forDo (\r -> set r obj) (toList arr);
	}
