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
	setCarP :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,(Object r m,())) -> m NullObjType;
	setCarP ((PairObject carLoc _),(obj,())) = do
		{
		set carLoc obj;
		return MkNullObjType;
		};
	setCarP (p,(obj,())) = throwArgError "wrong-type-arg" [p];

	setCdrP :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,(Object r m,())) -> m NullObjType;
	setCdrP ((PairObject _ cdrLoc),(obj,())) = do
		{
		set cdrLoc obj;
		return MkNullObjType;
		};
	setCdrP (p,(obj,())) = throwArgError "wrong-type-arg" [p];

	-- 6.3.5 Strings
	stringSetP :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Char,(Integer,(Char,()))) -> m NullObjType;
	stringSetP (arr,(i,(c,()))) = do
		{
		r <- getArrayRef i arr;
		set r c;
		return MkNullObjType;
		};

	-- 6.3.5 Strings
	byteArraySetP :: (FullScheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Word8,(Integer,(Word8,()))) -> m NullObjType;
	byteArraySetP (arr,(i,(c,()))) = do
		{
		r <- getArrayRef i arr;
		set r c;
		return MkNullObjType;
		};
	}
