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

module Org.Org.Semantic.HScheme.FullProcedures where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	-- 4.1.6 Assignments
	setBang :: (FullScheme m r,?bindings :: Bindings r m) =>
	 Symbol -> Object r m -> m ();
	setBang name obj = case (getBinding ?bindings name) of
		{
		Just loc -> set loc obj;
		Nothing -> fail ((unSymbol name)++" not defined");
		};
	
	setBangM :: (FullScheme m r,?bindings :: Bindings r m) =>
	 (Symbol,(Object r m,())) -> m ArgNoneType;
	setBangM (name,(obj,())) = do
		{
		res <- evaluate obj;
		setBang name res;
		return MkArgNoneType;
		};

	-- 6.3.2 Pairs and Lists
	setCarP :: (FullScheme m r,?refType :: Type (r ())) =>
	 (Object r m,(Object r m,())) -> m ArgNoneType;
	setCarP ((PairObject carLoc _),(obj,())) = do
		{
		set carLoc obj;
		return MkArgNoneType;
		};	
	setCarP (_,(obj,())) = fail "not a pair";	

	setCdrP :: (FullScheme m r,?refType :: Type (r ())) =>
	 (Object r m,(Object r m,())) -> m ArgNoneType;
	setCdrP ((PairObject _ cdrLoc),(obj,())) = do
		{
		set cdrLoc obj;
		return MkArgNoneType;
		};	
	setCdrP (_,(obj,())) = fail "not a pair";	

	-- 6.3.5 Strings
	getArrayRef :: (Monad m) => Integer -> ArrayList a -> m a;
	getArrayRef i _ | i < 0 = fail "array out of range";
	getArrayRef i arr | i >= convertFromInt (length arr) = fail "array out of range";
	getArrayRef i arr = return (arr !! (convertToInt i));

	stringSetP :: (FullScheme m r,?refType :: Type (r ())) =>
	 (SRefArray r Char,(Integer,(Char,()))) -> m ArgNoneType;
	stringSetP (arr,(i,(c,()))) = do
		{
		r <- getArrayRef i arr;
		set r c;
		return MkArgNoneType;
		};

	-- 6.3.5 Strings
	byteArraySetP :: (FullScheme m r,?refType :: Type (r ())) =>
	 (SRefArray r Word8,(Integer,(Word8,()))) -> m ArgNoneType;
	byteArraySetP (arr,(i,(c,()))) = do
		{
		r <- getArrayRef i arr;
		set r c;
		return MkArgNoneType;
		};
	}
