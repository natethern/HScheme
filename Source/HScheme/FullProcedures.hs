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
	setBang ::
		(
		FullScheme m r,
		?bindings :: Bindings r m
		) =>
	 Symbol -> Object r m -> m ();
	setBang name obj = case (getBinding ?bindings name) of
		{
		Just loc -> set loc obj;
		Nothing -> fail ((unSymbol name)++" not defined");
		};
	
	setBangM ::
		(
		FullScheme m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> (Symbol,(Object r m,())) -> m ArgNoneType;
	setBangM Type (name,(obj,())) = do
		{
		res <- evaluate obj;
		setBang name res;
		return MkArgNoneType;
		};

	-- 6.3.2 Pairs and Lists
	setCarP ::  (FullScheme m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m ArgNoneType;
	setCarP Type ((PairObject carLoc _),(obj,())) = do
		{
		set carLoc obj;
		return MkArgNoneType;
		};	
	setCarP Type (_,(obj,())) = fail "not a pair";	

	setCdrP ::  (FullScheme m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m ArgNoneType;
	setCdrP Type ((PairObject _ cdrLoc),(obj,())) = do
		{
		set cdrLoc obj;
		return MkArgNoneType;
		};	
	setCdrP Type (_,(obj,())) = fail "not a pair";	

	-- 6.3.5 Strings
	getListRef :: (Monad m) => Integer -> [a] -> m a;
	getListRef i _ | i < 0 = fail "array out of range";
	getListRef 0 (a:_) = return a;
	getListRef _ [] = fail "array out of range";
	getListRef i (_:as) = getListRef (i - 1) as;

	stringSetP :: (FullScheme m r) =>
	 Type (r ()) -> (StringRefType r,(Integer,(Char,()))) -> m ArgNoneType;
	stringSetP Type (MkStringRefType rs,(i,(c,()))) = do
		{
		r <- getListRef i rs;
		set r c;
		return MkArgNoneType;
		};
	}
