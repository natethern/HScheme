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

module FullProcedures where
	{
	import Evaluate;
	import Conversions;
	import Object;
	import HBase;

	-- 4.1.6 Assignments
	setBang ::
		(
		FullScheme x m r,
		?bindings :: Bindings r m
		) =>
	 Symbol -> Object r m -> m ();
	setBang name obj = case (getBinding ?bindings name) of
		{
		Just loc -> setLocation loc obj;
		Nothing -> fail ((unSymbol name)++" not defined");
		};
	
	setBangM ::
		(
		FullScheme x m r,
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
	setCarP ::  (FullScheme x m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m ArgNoneType;
	setCarP Type ((PairObject carLoc _),(obj,())) = do
		{
		setLocation carLoc obj;
		return MkArgNoneType;
		};	
	setCarP Type (_,(obj,())) = fail "not a pair";	

	setCdrP ::  (FullScheme x m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m ArgNoneType;
	setCdrP Type ((PairObject _ cdrLoc),(obj,())) = do
		{
		setLocation cdrLoc obj;
		return MkArgNoneType;
		};	
	setCdrP Type (_,(obj,())) = fail "not a pair";	
	}
