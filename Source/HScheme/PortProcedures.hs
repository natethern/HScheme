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

module Org.Org.Semantic.HScheme.PortProcedures where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	-- 6.6.1 Ports
	isInputPortP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isInputPortP (InputPortObject _,()) = return True;
	isInputPortP (_,()) = return False;

	isOutputPortP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isOutputPortP (OutputPortObject _,()) = return True;
	isOutputPortP (_,()) = return False;

	inputPortCloseP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Char m,()) -> m ArgNoneType;
	inputPortCloseP (port,()) = do
		{
		ipClose port;
		return MkArgNoneType;
		};

	outputPortCloseP :: (Scheme m r,?refType :: Type (r ())) =>
	 (OutputPort Char m,()) -> m ArgNoneType;
	outputPortCloseP (port,()) = do
		{
		opClose port;
		return MkArgNoneType;
		};

	-- 6.6.2 Input
	eofObject :: (Scheme m r) => Object r m;
	eofObject = nullObject;

	isEOFObjectP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isEOFObjectP (obj,()) = return (isNullObject obj);
	
	portReadCharP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Char m,()) -> m (Either ArgNoneType Char);
	portReadCharP (port,()) = do
		{
		mc <- ipRead port;
		return (case mc of
			{
			Nothing -> Left MkArgNoneType;	-- null object, which also happens to be eof object
			Just c -> Right c;
			});
		};
	
	portPeekCharP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Char m,()) -> m (Either ArgNoneType Char);
	portPeekCharP (port,()) = do
		{
		mc <- ipPeek port;
		return (case mc of
			{
			Nothing -> Left MkArgNoneType;	-- null object, which also happens to be eof object
			Just c -> Right c;
			});
		};
	
	portCharReadyP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Char m,()) -> m Bool;
	portCharReadyP (port,()) = ipReady port;

	-- 6.6.3 Output
	portWriteCharP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Char,(OutputPort Char m,())) -> m ArgNoneType;
	portWriteCharP (c,(port,())) = do
		{
		opWriteOne port c;
		return MkArgNoneType;
		};
	}
