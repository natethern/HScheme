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

module Org.Org.Semantic.HScheme.RunLib.PortProcedures where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	-- 6.6.1 Ports

	isInputPortP :: (Build m r,ObjectSubtype r obj (InputPort Word8 m),?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isInputPortP (obj,()) :: m Bool = getObjectIs (MkType :: Type (InputPort Word8 m)) obj;

	isOutputPortP :: (Build m r,ObjectSubtype r obj (OutputPort Word8 m),?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isOutputPortP (obj,()) :: m Bool = getObjectIs (MkType :: Type (OutputPort Word8 m)) obj;

	inputPortClosePN :: (Monad m) =>
	 (InputPort Word8 m,()) -> m ();
	inputPortClosePN (port,()) = ipClose port;

	outputPortClosePN :: (Monad m) =>
	 (OutputPort Word8 m,()) -> m ();
	outputPortClosePN (port,()) = opClose port;

	-- 6.6.2 Input
	isEOFObjectP :: (Build m r,ObjectSubtype r obj VoidObjType,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isEOFObjectP (obj,()) = getObjectIs (MkType :: Type VoidObjType) obj;
	
	portReadByteP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either VoidObjType Word8);
	portReadByteP (port,()) = do
		{
		mc <- ipRead port;
		return (case mc of
			{
			Nothing -> Left MkVoidObjType;
			Just c -> Right c;
			});
		};
	
	portPeekByteP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either VoidObjType Word8);
	portPeekByteP (port,()) = do
		{
		mc <- ipPeek port;
		return (case mc of
			{
			Nothing -> Left MkVoidObjType;
			Just c -> Right c;
			});
		};
	
	portByteReadyP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m Bool;
	portByteReadyP (port,()) = ipReady port;

	-- 6.6.3 Output
	portWriteByteP :: (Monad m) =>
	 (Word8,(OutputPort Word8 m,())) -> m VoidObjType;
	portWriteByteP (c,(port,())) = do
		{
		opWriteOne port c;
		return MkVoidObjType;
		};
	}
