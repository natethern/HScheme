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
	import Org.Org.Semantic.HScheme.Parse;
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
	isEOFObjectP :: (Build m r,ObjectSubtype r obj EOFObjType,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isEOFObjectP (obj,()) = getObjectIs (MkType :: Type EOFObjType) obj;
	
	portReadByteP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either EOFObjType Word8);
	portReadByteP (port,()) = do
		{
		mb <- ipRead port;
		return (case mb of
			{
			Nothing -> Left MkEOFObjType;
			Just b -> Right b;
			});
		};
	
	portReadCharLatin1P :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either EOFObjType Char);
	portReadCharLatin1P (port,()) = do
		{
		mb <- ipRead port;
		return (case mb of
			{
			Nothing -> Left MkEOFObjType;
			Just b -> Right (decodeCharLatin1 b);
			});
		};
	
	portReadCharUTF8P :: (ParserError m obj,?objType :: Type obj) =>
	 (InputPort Word8 m,()) -> m (Either EOFObjType Char);
	portReadCharUTF8P (port,()) = do
		{
		mc <- parseUTF8Char (ipRead port);
		return (case mc of
			{
			Nothing -> Left MkEOFObjType;
			Just c -> Right c;
			});
		};
	
	portPeekByteP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either EOFObjType Word8);
	portPeekByteP (port,()) = do
		{
		mb <- ipPeek port;
		return (case mb of
			{
			Nothing -> Left MkEOFObjType;
			Just b -> Right b;
			});
		};
	
	portPeekCharLatin1P :: (Monad m) =>
	 (InputPort Word8 m,()) -> m (Either EOFObjType Char);
	portPeekCharLatin1P (port,()) = do
		{
		mb <- ipPeek port;
		return (case mb of
			{
			Nothing -> Left MkEOFObjType;
			Just b -> Right (decodeCharLatin1 b);
			});
		};
	
	portByteReadyP :: (Monad m) =>
	 (InputPort Word8 m,()) -> m Bool;
	portByteReadyP (port,()) = ipReady port;

	-- 6.6.3 Output
	portWriteBytePN :: (Monad m) =>
	 (Word8,(OutputPort Word8 m,())) -> m ();
	portWriteBytePN (b,(port,())) = opWriteOne port b;

	portWriteByteArrayPN :: (Monad m) =>
	 (SList Word8,(OutputPort Word8 m,())) -> m ();
	portWriteByteArrayPN (MkSList ba,(port,())) = forDo (opWriteOne port) ba;

	portWriteCharLatin1PN :: (Monad m) =>
	 (Char,(OutputPort Word8 m,())) -> m ();
	portWriteCharLatin1PN (c,(port,())) = opWriteOne port (encodeCharLatin1 c);

	portWriteCharUTF8PN :: (Monad m) =>
	 (Char,(OutputPort Word8 m,())) -> m ();
	portWriteCharUTF8PN (c,(port,())) = forDo (opWriteOne port) (encodeSingleUTF8 c);

	portWriteStringLatin1PN :: (Monad m) =>
	 (SList Char,(OutputPort Word8 m,())) -> m ();
	portWriteStringLatin1PN (MkSList s,(port,())) = forDo (opWriteOne port) (encodeLatin1 s);

	portWriteStringUTF8PN :: (Monad m) =>
	 (SList Char,(OutputPort Word8 m,())) -> m ();
	portWriteStringUTF8PN (MkSList s,(port,())) = forDo (opWriteOne port) (encodeUTF8 s);
	}
