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
	 (InputPort Word8 m,()) -> m NullObjType;
	inputPortCloseP (port,()) = do
		{
		ipClose port;
		return MkNullObjType;
		};

	outputPortCloseP :: (Scheme m r,?refType :: Type (r ())) =>
	 (OutputPort Word8 m,()) -> m NullObjType;
	outputPortCloseP (port,()) = do
		{
		opClose port;
		return MkNullObjType;
		};

	-- 6.6.2 Input
	eofObject :: (Scheme m r) => Object r m;
	eofObject = nullObject;

	isEOFObjectP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isEOFObjectP (obj,()) = return (isNullObject obj);
	
	portReadByteP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Word8 m,()) -> m (Either NullObjType Word8);
	portReadByteP (port,()) = do
		{
		mc <- ipRead port;
		return (case mc of
			{
			Nothing -> Left MkNullObjType;	-- null object, which also happens to be eof object
			Just c -> Right c;
			});
		};
	
	portPeekByteP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Word8 m,()) -> m (Either NullObjType Word8);
	portPeekByteP (port,()) = do
		{
		mc <- ipPeek port;
		return (case mc of
			{
			Nothing -> Left MkNullObjType;	-- null object, which also happens to be eof object
			Just c -> Right c;
			});
		};
	
	portByteReadyP :: (Scheme m r,?refType :: Type (r ())) =>
	 (InputPort Word8 m,()) -> m Bool;
	portByteReadyP (port,()) = ipReady port;

	-- 6.6.3 Output
	portWriteByteP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Word8,(OutputPort Word8 m,())) -> m NullObjType;
	portWriteByteP (c,(port,())) = do
		{
		opWriteOne port c;
		return MkNullObjType;
		};

	-- conversion
	handleUTF8Error :: (Scheme m r,?refType :: Type (r ())) =>
	 UTF8Error -> m a;
	handleUTF8Error err = throwSchemeError "bad-utf8-parse" [show err];

	parseUTF8Char :: (Scheme m r,?refType :: Type (r ())) =>
	 m (Maybe Word8) -> m (Maybe Char);
	parseUTF8Char source = runExceptionMonad handleUTF8Error (parseUTF8 source);

	parseUTF8P :: (Scheme m r,?bindings :: Bindings r m,?refType :: Type (r ())) =>
	 (Procedure r m,()) -> m (Either NullObjType Char);
	parseUTF8P (source,()) = do
		{
		mc <- parseUTF8Char (do
			{
			obj <- source ?bindings [];
			meb <- getMaybeConvert obj;
			case meb of
				{
				Just (Right b) -> return (Just (b :: Word8));
				Just (Left MkNullObjType) -> return Nothing;
				Nothing -> throwSimpleError "wrong-type";
				};
			});
		return (case mc of
			{
			Just c -> Right c;
			Nothing -> Left MkNullObjType;
			});
		};
	}
