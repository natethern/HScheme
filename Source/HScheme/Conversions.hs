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

module Org.Org.Semantic.HScheme.Conversions where
	{
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HScheme.Numerics;
	import Org.Org.Semantic.HBase;

	
	-- NullObjType

	data NullObjType = MkNullObjType;

	instance (Scheme m r) => MonadIsA m (Object r m) NullObjType where
		{
		getConvert MkNullObjType = return nullObject;
		};

	instance (Scheme m r) => MonadMaybeA m NullObjType (Object r m) where
		{
		getMaybeConvert obj | isNullObject obj = return (Just MkNullObjType);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) NullObjType;


	-- Either

	instance
		(
		Scheme m r,
		MonadIsA m (Object r m) a,
		MonadIsA m (Object r m) b
		) => MonadIsA m (Object r m) (Either a b) where
		{
		getConvert (Left a) = getConvert a;
		getConvert (Right b) = getConvert b;
		};

	instance
		(
		Scheme m r,
		MonadMaybeA m a (Object r m),
		MonadMaybeA m b (Object r m)
		) => MonadMaybeA m (Either a b) (Object r m) where
		{
		getMaybeConvert obj = do
			{
			ma <- getMaybeConvert obj;
			case ma of
				{
				Just a -> return (return (Left a));
				Nothing -> do
					{
					mb <- getMaybeConvert obj;
					return (do
						{
						b <- mb;
						return (Right b);
						});
					};
				};
			};
		};
	
	instance
		(
		Scheme m r,
		MonadSubtype m (Object r m) a,
		MonadSubtype m (Object r m) b
		) =>
	 MonadSubtype m (Object r m) (Either a b);

	
	-- NilType

	type NilType = ();

	instance (Scheme m r) => MonadIsA m (Object r m) NilType where
		{
		getConvert () = return NilObject;
		};

	instance (Scheme m r) => MonadMaybeA m NilType (Object r m) where
		{
		getMaybeConvert NilObject = return (Just ());
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) NilType;

	
	-- Maybe a

	instance
		(
		Scheme m r,
		MonadIsA m (Object r m) a
		) =>
	 MonadIsA m (Object r m) (Maybe a) where
		{
		getConvert Nothing = getConvert ();
		getConvert (Just a) = getConvert (a,());
		};

	instance
		(
		Scheme m r,
		MonadMaybeA m a (Object r m)
		) =>
	 MonadMaybeA m (Maybe a) (Object r m) where
		{
		getMaybeConvert (PairObject hloc tloc) = do
			{
			mh <- getMaybeConvert (PairObject hloc tloc);
			return (do
				{
				(h,()) <- mh;
				return (Just h);
				});
			};
		getMaybeConvert NilObject = return (Just Nothing);
		getMaybeConvert _ = return Nothing;
		};
	
	instance
		(
		Scheme m r,
		MonadSubtype m (Object r m) a
		) =>
	 MonadSubtype m (Object r m) (Maybe a);

	
	-- []

	instance
		(
		Scheme m r,
		MonadIsA m (Object r m) a
		) =>
	 MonadIsA m (Object r m) [a] where
		{
		getConvert [] = getConvert ();
		getConvert (ah:at) = getConvert (ah,at);
		};

	instance
		(
		Scheme m r,
		MonadMaybeA m a (Object r m)
		) =>
	 MonadMaybeA m [a] (Object r m) where
		{
		getMaybeConvert (PairObject hloc tloc) = do
			{
			mht <- getMaybeConvert (PairObject hloc tloc);
			return (do
				{
				(h,t) <- mht;
				return (h:t);
				});
			};
		getMaybeConvert NilObject = return (Just []);
		getMaybeConvert _ = return Nothing;
		};
	
	instance
		(
		Scheme m r,
		MonadSubtype m (Object r m) a
		) =>
	 MonadSubtype m (Object r m) [a];

	
	-- PairType

	type PairType = (,);

	instance
		(
		Scheme m r,
		MonadIsA m (Object r m) ah,
		MonadIsA m (Object r m) at
		) =>
	 MonadIsA m (Object r m) (PairType ah at) where
		{
		getConvert (ah,at) = do
			{
			objH <- getConvert ah;
			objT <- getConvert at;
			cons objH objT;
			};
		};
{--
	swapMonadMaybe :: (Monad m) =>
	 Maybe (m a) -> m (Maybe a);
	swapMonadMaybe Nothing = return Nothing;
	swapMonadMaybe (Just ma) = do
		{
		a <- ma;
		return (Just a);
		};
--}
	instance
		(
		Scheme m r,
		MonadMaybeA m ah (Object r m),
		MonadMaybeA m at (Object r m)
		) =>
	 MonadMaybeA m (PairType ah at) (Object r m) where
		{
		getMaybeConvert (PairObject hloc tloc) = do
			{
			h <- get hloc;
			t <- get tloc;
			mobjH <- getMaybeConvert h;
			mobjT <- getMaybeConvert t;
			return (do
				{	-- this one's in Maybe. Clever, huh?
				objH <- mobjH;
				objT <- mobjT;
				return (objH,objT);
				});
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance
		(
		Scheme m r,
		MonadSubtype m (Object r m) ah,
		MonadSubtype m (Object r m) at
		) =>
	 MonadSubtype m (Object r m) (PairType ah at);

	
	-- Object

	instance (Scheme m r) => MonadMaybeA m (Object r m) (Object r m) where
		{
		getMaybeConvert = return . Just;
		};

	instance (Scheme m r) => MonadIsA m (Object r m) (Object r m) where
		{
		getConvert = return;
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) (Object r m);

	
	-- Bool

	instance (Scheme m r) => MonadIsA m (Object r m) Bool where
		{
		getConvert = return . BooleanObject;
		};

	instance (Scheme m r) => MonadIsA m Bool (Object r m) where
		{
		getConvert (BooleanObject a) = return a;
		
		-- everything apart from #f is True
		getConvert _ = return True;
		};

	instance (Scheme m r) => MonadMaybeA m Bool (Object r m) where
		{
		getMaybeConvert obj = do
			{
			result <- getConvert obj;
			return (Just result);
			};
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) Bool;

	
	-- Char

	instance (Scheme m r) => MonadIsA m (Object r m) Char where
		{
		getConvert = return . CharObject;
		};

	instance (Scheme m r) => MonadMaybeA m Char (Object r m) where
		{
		getMaybeConvert (CharObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) Char;

	
	-- Number

	instance (Scheme m r) => MonadIsA m (Object r m) Number where
		{
		getConvert = return . NumberObject;
		};

	instance (Scheme m r) => MonadMaybeA m Number (Object r m) where
		{
		getMaybeConvert (NumberObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) Number;


	-- Integer

	instance (Scheme m r) => MonadIsA m (Object r m) Integer where
		{
		getConvert i = getConvert (convert i :: Number);
		};

	instance (Scheme m r) => MonadMaybeA m Integer (Object r m) where
		{
		getMaybeConvert obj = do
			{
			mn <- getMaybeConvert obj;
			return (do
				{
				(n :: Number) <- mn;
				maybeConvert n;
				});
			};
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) Integer;


	-- Rational

	instance (Scheme m r) => MonadIsA m (Object r m) Rational where
		{
		getConvert i = getConvert (convert i :: Number);
		};

	instance (Scheme m r) => MonadMaybeA m Rational (Object r m) where
		{
		getMaybeConvert obj = do
			{
			mn <- getMaybeConvert obj;
			return (do
				{
				(n :: Number) <- mn;
				maybeConvert n;
				});
			};
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) Rational;


	-- Int

	instance (Scheme m r) => MonadIsA m (Object r m) Int where
		{
		getConvert i = getConvert (convert (convertFromInt i :: Integer) :: Number);
		};
{--
	instance (Scheme m r) => MonadMaybeA m Int (Object r m) where
		{
		getMaybeConvert obj = do
			{
			mn <- getMaybeConvert obj;
			return (do
				{
				(n :: Number) <- mn;
				maybeConvert n;
				});
			};
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) Int;
--}

	-- Word8

	instance (Scheme m r) => MonadIsA m (Object r m) Word8 where
		{
		getConvert i = getConvert (convert (convert i :: Integer) :: Number);
		};

	instance (Scheme m r) => MonadMaybeA m Word8 (Object r m) where
		{
		getMaybeConvert obj = do
			{
			mn <- getMaybeConvert obj;
			return (do
				{
				(n :: Number) <- mn;
				(i :: Integer) <- maybeConvert n;
				maybeConvert i;
				});
			};
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) Word8;

	
	-- Symbol

	instance (Scheme m r) => MonadIsA m (Object r m) Symbol where
		{
		getConvert = return . SymbolObject;
		};

	instance (Scheme m r) => MonadMaybeA m Symbol (Object r m) where
		{
		getMaybeConvert (SymbolObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) Symbol;

	
	-- Bindings

	instance (Scheme m r) => MonadIsA m (Object r m) (Bindings r m) where
		{
		getConvert = return . BindingsObject;
		};

	instance (Scheme m r) => MonadMaybeA m (Bindings r m) (Object r m) where
		{
		getMaybeConvert (BindingsObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (Bindings r m);

	
	-- InputPort

	instance (Scheme m r) => MonadIsA m (Object r m) (InputPort Word8 m) where
		{
		getConvert = return . InputPortObject;
		};

	instance (Scheme m r) => MonadMaybeA m (InputPort Word8 m) (Object r m) where
		{
		getMaybeConvert (InputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (InputPort Word8 m);

	
	-- OutputPort

	instance (Scheme m r) => MonadIsA m (Object r m) (OutputPort Word8 m) where
		{
		getConvert = return . OutputPortObject;
		};

	instance (Scheme m r) => MonadMaybeA m (OutputPort Word8 m) (Object r m) where
		{
		getMaybeConvert (OutputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (OutputPort Word8 m);


	-- SRefArray r Word8

	instance (Scheme m r) => MonadIsA m (Object r m) (SRefArray r Word8) where
		{
		getConvert rs = return (ByteArrayObject rs);
		};

	instance (Scheme m r) => MonadMaybeA m (SRefArray r Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject rs) = return (Just rs);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (SRefArray r Word8);


	-- SRefList r Word8

	newtype SRefList r a = MkSRefList {unSRefList :: [r a]};

	instance (Scheme m r) => MonadIsA m (Object r m) (SRefList r Word8) where
		{
		getConvert (MkSRefList rs) = return (ByteArrayObject (fromList rs));
		};

	instance (Scheme m r) => MonadMaybeA m (SRefList r Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject arr) = return (Just (MkSRefList (toList arr)));
		getMaybeConvert _ = return Nothing;
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) (SRefList r Word8);


	-- SList Word8

	getSRefArrayList :: (Scheme m r) =>
	 SRefArray r a -> m [a];
	getSRefArrayList rarray = readRList (toList rarray) where
		{
		readRList [] = return [];
		readRList (r:rs) = do
			{
			c <- get r;
			cs <- readRList rs;
			return (c:cs);
			};
		};

	makeSRefArray :: (Scheme m r) =>
	 [a] -> m (SRefArray r a);
	makeSRefArray list = do
		{
		rlist <- getRList list;
		return (fromList rlist);
		} where
		{
		getRList [] = return [];
		getRList (c:cs) = do
			{
			r <- new c;
			rs <- getRList cs;
			return (r:rs);
			};
		};

	newtype SList a = MkSList {unSList :: [a]};

	instance (Scheme m r) => MonadIsA m (Object r m) (SList Word8) where
		{
		getConvert (MkSList list) = do
			{
			arr <- makeSRefArray list;
			return (ByteArrayObject arr);
			};
		};

	instance (Scheme m r) => MonadMaybeA m (SList Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject rarray) = do
			{
			slist <- getSRefArrayList rarray;
			return (Just (MkSList slist));
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (SList Word8);


	-- SRefArray r Char

	instance (Scheme m r) => MonadIsA m (Object r m) (SRefArray r Char) where
		{
		getConvert rs = return (StringObject rs);
		};

	instance (Scheme m r) => MonadMaybeA m (SRefArray r Char) (Object r m) where
		{
		getMaybeConvert (StringObject rs) = return (Just rs);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (SRefArray r Char);


	-- SRefList r Char

	type StringRefType r = SRefList r Char;

	instance (Scheme m r) => MonadIsA m (Object r m) (SRefList r Char) where
		{
		getConvert (MkSRefList rs) = return (StringObject (fromList rs));
		};

	instance (Scheme m r) => MonadMaybeA m (SRefList r Char) (Object r m) where
		{
		getMaybeConvert (StringObject arr) = return (Just (MkSRefList (toList arr)));
		getMaybeConvert _ = return Nothing;
		};

	instance (Scheme m r) => MonadSubtype m (Object r m) (SRefList r Char);


	-- SList Char

	type StringType = SList Char;

	instance (Scheme m r) => MonadIsA m (Object r m) (SList Char) where
		{
		getConvert (MkSList list) = do
			{
			arr <- makeSRefArray list;
			return (StringObject arr);
			};
		};

	instance (Scheme m r) => MonadMaybeA m (SList Char) (Object r m) where
		{
		getMaybeConvert (StringObject rarray) = do
			{
			slist <- getSRefArrayList rarray;
			return (Just (MkSList slist));
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (SList Char);

	
	-- Procedure

	instance (Scheme m r) => MonadIsA m (Object r m) (Procedure r m) where
		{
		getConvert = return . ProcedureObject;
		};

	instance (Scheme m r) => MonadMaybeA m (Procedure r m) (Object r m) where
		{
		getMaybeConvert (ProcedureObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (Procedure r m);

	
	-- Syntax

	instance (Scheme m r) => MonadIsA m (Object r m) (Syntax r m) where
		{
		getConvert = return . SyntaxObject;
		};

	instance (Scheme m r) => MonadMaybeA m (Syntax r m) (Object r m) where
		{
		getMaybeConvert (SyntaxObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme m r) => MonadSubtype m (Object r m) (Syntax r m);
	}
