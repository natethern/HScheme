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

module Org.Org.Semantic.HScheme.Core.Conversions where
	{
	import Org.Org.Semantic.HScheme.Core.Object;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HScheme.Core.Port;
	import Org.Org.Semantic.HScheme.Core.Numerics;
	import Org.Org.Semantic.HBase;

	
	-- NullObjType

	data NullObjType = MkNullObjType;

	instance (Monad cm) => MonadIsA cm (Object r m) NullObjType where
		{
		getConvert MkNullObjType = return nullObject;
		};

	instance (Monad cm) => MonadMaybeA cm NullObjType (Object r m) where
		{
		getMaybeConvert obj | isNullObject obj = return (Just MkNullObjType);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) NullObjType;


	-- Either

	instance
		(
		MonadIsA cm (Object r m) a,
		MonadIsA cm (Object r m) b
		) => MonadIsA cm (Object r m) (Either a b) where
		{
		getConvert (Left a) = getConvert a;
		getConvert (Right b) = getConvert b;
		};

	instance
		(
		MonadMaybeA cm a (Object r m),
		MonadMaybeA cm b (Object r m)
		) => MonadMaybeA cm (Either a b) (Object r m) where
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
		MonadSubtype cm (Object r m) a,
		MonadSubtype cm (Object r m) b
		) =>
	 MonadSubtype cm (Object r m) (Either a b);

	
	-- NilType

	type NilType = ();

	instance (Monad cm) => MonadIsA cm (Object r m) NilType where
		{
		getConvert () = return NilObject;
		};

	instance (Monad cm) => MonadMaybeA cm NilType (Object r m) where
		{
		getMaybeConvert NilObject = return (Just ());
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) NilType;

	
	-- Maybe a

	instance
		(
		Build cm r,
		MonadIsA cm (Object r m) a
		) =>
	 MonadIsA cm (Object r m) (Maybe a) where
		{
		getConvert Nothing = getConvert ();
		getConvert (Just a) = getConvert (a,());
		};

	instance
		(
		Build cm r,
		MonadMaybeA cm a (Object r m)
		) =>
	 MonadMaybeA cm (Maybe a) (Object r m) where
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
		Build cm r,
		MonadSubtype cm (Object r m) a
		) =>
	 MonadSubtype cm (Object r m) (Maybe a);

	
	-- []

	instance
		(
		Build cm r,
		MonadIsA cm (Object r m) a
		) =>
	 MonadIsA cm (Object r m) [a] where
		{
		getConvert [] = getConvert ();
		getConvert (ah:at) = getConvert (ah,at);
		};

	instance
		(
		Build cm r,
		MonadMaybeA cm a (Object r m)
		) =>
	 MonadMaybeA cm [a] (Object r m) where
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
		Build cm r,
		MonadSubtype cm (Object r m) a
		) =>
	 MonadSubtype cm (Object r m) [a];

	
	-- PairType

	type PairType = (,);

	instance
		(
		Build cm r,
		MonadIsA cm (Object r m) ah,
		MonadIsA cm (Object r m) at
		) =>
	 MonadIsA cm (Object r m) (PairType ah at) where
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
		Build cm r,
		MonadMaybeA cm ah (Object r m),
		MonadMaybeA cm at (Object r m)
		) =>
	 MonadMaybeA cm (PairType ah at) (Object r m) where
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
		Build cm r,
		MonadSubtype cm (Object r m) ah,
		MonadSubtype cm (Object r m) at
		) =>
	 MonadSubtype cm (Object r m) (PairType ah at);

	
	-- Object

	instance (Monad cm) => MonadMaybeA cm (Object r m) (Object r m) where
		{
		getMaybeConvert = return . Just;
		};

	instance (Monad cm) => MonadIsA cm (Object r m) (Object r m) where
		{
		getConvert = return;
		};

	instance (Monad cm) => MonadSubtype cm (Object r m) (Object r m);

	
	-- Bool

	instance (Monad cm) => MonadIsA cm (Object r m) Bool where
		{
		getConvert = return . BooleanObject;
		};

	instance (Monad cm) => MonadIsA cm Bool (Object r m) where
		{
		getConvert (BooleanObject a) = return a;
		
		-- everything apart from #f is True
		getConvert _ = return True;
		};

	instance (Monad cm) => MonadMaybeA cm Bool (Object r m) where
		{
		getMaybeConvert obj = do
			{
			result <- getConvert obj;
			return (Just result);
			};
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) Bool;

	
	-- Char

	instance (Monad cm) => MonadIsA cm (Object r m) Char where
		{
		getConvert = return . CharObject;
		};

	instance (Monad cm) => MonadMaybeA cm Char (Object r m) where
		{
		getMaybeConvert (CharObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) Char;

	
	-- Number

	instance (Monad cm) => MonadIsA cm (Object r m) Number where
		{
		getConvert = return . NumberObject;
		};

	instance (Monad cm) => MonadMaybeA cm Number (Object r m) where
		{
		getMaybeConvert (NumberObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};

	instance (Monad cm) => MonadSubtype cm (Object r m) Number;


	-- Integer

	instance (Monad cm) => MonadIsA cm (Object r m) Integer where
		{
		getConvert i = getConvert (convert i :: Number);
		};

	instance (Monad cm) => MonadMaybeA cm Integer (Object r m) where
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

	instance (Monad cm) => MonadSubtype cm (Object r m) Integer;


	-- Rational

	instance (Monad cm) => MonadIsA cm (Object r m) Rational where
		{
		getConvert i = getConvert (convert i :: Number);
		};

	instance (Monad cm) => MonadMaybeA cm Rational (Object r m) where
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

	instance (Monad cm) => MonadSubtype cm (Object r m) Rational;


	-- Int

	instance (Monad cm) => MonadIsA cm (Object r m) Int where
		{
		getConvert i = getConvert (convert (convertFromInt i :: Integer) :: Number);
		};
{--
	instance (Monad cm) => MonadMaybeA cm Int (Object r m) where
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

	instance (Monad cm) => MonadSubtype cm (Object r m) Int;
--}

	-- Word8

	instance (Monad cm) => MonadIsA cm (Object r m) Word8 where
		{
		getConvert i = getConvert (convert (convert i :: Integer) :: Number);
		};

	instance (Monad cm) => MonadMaybeA cm Word8 (Object r m) where
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

	instance (Monad cm) => MonadSubtype cm (Object r m) Word8;

	
	-- Symbol

	instance (Monad cm) => MonadIsA cm (Object r m) Symbol where
		{
		getConvert = return . SymbolObject;
		};

	instance (Monad cm) => MonadMaybeA cm Symbol (Object r m) where
		{
		getMaybeConvert (SymbolObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) Symbol;

	
	-- Bindings

	instance (Monad cm) => MonadIsA cm (Object r m) (Bindings r m) where
		{
		getConvert = return . BindingsObject;
		};

	instance (Monad cm) => MonadMaybeA cm (Bindings r m) (Object r m) where
		{
		getMaybeConvert (BindingsObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (Bindings r m);

	
	-- InputPort

	instance (Monad cm) => MonadIsA cm (Object r m) (InputPort Word8 m) where
		{
		getConvert = return . InputPortObject;
		};

	instance (Monad cm) => MonadMaybeA cm (InputPort Word8 m) (Object r m) where
		{
		getMaybeConvert (InputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (InputPort Word8 m);

	
	-- OutputPort

	instance (Monad cm) => MonadIsA cm (Object r m) (OutputPort Word8 m) where
		{
		getConvert = return . OutputPortObject;
		};

	instance (Monad cm) => MonadMaybeA cm (OutputPort Word8 m) (Object r m) where
		{
		getMaybeConvert (OutputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (OutputPort Word8 m);


	-- SRefArray r Word8

	instance (Monad cm) => MonadIsA cm (Object r m) (SRefArray r Word8) where
		{
		getConvert rs = return (ByteArrayObject rs);
		};

	instance (Monad cm) => MonadMaybeA cm (SRefArray r Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject rs) = return (Just rs);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (SRefArray r Word8);


	-- SRefList r Word8

	newtype SRefList r a = MkSRefList {unSRefList :: [r a]};

	instance (Monad cm) => MonadIsA cm (Object r m) (SRefList r Word8) where
		{
		getConvert (MkSRefList rs) = return (ByteArrayObject (fromList rs));
		};

	instance (Monad cm) => MonadMaybeA cm (SRefList r Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject arr) = return (Just (MkSRefList (toList arr)));
		getMaybeConvert _ = return Nothing;
		};

	instance (Monad cm) => MonadSubtype cm (Object r m) (SRefList r Word8);


	-- SList Word8

	getSRefArrayList :: (Build cm r) =>
	 SRefArray r a -> cm [a];
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

	makeSRefArray :: (Build cm r) =>
	 [a] -> cm (SRefArray r a);
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

	instance (Build cm r) => MonadIsA cm (Object r m) (SList Word8) where
		{
		getConvert (MkSList list) = do
			{
			arr <- makeSRefArray list;
			return (ByteArrayObject arr);
			};
		};

	instance (Build cm r) => MonadMaybeA cm (SList Word8) (Object r m) where
		{
		getMaybeConvert (ByteArrayObject rarray) = do
			{
			slist <- getSRefArrayList rarray;
			return (Just (MkSList slist));
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Build cm r) => MonadSubtype cm (Object r m) (SList Word8);


	-- SRefArray r Char

	instance (Monad cm) => MonadIsA cm (Object r m) (SRefArray r Char) where
		{
		getConvert rs = return (StringObject rs);
		};

	instance (Monad cm) => MonadMaybeA cm (SRefArray r Char) (Object r m) where
		{
		getMaybeConvert (StringObject rs) = return (Just rs);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (SRefArray r Char);


	-- SRefList r Char

	type StringRefType r = SRefList r Char;

	instance (Monad cm) => MonadIsA cm (Object r m) (SRefList r Char) where
		{
		getConvert (MkSRefList rs) = return (StringObject (fromList rs));
		};

	instance (Monad cm) => MonadMaybeA cm (SRefList r Char) (Object r m) where
		{
		getMaybeConvert (StringObject arr) = return (Just (MkSRefList (toList arr)));
		getMaybeConvert _ = return Nothing;
		};

	instance (Monad cm) => MonadSubtype cm (Object r m) (SRefList r Char);


	-- SList Char

	type StringType = SList Char;

	instance (Build cm r) => MonadIsA cm (Object r m) (SList Char) where
		{
		getConvert (MkSList list) = do
			{
			arr <- makeSRefArray list;
			return (StringObject arr);
			};
		};

	instance (Build cm r) => MonadMaybeA cm (SList Char) (Object r m) where
		{
		getMaybeConvert (StringObject rarray) = do
			{
			slist <- getSRefArrayList rarray;
			return (Just (MkSList slist));
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Build cm r) => MonadSubtype cm (Object r m) (SList Char);

	
	-- Procedure

	instance (Monad cm) => MonadIsA cm (Object r m) (Procedure r m) where
		{
		getConvert = return . ProcedureObject;
		};

	instance (Monad cm) => MonadMaybeA cm (Procedure r m) (Object r m) where
		{
		getMaybeConvert (ProcedureObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Monad cm) => MonadSubtype cm (Object r m) (Procedure r m);
	}
