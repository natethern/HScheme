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
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HScheme.Core.Port;
	import Org.Org.Semantic.HScheme.Core.Numerics;
	import Org.Org.Semantic.HBase;


	class (SchemeObject r obj) =>
	 ObjectSubtype r obj a | obj -> r where
		{
		getObject :: forall cm. (Build cm r) => a -> cm obj;
		fromObject :: forall cm. (Build cm r) => obj -> cm (Maybe a);
		
		getObjectIs :: forall cm. (Build cm r) => Type a -> obj -> cm Bool;
		getObjectIs _ obj = do
			{
			(ma :: Maybe a) <- fromObject obj;
			return (isJust ma);
			};
		};

	
	-- NullObjType

	data NullObjType = MkNullObjType;

	instance MaybeA NullObjType (Object r m) where
		{
		maybeConvert obj = if isNullObject obj
		 then Just MkNullObjType
		 else Nothing;
		};

	instance ObjectSubtype r (Object r m) NullObjType where
		{
		getObject MkNullObjType = return nullObject;
		fromObject = return . maybeConvert;
		};


	-- Either

	instance
		(
		ObjectSubtype r obj a,
		ObjectSubtype r obj b
		) =>
	 ObjectSubtype r obj (Either a b) where
		{
		getObject (Left a) = getObject a;
		getObject (Right b) = getObject b;
		fromObject obj = do
			{
			ma <- fromObject obj;
			case ma of
				{
				Just a -> return (return (Left a));
				Nothing -> do
					{
					mb <- fromObject obj;
					return (do
						{
						b <- mb;
						return (Right b);
						});
					};
				};
			};
		};

	
	-- NilType

	type NilType = ();

	instance (SchemeObject r obj) =>
	 ObjectSubtype r obj NilType where
		{
		getObject () = return nilObject;
		fromObject obj | Just Nothing <- objectCell obj = return (Just ());
		fromObject _ = return Nothing;
		};

	
	-- Maybe a

	instance (ObjectSubtype r obj a) =>
	 ObjectSubtype r obj (Maybe a) where
		{
		getObject Nothing = getObject ();
		getObject (Just a) = getObject (a,());

		fromObject obj = case objectCell obj of
			{
			Just (Just (hloc,tloc)) -> do
				{
				mh <- fromObject obj;
				return (do
					{
					(h,()) <- mh;
					return (Just h);
					});
				};
			Just Nothing -> return (Just Nothing);
			Nothing ->  return Nothing;
			};
		};

	
	-- []

	instance (ObjectSubtype r obj a) =>
	 ObjectSubtype r obj [a] where
		{
		getObject [] = getObject ();
		getObject (ah:at) = getObject (ah,at);

		fromObject obj = case objectCell obj of
			{
			Just (Just (hloc,tloc)) -> do
				{
				mht <- fromObject obj;
				return (do
					{
					(h,t) <- mht;
					return (h:t);
					});
				};
			Just Nothing -> return (Just []);
			Nothing ->  return Nothing;
			};
		};

	
	-- PairType

	type PairType = (,);

	instance
		(
		ObjectSubtype r obj ah,
		ObjectSubtype r obj at
		) =>
	 ObjectSubtype r obj (PairType ah at) where
		{
		getObject (ah,at) = do
			{
			objH <- getObject ah;
			objT <- getObject at;
			cons objH objT;
			};
		fromObject obj | Just (Just (hloc,tloc)) <- objectCell obj = do
			{
			h <- get hloc;
			t <- get tloc;
			mobjH <- fromObject h;
			mobjT <- fromObject t;
			return (do
				{	-- this one's in Maybe. Clever, huh?
				objH <- mobjH;
				objT <- mobjT;
				return (objH,objT);
				});
			};
		fromObject _ = return Nothing;
		};

	
	-- Object

	instance ObjectSubtype r (Object r m) (Object r m) where
		{
		getObject = return;
		fromObject = return . Just;
		};

	
	-- Bool

	instance MaybeA Bool (Object r m) where
		{
		maybeConvert = Just . convert;
		};

	instance IsA Bool (Object r m) where
		{
		convert (BooleanObject a) = a;
		
		-- everything apart from #f is True
		convert _ = True;
		};

	instance ObjectSubtype r (Object r m) Bool where
		{
		getObject = return . BooleanObject;
		fromObject obj = return (Just (convert obj));
		};

	
	-- Char

	instance MaybeA Char (Object r m) where
		{
		maybeConvert (CharObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Char where
		{
		getObject = return . CharObject;
		fromObject = return . maybeConvert;
		};

	
	-- Number

	instance MaybeA Number (Object r m) where
		{
		maybeConvert (NumberObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Number where
		{
		getObject = return . NumberObject;
		fromObject = return . maybeConvert;
		};


	-- EIReal

	instance ObjectSubtype r (Object r m) EIReal where
		{
		getObject i = getObject (convert i :: Number);
		fromObject obj = do
			{
			mn <- fromObject obj;
			return (do
				{
				(n :: Number) <- mn;
				maybeApproximate n;
				});
			};
		};


	-- Rational

	instance ObjectSubtype r (Object r m) Rational where
		{
		getObject i = getObject (convert i :: EIReal);
		fromObject obj = do
			{
			mn <- fromObject obj;
			return (do
				{
				(n :: EIReal) <- mn;
				maybeApproximate n;
				});
			};
		};


	-- Integer

	instance ObjectSubtype r (Object r m) Integer where
		{
		getObject i = getObject (convert i :: EIReal);
		fromObject obj = do
			{
			mn <- fromObject obj;
			return (do
				{
				(n :: EIReal) <- mn;
				maybeApproximate n;
				});
			};
		};


	-- Int

	instance ObjectSubtype r (Object r m) Int where
		{
		getObject i = getObject (convert i :: Integer);
		fromObject obj = do
			{
			mn <- fromObject obj;
			return (do
				{
				(n :: Integer) <- mn;
				maybeConvert n;
				});
			};
		};


	-- Word8

	instance ObjectSubtype r (Object r m) Word8 where
		{
		getObject i = getObject (convert i :: Integer);
		fromObject obj = do
			{
			mn <- fromObject obj;
			return (do
				{
				(n :: Integer) <- mn;
				maybeConvert n;
				});
			};
		};

	
	-- Symbol

	instance MaybeA Symbol (Object r m) where
		{
		maybeConvert (SymbolObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Symbol where
		{
		getObject = return . SymbolObject;
		fromObject = return . maybeConvert;
		};

	
	-- Environment

	instance MaybeA (Environment r (Object r m)) (Object r m) where
		{
		maybeConvert (EnvironmentObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (Environment r (Object r m)) where
		{
		getObject = return . EnvironmentObject;
		fromObject = return . maybeConvert;
		};

	
	-- InputPort

	instance MaybeA (InputPort Word8 m) (Object r m) where
		{
		maybeConvert (InputPortObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (InputPort Word8 m) where
		{
		getObject = return . InputPortObject;
		fromObject = return . maybeConvert;
		};

	
	-- OutputPort

	instance MaybeA (OutputPort Word8 m) (Object r m) where
		{
		maybeConvert (OutputPortObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (OutputPort Word8 m) where
		{
		getObject = return . OutputPortObject;
		fromObject = return . maybeConvert;
		};


	-- SRefArray r Word8

	instance MaybeA (SRefArray r Word8) (Object r m) where
		{
		maybeConvert (ByteArrayObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r Word8) where
		{
		getObject = return . ByteArrayObject;
		fromObject = return . maybeConvert;
		};


	-- SRefArray r Char

	instance MaybeA (SRefArray r Char) (Object r m) where
		{
		maybeConvert (StringObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r Char) where
		{
		getObject = return . StringObject;
		fromObject = return . maybeConvert;
		};


	-- SRefArray r (Object r m)

	instance MaybeA (SRefArray r (Object r m)) (Object r m) where
		{
		maybeConvert (VectorObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r (Object r m)) where
		{
		getObject = return . VectorObject;
		fromObject = return . maybeConvert;
		};


	-- SRefList r t

	newtype SRefList r a = MkSRefList {unSRefList :: [r a]};

	instance (MaybeA (SRefArray r t) (Object r m)) =>
	 MaybeA (SRefList r t) (Object r m) where
		{
		maybeConvert obj = do
			{
			(arr :: SRefArray r t) <- maybeConvert obj;
			return (MkSRefList (toList arr));
			};
		};

	instance (MaybeA (SRefArray r t) (Object r m),ObjectSubtype r (Object r m) (SRefArray r t)) =>
	 ObjectSubtype r (Object r m) (SRefList r t) where
		{
		getObject (MkSRefList rs) = getObject (fromList rs :: SRefArray r t);
		fromObject = return . maybeConvert;
		};


	-- SList t

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

	instance (ObjectSubtype r (Object r m) (SRefArray r t)) =>
	 ObjectSubtype r (Object r m) (SList t) where
		{
		getObject (MkSList list) = do
			{
			(arr :: SRefArray r t) <- makeSRefArray list;
			getObject arr;
			};

		fromObject obj = do
			{
			mrarray <- fromObject obj;
			case mrarray of
				{
				Just (rarray :: SRefArray r t) -> do
					{
					slist <- getSRefArrayList rarray;
					return (Just (MkSList slist));
					};
				Nothing -> return Nothing;
				};
			};
		};

	
	-- Procedure

	instance MaybeA (Procedure (Object r m) m) (Object r m) where
		{
		maybeConvert (ProcedureObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (Procedure (Object r m) m) where
		{
		getObject = return . ProcedureObject;
		fromObject = return . maybeConvert;
		};
	}
