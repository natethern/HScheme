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

module Conversions where
	{
	import Object;
	import Numerics;
	import Subtype;
	import Type;
	
	class (Scheme x m r) => ArgumentList x m r a where
		{
		convertFromObjects :: [Object r m] -> m a;
		};

	instance (Scheme x m r) => ArgumentList x m r () where
		{
		convertFromObjects [] = return ();
		convertFromObjects (_:_) = fail "too many arguments";
		};
	
	convertFromObject :: (MonadMaybeA m to from) => from -> m to;
	convertFromObject from = do
		{
		mto <- getMaybeConvert from;
		case mto of
			{
			Just to -> return to;
			Nothing -> fail "wrong type for object";
			};
		};

	instance
		(
		MonadMaybeA m a (Object r m),
		ArgumentList x m r b
		) =>
	 ArgumentList x m r (a,b) where
		{
		convertFromObjects [] = fail "too few arguments";
		convertFromObjects (obj:objs) = do
			{
			a <- convertFromObject obj;
			b <- convertFromObjects objs;
			return (a,b);
			};
		};

	instance
		(
		Scheme x m r,
		MonadMaybeA m a (Object r m)
		) =>
	 ArgumentList x m r (Maybe a) where
		{
		convertFromObjects [] = return Nothing;
		convertFromObjects [obj] = do
			{
			a <- convertFromObject obj;
			return (Just a);
			};
		convertFromObjects _ = fail "too few arguments";
		};

	instance
		(
		Scheme x m r,
		MonadMaybeA m a (Object r m)
		) =>
	 ArgumentList x m r [a] where
		{
		convertFromObjects [] = return [];
		convertFromObjects (obj:objs) = do
			{
			a <- convertFromObject obj;
			as <- convertFromObjects objs;
			return (a:as);
			};
		};
	
	convertToProcedure ::
		(
		ArgumentList x m r args,
		MonadIsA m (Object r m) ret
		) =>
	 ((?bindings :: Bindings r m) => args -> m ret) -> Procedure r m;
	
	convertToProcedure foo bindings obj = do
		{
		args <- convertFromObjects obj;
		r <- foo args with {?bindings=bindings;};
		getConvert r;
		};
	
	convertToMacro ::
		(
		Scheme x m r,
		MonadMaybeA m args (Object r m),
		MonadIsA m (Object r m) ret
		) =>
	 ((?bindings :: Bindings r m) => args -> m ret) -> Macro r m;
	
	convertToMacro foo bindings obj = do
		{
		args <- convertFromObject obj;
		r <- foo args with {?bindings=bindings;};
		getConvert r;
		};

	
	-- ArgNoneType

	data ArgNoneType = MkArgNoneType;

	instance (Scheme x m r) => MonadIsA m (Object r m) ArgNoneType where
		{
		getConvert MkArgNoneType = return nullObject;
		};

	
	-- NilType

	type NilType = ();

	instance (Scheme x m r) => MonadIsA m (Object r m) NilType where
		{
		getConvert () = return NilObject;
		};

	instance (Scheme x m r) => MonadMaybeA m NilType (Object r m) where
		{
		getMaybeConvert NilObject = return (Just ());
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) NilType;

	
	-- Maybe a

	instance
		(
		Scheme x m r,
		MonadIsA m (Object r m) a
		) =>
	 MonadIsA m (Object r m) (Maybe a) where
		{
		getConvert Nothing = getConvert ();
		getConvert (Just a) = getConvert (a,());
		};

	instance
		(
		Scheme x m r,
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
		Scheme x m r,
		MonadSubtype m (Object r m) a
		) =>
	 MonadSubtype m (Object r m) (Maybe a);

	
	-- []

	instance
		(
		Scheme x m r,
		MonadIsA m (Object r m) a
		) =>
	 MonadIsA m (Object r m) [a] where
		{
		getConvert [] = getConvert ();
		getConvert (ah:at) = getConvert (ah,at);
		};

	instance
		(
		Scheme x m r,
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
		Scheme x m r,
		MonadSubtype m (Object r m) a
		) =>
	 MonadSubtype m (Object r m) [a];

	
	-- PairType

	type PairType = (,);

	instance
		(
		Scheme x m r,
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
		Scheme x m r,
		MonadMaybeA m ah (Object r m),
		MonadMaybeA m at (Object r m)
		) =>
	 MonadMaybeA m (PairType ah at) (Object r m) where
		{
		getMaybeConvert (PairObject hloc tloc) = do
			{
			h <- getLocation hloc;
			t <- getLocation tloc;
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
		Scheme x m r,
		MonadSubtype m (Object r m) ah,
		MonadSubtype m (Object r m) at
		) =>
	 MonadSubtype m (Object r m) (PairType ah at);

	
	-- Object

	instance (Scheme x m r) => MonadMaybeA m (Object r m) (Object r m) where
		{
		getMaybeConvert = return . Just;
		};

	instance (Scheme x m r) => MonadIsA m (Object r m) (Object r m) where
		{
		getConvert = return;
		};

	instance (Scheme x m r) => MonadSubtype m (Object r m) (Object r m);

	
	-- Bool

	instance (Scheme x m r) => MonadIsA m (Object r m) Bool where
		{
		getConvert = return . BooleanObject;
		};

	instance (Scheme x m r) => MonadIsA m Bool (Object r m) where
		{
		getConvert (BooleanObject a) = return a;
		
		-- everything apart from #f is True
		getConvert _ = return True;
		};

	instance (Scheme x m r) => MonadMaybeA m Bool (Object r m) where
		{
		getMaybeConvert obj = do
			{
			result <- getConvert obj;
			return (Just result);
			};
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) Bool;

	
	-- Char

	instance (Scheme x m r) => MonadIsA m (Object r m) Char where
		{
		getConvert = return . CharObject;
		};

	instance (Scheme x m r) => MonadMaybeA m Char (Object r m) where
		{
		getMaybeConvert (CharObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) Char;

	
	-- Number

	instance (Scheme x m r) => MonadIsA m (Object r m) Number where
		{
		getConvert = return . NumberObject;
		};

	instance (Scheme x m r) => MonadMaybeA m Number (Object r m) where
		{
		getMaybeConvert (NumberObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) Number;

	
	-- Symbol

	instance (Scheme x m r) => MonadIsA m (Object r m) Symbol where
		{
		getConvert = return . SymbolObject;
		};

	instance (Scheme x m r) => MonadMaybeA m Symbol (Object r m) where
		{
		getMaybeConvert (SymbolObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) Symbol;

	
	-- Bindings

	instance (Scheme x m r) => MonadIsA m (Object r m) (Bindings r m) where
		{
		getConvert = return . BindingsObject;
		};

	instance (Scheme x m r) => MonadMaybeA m (Bindings r m) (Object r m) where
		{
		getMaybeConvert (BindingsObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) (Bindings r m);

	
	-- InputPort

	instance (Scheme x m r) => MonadIsA m (Object r m) (InputPort m) where
		{
		getConvert = return . InputPortObject;
		};

	instance (Scheme x m r) => MonadMaybeA m (InputPort m) (Object r m) where
		{
		getMaybeConvert (InputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) (InputPort m);

	
	-- OutputPort

	instance (Scheme x m r) => MonadIsA m (Object r m) (OutputPort m) where
		{
		getConvert = return . OutputPortObject;
		};

	instance (Scheme x m r) => MonadMaybeA m (OutputPort m) (Object r m) where
		{
		getMaybeConvert (OutputPortObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) (OutputPort m);


	-- String

	newtype StringType = MkStringType String;

	instance (Scheme x m r) => MonadIsA m (Object r m) StringType where
		{
		getConvert (MkStringType s) = do
			{
			rlist <- getRList s;
			return (StringObject rlist);
			} where
			{
			getRList [] = return [];
			getRList (c:cs) = do
				{
				r <- newLocation c;
				rs <- getRList cs;
				return (r:rs);
				};
			};
		};

	instance (Scheme x m r) => MonadMaybeA m StringType (Object r m) where
		{
		getMaybeConvert (StringObject rlist) = do
			{
			s <- readRList rlist;
			return (Just (MkStringType s));
			} where
			{
			readRList [] = return [];
			readRList (r:rs) = do
				{
				c <- getLocation r;
				cs <- readRList rs;
				return (c:cs);
				};
			};
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) StringType;

	
	-- Procedure

	instance (Scheme x m r) => MonadIsA m (Object r m) (Procedure r m) where
		{
		getConvert = return . ProcedureObject;
		};

	instance (Scheme x m r) => MonadMaybeA m (Procedure r m) (Object r m) where
		{
		getMaybeConvert (ProcedureObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) (Procedure r m);
	}
