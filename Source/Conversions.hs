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
	import Subtype;
	import Type;
	
	class (Scheme x m r) => ArgumentList x m r a where
		{
		convertFromObjects :: [Object r m] -> m a;
		};
	
	class
		(
		ArgumentList x m r a,
		MonadSubtype m (Object r m) a
		) =>
	 Value x m r a;
	
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
	
	instance (Scheme x m r) => ArgumentList x m r () where
		{
		convertFromObjects [] = return ();
		convertFromObjects _ = fail "too many arguments";
		};
	
	instance 
		(
		Scheme x m r,
		MonadSubtype m (Object r m) a1,
		MonadSubtype m (Object r m) a2
		) =>
		ArgumentList x m r (a1,a2) where
		{
		convertFromObjects [o1,o2] = do
			{
			a1 <- convertFromObject o1;
			a2 <- convertFromObject o2;
			return (a1,a2);
			};
		convertFromObjects [o1] = fail "not enough arguments";
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};
	
	instance 
		(
		Scheme x m r,
		MonadSubtype m (Object r m) a1,
		MonadSubtype m (Object r m) a2
		) =>
		ArgumentList x m r (a1,Maybe a2) where
		{
		convertFromObjects [o1,o2] = do
			{
			a1 <- convertFromObject o1;
			a2 <- convertFromObject o2;
			return (a1,Just a2);
			};
		convertFromObjects [o1] = do
			{
			a1 <- convertFromObject o1;
			return (a1,Nothing);
			};
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};
	
	convertToProcedure ::
		(
		ArgumentList x m r args,
		MonadIsA m (Object r m) ret
		) => ((?bindings :: Bindings r m) => args -> m ret) -> Procedure r m;
	
	convertToProcedure foo bindings arglist = do
		{
		args <- convertFromObjects arglist;
		r <- foo args with {?bindings=bindings;};
		getConvert r;
		};
	
	convertToMacro ::
		(
		ArgumentList x m r args,
		MonadIsA m (Object r m) ret
		) => (args -> m ret) -> [Object r m] -> m (Object r m);
	
	convertToMacro foo arglist = do
		{
		args <- convertFromObjects arglist;
		r <- foo args;
		getConvert r;
		};

	
	-- List

	instance (Scheme x m r) => ArgumentList x m r [Object r m] where
		{
		convertFromObjects = return;
		};

	
	-- Object

	instance (Scheme x m r) => ArgumentList x m r (Object r m) where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

	instance (Scheme x m r) => MonadMaybeA m (Object r m) (Object r m) where
		{
		getMaybeConvert = return . Just;
		};

	instance (Scheme x m r) => MonadIsA m (Object r m) (Object r m) where
		{
		getConvert = return;
		};

	instance (Scheme x m r) => MonadSubtype m (Object r m) (Object r m);

	instance (Scheme x m r) => MonadIsA m (Object r m) () where
		{
		getConvert () = return NilObject;
		};

	
	-- Bool

	instance (Scheme x m r) => ArgumentList x m r Bool where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

	instance (Scheme x m r) => MonadIsA m (Object r m) Bool where
		{
		getConvert = return . BooleanObject;
		};

	instance (Scheme x m r) => MonadMaybeA m Bool (Object r m) where
		{
		getMaybeConvert (BooleanObject a) = return (Just a);
		getMaybeConvert _ = return Nothing;
		};
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) Bool;

	
	-- Char

	instance (Scheme x m r) => ArgumentList x m r Char where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

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

	
	-- Bindings

	instance (Scheme x m r) => ArgumentList x m r (Bindings r m) where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

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

	
	-- String

	instance (Scheme x m r) => ArgumentList x m r String where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

	instance (Scheme x m r) => MonadIsA m (Object r m) String where
		{
		getConvert s = do
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

	instance (Scheme x m r) => MonadMaybeA m String (Object r m) where
		{
		getMaybeConvert (StringObject rlist) = do
			{
			s <- readRList rlist;
			return (Just s);
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
	
	instance (Scheme x m r) => MonadSubtype m (Object r m) String;

	
	-- Procedure

	instance (Scheme x m r) => ArgumentList x m r (Procedure r m) where
		{
		convertFromObjects [obj] = convertFromObject obj;
		convertFromObjects [] = fail "not enough arguments";
		convertFromObjects _ = fail "too many arguments";
		};

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
