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

module Org.Org.Semantic.HScheme.Object where
	{
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HScheme.Numerics;
	import Org.Org.Semantic.HBase;

	class
		(
		MonadCreatable m r,
		MonadGettableReference m r
		) =>
	 Scheme m r;

	instance
		(
		MonadCreatable m r,
		MonadGettableReference m r
		) =>
	 Scheme m r;

	class
		(
		Scheme m r,
		MonadFullReference m r,
		MonadEqualReference m r
		) =>
	 FullScheme m r;

	instance
		(
		Scheme m r,
		MonadFullReference m r,
		MonadEqualReference m r
		) =>
	 FullScheme m r;

	type ObjLocation r m = r (Object r m);

	newtype Symbol = MkSymbol {unSymbol :: String} deriving (Ordered,Eq);

	instance Show Symbol where
		{
		show (MkSymbol s) = s;
		};

	data Bindings r m = MkBindings
		{
		newBinding :: Symbol -> ObjLocation r m -> Bindings r m,
		getBinding :: Symbol -> Maybe (ObjLocation r m)
		};

	newObjBinding :: (Scheme m r) => 
	 Bindings r m -> Symbol -> Object r m -> m (ObjLocation r m,Bindings r m);
	newObjBinding bindings sym obj = do
		{
		loc <- new obj;
		return (loc,newBinding bindings sym loc);
		};

	type Procedure r m =
	 Bindings r m -> [Object r m] -> m (Object r m);
	type Macro r m =
	 Bindings r m -> Object r m -> m (Object r m);
	type TopLevelMacro r m =
	 Bindings r m -> Object r m -> m (Bindings r m,Object r m);
	type Syntax r m =
	 [Object r m] -> m (Object r m);

	type SRefArray r a = ArrayList (r a);

	data Object r m =
	 NilObject												|
	 BooleanObject		Bool								|
	 SymbolObject		Symbol								|
	 NumberObject		Number								|
	 CharObject			Char								|
	 ByteArrayObject	(SRefArray r Word8)					|
	 StringObject		(SRefArray r Char)					|
	 ValuesObject		[Object r m]						|
	 PairObject			(ObjLocation r m) (ObjLocation r m)	|
	 VectorObject		(SRefArray r (Object r m))			|
	 InputPortObject 	(InputPort Char m)					|
	 OutputPortObject 	(OutputPort Char m)					|
	 ProcedureObject	(Procedure r m)						|
	 BindingsObject		(Bindings r m)						|
	 SyntaxObject		(Syntax r m)						|
	 MacroObject		(Macro r m)							|
	 TopLevelMacroObject(TopLevelMacro r m)					;

	mkValuesObject :: [Object r m] -> Object r m;
	mkValuesObject [a] = a;
	mkValuesObject a = ValuesObject a;

	nullObject = mkValuesObject [];

	isNullObject :: Object r m -> Bool;
	isNullObject (ValuesObject []) = True;
	isNullObject _ = False;

	cons :: (Scheme m r) =>
	 Object r m -> Object r m -> m (Object r m);
	cons head tail = do
		{
		h <- new head;
		t <- new tail;
		return (PairObject h t);
		};
	}
