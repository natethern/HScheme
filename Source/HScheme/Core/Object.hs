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

module Org.Org.Semantic.HScheme.Core.Object where
	{
	import Org.Org.Semantic.HScheme.Core.Binding;
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HScheme.Core.Port;
	import Org.Org.Semantic.HScheme.Core.Numerics;
	import Org.Org.Semantic.HBase;

	type SymbolBindings = Bindings Symbol;

	type Syntax cm r m = [Object r m] -> cm (Object r m);

	data Environment r m = MkEnvironment
		{
		envSyn :: SymbolBindings (Syntax m r m),
		envLoc :: SymbolBindings (ObjLocation r m)
		};

	type Procedure r m = ([Object r m] -> m (Object r m));

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
	 InputPortObject 	(InputPort Word8 m)					|
	 OutputPortObject 	(OutputPort Word8 m)				|
	 ProcedureObject	(Procedure r m)						|
	 EnvironmentObject	(Environment r m)					;

	type ObjLocation r m = r (Object r m);

	mkValuesObject :: [Object r m] -> Object r m;
	mkValuesObject [a] = a;
	mkValuesObject a = ValuesObject a;

	nullObject :: Object r m;
	nullObject = mkValuesObject [];

	isNullObject :: Object r m -> Bool;
	isNullObject (ValuesObject []) = True;
	isNullObject _ = False;

	cons :: (Build cm r) =>
	 Object r m -> Object r m -> cm (Object r m);
	cons head tail = do
		{
		h <- new head;
		t <- new tail;
		return (PairObject h t);
		};
	}
