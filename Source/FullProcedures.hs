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

module FullProcedures where
	{
	import Evaluate;
	import Conversions;
	import Object;
	import Type;

	setBang ::
		(
		FullScheme x m r,
		?bindings :: Bindings r m
		) =>
	 Symbol -> Object r m -> m ();
	setBang name obj = case (getBinding ?bindings name) of
		{
		Just loc -> setLocation loc obj;
		Nothing -> fail ((unSymbol name)++" not defined");
		};
	
	setBangS ::
		(
		FullScheme x m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> (Symbol,(Object r m,())) -> m ArgNoneType;
	setBangS Type (name,(obj,())) = do
		{
		res <- evaluate obj;
		setBang name res;
		return MkArgNoneType;
		};
	}
