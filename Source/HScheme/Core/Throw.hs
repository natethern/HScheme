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

module Org.Org.Semantic.HScheme.Core.Throw where
	{
	import Org.Org.Semantic.HScheme.Core.Conversions;
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HBase;

	throwSchemeError ::
		(
		Build cm r,
		MonadThrow obj cm,
		?objType :: Type obj,
		ObjectSubtype r obj Symbol,
		ObjectSubtype r obj rest
		) =>
	 String -> rest -> cm a;
	throwSchemeError name errRest = do
		{
		errorObj <- getObject (MkSymbol name,errRest);
		throwObject errorObj;
		};

	throwSimpleError ::
		(
		Build cm r,
		MonadThrow obj cm,
		ObjectSubtype r obj Symbol,
		?objType :: Type obj
		) =>
	 String -> cm a;
	throwSimpleError name = throwSchemeError name ();

	throwArgError ::
		(
		Build cm r,
		MonadThrow obj cm,
		ObjectSubtype r obj Symbol,
		ObjectSubtype r obj obj,
		?objType :: Type obj
		) =>
	 String -> [obj] -> cm a;
	throwArgError = throwSchemeError;
	}
