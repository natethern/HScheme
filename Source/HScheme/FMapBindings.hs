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

module Org.Org.Semantic.HScheme.FMapBindings(emptyBindings) where
	{
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	fmapGetBinding ::
	 FiniteMap Symbol a -> Symbol -> Maybe a;
	fmapGetBinding map sym = lookup sym map;

	fmapNewBinding ::
	 FiniteMap Symbol a ->
	 Symbol ->
	 a ->
	 Binds Symbol a;
	fmapNewBinding map sym a = toBindings (addMapEntry (sym,a) map);

	toBindings :: FiniteMap Symbol a -> Binds Symbol a;
	toBindings map = MkBinds
		{
		getBinding = fmapGetBinding map,
		newBinding = fmapNewBinding map
		};

	emptyBindings :: Binds Symbol a;
	emptyBindings = toBindings empty;
	}
