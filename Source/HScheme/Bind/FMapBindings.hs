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

module Org.Org.Semantic.HScheme.Bind.FMapBindings(emptyBindings) where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	fmapGetBinding :: (Ordered sym) =>
	 FiniteMap sym a -> sym -> Maybe a;
	fmapGetBinding map sym = lookup sym map;

	fmapNewBinding :: (Ordered sym) =>
	 FiniteMap sym a ->
	 sym ->
	 a ->
	 Bindings sym a;
	fmapNewBinding map sym a = toBindings (addMapEntry (sym,a) map);

	toBindings :: (Ordered sym) =>
	 FiniteMap sym a -> Bindings sym a;
	toBindings map = MkBindings
		{
		getBinding = fmapGetBinding map,
		newBinding = fmapNewBinding map
		};

	emptyBindings :: (Ordered sym) => Bindings sym a;
	emptyBindings = toBindings empty;
	}
