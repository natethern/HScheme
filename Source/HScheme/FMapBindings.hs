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
	 FiniteMap Symbol (ObjLocation r m) -> Symbol -> Maybe (ObjLocation r m);
	fmapGetBinding map sym = lookup sym map;

	fmapNewBinding :: (Scheme m r) =>
	 FiniteMap Symbol (ObjLocation r m) -> Symbol -> ObjLocation r m ->
	 Bindings r m;
	fmapNewBinding map sym loc = toBindings (addMapEntry (sym,loc) map);

	toBindings :: (Scheme m r) =>
	 FiniteMap Symbol (ObjLocation r m) -> Bindings r m;
	toBindings map = MkBindings
		{
		getBinding = fmapGetBinding map,
		newBinding = fmapNewBinding map
		};

	emptyBindings :: (Scheme m r) => Bindings r m;
	emptyBindings = toBindings empty;
	}
