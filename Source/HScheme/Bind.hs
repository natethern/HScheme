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

module Org.Org.Semantic.HScheme.Bind
	(
	module Org.Org.Semantic.HScheme.Bind.Add,
	module Org.Org.Semantic.HScheme.Bind.Macro,
	module Org.Org.Semantic.HScheme.Bind.TopLevel,
	module Org.Org.Semantic.HScheme.Bind.Run,
	module Org.Org.Semantic.HScheme.Bind.FMapBindings,
	module Org.Org.Semantic.HScheme.Bind
	) where
	{
	import Org.Org.Semantic.HScheme.Bind.FMapBindings;
	import Org.Org.Semantic.HScheme.Bind.Run;
	import Org.Org.Semantic.HScheme.Bind.TopLevel;
	import Org.Org.Semantic.HScheme.Bind.Macro;
	import Org.Org.Semantic.HScheme.Bind.Add;

	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	mutualBind ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 ((
	 	?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
	 	?macrobindings :: SymbolBindings (Macro cm r m),
	 	?syntacticbindings :: SymbolBindings (Syntax r (Object r m))
	 	) => SymbolBindings (Macro cm r m) -> SymbolBindings (Macro cm r m)) ->
	 ((
	 	?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
	 	?macrobindings :: SymbolBindings (Macro cm r m),
	 	?syntacticbindings :: SymbolBindings (Syntax r (Object r m))
	 	) => SymbolBindings (TopLevelMacro cm r m) -> SymbolBindings (TopLevelMacro cm r m)) ->
	 ((?macrobindings :: SymbolBindings (Macro cm r m),
	 	?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
	 	?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m)
	 	) => a) ->
	 a;
	mutualBind macroBinds tlBinds a = 
	 let
	 	{
	 	?syntacticbindings = emptyBindings;
	 	} in
	 let
		{
		mb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 macroBinds emptyBindings;
		tlb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 tlBinds emptyBindings;
		} in
	 let
	 	{
	 	?macrobindings = mb;
	 	?toplevelbindings = tlb;
	 	} in a;
	}
