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

module Org.Org.Semantic.HScheme.Interpret.Evaluate where
	{
	import Org.Org.Semantic.HScheme.Interpret.Interpret;
	import Org.Org.Semantic.HScheme.Interpret.TopLevel;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;


	-- 6.5 Eval

	evaluateObject ::
		(
		Scheme m r,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro m r m)
		) =>
	 Object r m -> (Symbol -> Maybe (ObjLocation r m)) -> m (Object r m);
	evaluateObject obj lookupSym = do
		{
		mobj <- interpretTopLevelExpression obj;
		mobj lookupSym;
		};

	evaluateP ::
		(
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (forall a. cm a -> m a) ->
	 (Object r m,(Environment r (Object r m),())) -> m (Object r m);
	evaluateP remonad (obj,(MkEnvironment syn loc,())) = let
		{
		?syntacticbindings = syn;
		?toplevelbindings = fmap (fmap (remonadTopLevelMacro remonad)) ?toplevelbindings;
		?macrobindings = fmap (fmap (remonadMacro remonad)) ?macrobindings;
		} in
	 evaluateObject obj (getBinding loc);

{--
	currentEnvironmentP ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 () -> m (Bindings r m);
	currentEnvironmentP () = return ?bindings;
--}
	}
