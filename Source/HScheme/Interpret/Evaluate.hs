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

	data Environment r obj m = MkEnvironment
		{
		envSyn :: SymbolBindings (Syntax r obj m),
		envLoc :: Bindings Symbol (r obj)
		};

	evaluateObject ::
		(
		AssembleError m obj,
		MonadThrow obj m,
		InterpretObject m r obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro m r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro m r obj m)
		) =>
	 obj -> (Symbol -> Maybe (r obj)) -> m [obj];
	evaluateObject obj lookupSym = do
		{
		mobj <- interpretTopLevelExpression obj;
		mobj lookupSym;
		};

	evaluatePL ::
		(
		AssembleError m obj,
		MonadThrow obj m,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (forall a. cm a -> m a) ->
	 (obj,(Environment r obj m,())) -> m [obj];
	evaluatePL remonad (obj,(MkEnvironment syn loc,())) = let
		{
		?syntacticbindings = syn;
		?toplevelbindings = fmap (fmap (remonadTopLevelMacro remonad)) ?toplevelbindings;
		?macrobindings = fmap (fmap (remonadMacro remonad)) ?macrobindings;
		} in
	 evaluateObject obj (getBinding loc);
	}
