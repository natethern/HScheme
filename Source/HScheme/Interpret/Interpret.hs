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

module Org.Org.Semantic.HScheme.Interpret.Interpret where
	{
	import Org.Org.Semantic.HScheme.Interpret.TopLevel;
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	getBadLoc :: Symbol -> r obj;
--	getBadLoc sym = throwArgError "unbound-symbol" ([SymbolObject sym]);
	getBadLoc sym = error ("unbound-symbol: " ++ (show sym));

	getLoc ::
		(
--		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 (Symbol -> Maybe (r obj)) -> Symbol -> r obj;
	getLoc getter sym = case (getter sym) of
		{
		Just loc -> loc;
		Nothing -> getBadLoc sym;
		};
{--
	getSymbolBinding ::
		(
		InterpretObject m r obj
		) =>
	 Bindings r m -> Symbol -> m (r obj);
	getSymbolBinding bindings sym = getLoc (getBinding bindings);
--}

	bindExpression ::
		(
		Monad cm,
--		BuildThrow cm obj r,
--		InterpretObject m r obj,
		?objType :: Type obj
--		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
--		?syntacticbindings :: SymbolBindings (Syntax r obj),
--		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 SchemeExpression r obj a -> cm ((Symbol -> Maybe (r obj)) -> a);
	bindExpression rr = return (\lookupSym -> runLambda (getLoc lookupSym) rr);

	interpretTopLevelExpression ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> cm ((Symbol -> Maybe (r obj)) -> m [obj]);
	interpretTopLevelExpression obj = let {?objType = MkType} in do
		{
		rr <- assembleTopLevelExpression obj;
		bindExpression rr;
		};

	interpretTopLevelExpressionsEat ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (obj -> m ()) ->
	 TopLevelListCommand r obj m ->
	 [obj] -> cm ((Symbol -> Maybe (r obj)) -> m ());
	interpretTopLevelExpressionsEat eat command objs = let {?objType = MkType} in do	
		{
		rr <- assembleTopLevelExpressionsEat eat command objs;
		bindExpression rr;
		};

	interpretTopLevelExpressionsList ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 TopLevelListCommand r obj m ->
	 [obj] -> cm ((Symbol -> Maybe (r obj)) -> m [obj]);
	interpretTopLevelExpressionsList command objs = let {?objType = MkType} in do	
		{
		rr <- assembleTopLevelExpressionsList command objs;
		bindExpression rr;
		};
	}
