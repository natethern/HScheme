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
	import Org.Org.Semantic.HScheme.Interpret.SymbolExpression;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;


{--
	isNil :: Object r m -> Bool;
	isNil NilObject = True;
	isNil _ = False;
--}

	getBadLoc :: Symbol -> ObjLocation r m;
--	getBadLoc sym = throwArgError "unbound-symbol" ([SymbolObject sym]);
	getBadLoc sym = error ("unbound-symbol" ++ (show sym));

	getLoc ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 (Symbol -> Maybe (ObjLocation r m)) -> Symbol -> ObjLocation r m;
	getLoc getter sym = case (getter sym) of
		{
		Just loc -> loc;
		Nothing -> getBadLoc sym;
		};
{--
	getSymbolBinding ::
		(
		Scheme m r
		) =>
	 Bindings r m -> Symbol -> m (ObjLocation r m);
	getSymbolBinding bindings sym = getLoc (getBinding bindings);
--}

	bindExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 SchemeExpression r m a -> cm ((Symbol -> Maybe (ObjLocation r m)) -> a);
	bindExpression rr = return (\lookupSym -> runSymbolExpression (getLoc lookupSym) rr);

	interpretTopLevelExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m (Object r m));
	interpretTopLevelExpression obj = let {?objType = Type} in do
		{
		rr <- assembleTopLevelExpression obj;
		bindExpression rr;
		};

	interpretTopLevelExpressionsEat ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 [Object r m] -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m ());
	interpretTopLevelExpressionsEat eat objs = let {?objType = Type} in do	
		{
		rr <- assembleTopLevelExpressionsEat eat objs;
		bindExpression rr;
		};

	interpretTopLevelExpressionsList ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] -> cm ((Symbol -> Maybe (ObjLocation r m)) -> m [Object r m]);
	interpretTopLevelExpressionsList objs = let {?objType = Type} in do	
		{
		rr <- assembleTopLevelExpressionsList objs;
		bindExpression rr;
		};
	}
