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

module Org.Org.Semantic.HScheme.Interpret.Assemble
	(
	schemeExprSymbol,
	SchemeExpression,ObjectSchemeExpression,ListSchemeExpression,
	SymbolBindings,
	Macro(..),remonadMacro,
	PatternError(..),Syntax(..),
	AssembleError(..),
	InterpretObject,
	assembleExpression,assembleExpressionSingle,
	assembleSymbolExpressionSingle,assembleSymbolExpression
	) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class
		(
		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		ApplyObject m r obj
		) =>
	 InterpretObject m r obj | obj -> m r;

	instance
		(
		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		ApplyObject m r obj
		) =>
	 InterpretObject m r obj;

	class (ProcedureError cm obj) =>
	 PatternError cm obj where
		{
		throwBadPatternError :: forall a. (?objType :: Type obj) =>
		 obj -> cm a;
		throwPatternNotMatchedError :: forall a. (?objType :: Type obj) =>
		 [obj] -> cm a;
		};

	newtype Syntax r obj m = MkSyntax (forall cm.
		(
		Build cm r,
		AssembleError cm obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m),
		?objType :: Type obj
		) =>
	 (Type (r ())) -> [obj] -> cm (ListSchemeExpression r obj m));

	type SchemeExpression r obj = TrackingLambdaExpression Symbol (LookupLambdaExpression Symbol (r obj));

	type ObjectSchemeExpression r obj m = SchemeExpression r obj (m obj);

	type ListSchemeExpression r obj m = SchemeExpression r obj (m [obj]);

	type SymbolBindings = Bindings Symbol;

	newtype Macro cm r obj m = MkMacro
		((
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	 [obj] -> cm (ListSchemeExpression r obj m));

	remonadMacro :: (forall a. cm1 a -> cm2 a) -> Macro cm1 r obj m -> Macro cm2 r obj m;
	remonadMacro map (MkMacro tlm) = MkMacro (map . tlm);

	class
		(
		ProcedureError cm obj,
		PatternError cm obj
		) =>
	 AssembleError cm obj where
		{
		throwBadCombinationError :: forall a. (?objType :: Type obj) =>
		 obj -> Mismatch obj -> cm a;
		};

	makeApply ::
		(
		InterpretObject m r obj,
		?objType :: Type obj
		) =>
	 ObjectSchemeExpression r obj m ->
	 [ObjectSchemeExpression r obj m] ->
	 ListSchemeExpression r obj m;
	makeApply f args = fApply (fmap doApply f) (fmap fExtract (fExtract args));

	assembleApplyExpression ::
		(
		AssembleError cm obj,
		InterpretObject m r obj,
		Build cm r,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> [obj] -> cm (ListSchemeExpression r obj m);
	assembleApplyExpression f arglist = do
		{
		fe <- assembleExpressionSingle f;
		ae <- for assembleExpressionSingle arglist;
		return (makeApply fe ae);
		};

	schemeExprSymbol ::
	 Symbol -> SchemeExpression r obj (r obj);
	schemeExprSymbol = exprSymbol;

	assembleSymbolExpressionSingle ::
		(
		Build m r,
		?objType :: Type obj
		) =>
	 Symbol -> ObjectSchemeExpression r obj m;
	assembleSymbolExpressionSingle sym = fmap get (schemeExprSymbol sym);

	assembleSymbolExpression ::
		(
		Build m r,
		?objType :: Type obj
		) =>
	 Symbol -> ListSchemeExpression r obj m;
	assembleSymbolExpression sym = fmap (\loc -> do
		{
		obj <- get loc;
		return [obj];
		}) (schemeExprSymbol sym);

	assembleExpression ::
		(
		AssembleError cm obj,
		InterpretObject m r obj,
		Build cm r,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> cm (ListSchemeExpression r obj m);
	assembleExpression subjObj = do
		{
		msubj <- resultFromObject subjObj;
		case msubj of
			{
			SuccessResult (Left sym) -> return (assembleSymbolExpression sym);
			SuccessResult (Right (h :: obj,t :: obj)) -> do
				{
				marglist <- resultFromObject t;
				case marglist of
					{
					ExceptionResult mismatch -> throwBadCombinationError t mismatch;
					SuccessResult arglist -> do
						{
						mheadSym <- resultFromObject h;
						case mheadSym of
							{
							SuccessResult sym -> case getBinding ?syntacticbindings sym of
								{
								Just (MkSyntax syntax) -> do
									{
--									obj <- syntax arglist;
--									assembleExpression obj;
									syntax MkType arglist;
									};
								Nothing -> case ?macrobindings sym of
									{
									Just (MkMacro macro) -> macro arglist;
									Nothing -> assembleApplyExpression h arglist;
									};
								};
							_ -> assembleApplyExpression h arglist;
							};
						};
					};
				};
			ExceptionResult _ -> return (return (return [subjObj]));
			};
		};

	assembleExpressionSingle ::
		(
		AssembleError cm obj,
		InterpretObject m r obj,
		Build cm r,
		?objType :: Type obj,
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> cm (ObjectSchemeExpression r obj m);
	assembleExpressionSingle obj = do
		{
		listExp <- assembleExpression obj;
		return (fmap (\mlist -> mlist >>= singleValue) listExp);
		};
	}
