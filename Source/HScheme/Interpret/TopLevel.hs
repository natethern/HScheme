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

module Org.Org.Semantic.HScheme.Interpret.TopLevel
	(
	TopLevelCommand(..),TopLevelObjectCommand,TopLevelMacro(..),TopLevelBinder(..),
	begin,
	assembleTopLevelExpression,assembleTopLevelExpressions,
	assembleTopLevelExpressionsEat,assembleTopLevelExpressionsList
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
--	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	data TopLevelCommand r m a = MkTopLevelCommand
		{
		tleExpression :: SchemeExpression r m a,
		tleInitialBindings :: [(Symbol,ObjectSchemeExpression r m)],
		tleSyntaxes :: [(Symbol,Syntax r (Object r m))]
		};

	type TopLevelObjectCommand r m = TopLevelCommand r m (m (Object r m));

	newtype TopLevelMacro cm r m = MkTopLevelMacro ((
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] -> cm (TopLevelObjectCommand r m));

	newtype TopLevelBinder r m = MkTopLevelBinder
	 {unTopLevelBinder :: forall a. TopLevelCommand r m (m a) -> SchemeExpression r m (m a)};

	assembleTopLevelObjectCommand ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm (TopLevelObjectCommand r m);
	assembleTopLevelObjectCommand obj = do
		{
		mpair <- getMaybeConvert obj;
		case mpair of
			{
			Just (sym,args) -> case getBinding ?toplevelbindings sym of
				{
				Just (MkTopLevelMacro tlm) -> tlm args;
				Nothing -> compileExprTopLevel;
				};
			Nothing -> compileExprTopLevel;
			};
		} where
		{
		compileExprTopLevel = do
			{
			expr <- assembleExpression obj;
			return (MkTopLevelCommand expr [] []);
			};
		};

	assembleTopLevelExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 Object r m -> cm (ObjectSchemeExpression r m);
	assembleTopLevelExpression obj = do
		{
		MkTopLevelCommand expr _ _ <- assembleTopLevelObjectCommand obj;
		return expr;
		};

	begin ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 a -> 
	 (m (Object r m) -> a) ->
	 (m (Object r m) -> a -> a) ->
	 [Object r m] ->
	 cm (TopLevelCommand r m a);
	begin none one conn [] = return (MkTopLevelCommand (return none) [] []);
	begin none one conn [obj] = do
		{
		(MkTopLevelCommand expr binds syntax) <- assembleTopLevelObjectCommand obj;
		return (MkTopLevelCommand (fmap one expr) binds syntax);
		};
	begin none one conn (obj:objs) = do
		{
		(MkTopLevelCommand expr1 binds1 syntax1) <- assembleTopLevelObjectCommand obj;
		(MkTopLevelCommand exprr bindsr syntaxr) <- let
		 {?syntacticbindings = newBindings ?syntacticbindings syntax1} in
		 begin none one conn objs;
		return (MkTopLevelCommand (liftF2 conn expr1 exprr) (binds1 ++ bindsr) (syntax1 ++ syntaxr));
		};

	assembleTopLevelExpressions ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 m a -> 
	 (m (Object r m) -> m a) ->
	 (m (Object r m) -> m a -> m a) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a));
	assembleTopLevelExpressions none one conn objs = do
		{
		tlc <- begin none one conn objs;
		return (unTopLevelBinder ?binder tlc);
		};

	assembleTopLevelExpressionsList ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 [Object r m] ->
	 cm (SchemeExpression r m (m [Object r m]));
	assembleTopLevelExpressionsList = assembleTopLevelExpressions
	 (return [])
	 (\mr -> do
	 	{
		r <- mr;
		return [r];
	 	})
	 (\mr mrs -> do
		{
		r <- mr;
		rs <- mrs;
		return (r:rs);
		});

	assembleTopLevelExpressionsEat ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: SymbolBindings (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m ()));
	assembleTopLevelExpressionsEat eat = assembleTopLevelExpressions
	 (return ())
	 (\mr -> do
	 	{
		r <- mr;
		eat r;
	 	})
	 (\mr mrs -> do
		{
		r <- mr;
		eat r;
		mrs;
		});

{--
top-level: define, load, define-syntax
macro: quote, lambda, if, let, let*, syntax-rules, case-match, letrec, begin, set!
--}
	}
