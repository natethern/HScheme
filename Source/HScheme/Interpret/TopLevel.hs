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
	TopLevelCommand(..),TopLevelListCommand,TopLevelMacro(..),TopLevelBinder(..),
	remonadTopLevelMacro,
	begin,
	assembleTopLevelExpression,assembleTopLevelExpressions,
	assembleTopLevelExpressionsEat,assembleTopLevelExpressionsList,
	assembleTopLevelListCommand
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
--	import Org.Org.Semantic.HScheme.Interpret.LambdaExpression;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	data TopLevelCommand r m a = MkTopLevelCommand
		{
		tleExpression :: SchemeExpression r m a,
		tleInitialBindings :: [(Symbol,ObjectSchemeExpression r m)],
		tleSyntaxes :: [(Symbol,Syntax r (Object r m))]
		};

	expressionCommand :: SchemeExpression r m a -> TopLevelCommand r m a;
	expressionCommand expr = MkTopLevelCommand expr [] [];

	instance HasReturn (TopLevelCommand r m) where
		{
		return a = expressionCommand (return a);
		};

	instance Functor (TopLevelCommand r m) where
		{
		fmap ab (MkTopLevelCommand expr binds syntax) = MkTopLevelCommand (fmap ab expr) binds syntax;
		};

	instance FunctorApply (TopLevelCommand r m) where
		{
		fApply (MkTopLevelCommand ab binds1 syntax1) (MkTopLevelCommand a binds2 syntax2) = 
		 MkTopLevelCommand (fApply ab a) (binds1 ++ binds2) (syntax1 ++ syntax2);

		fPassTo (MkTopLevelCommand a binds1 syntax1) (MkTopLevelCommand ab binds2 syntax2) = 
		 MkTopLevelCommand (fPassTo a ab) (binds1 ++ binds2) (syntax1 ++ syntax2);

		fSeq (MkTopLevelCommand a binds1 syntax1) (MkTopLevelCommand b binds2 syntax2) = 
		 MkTopLevelCommand (fSeq a b) (binds1 ++ binds2) (syntax1 ++ syntax2);
		};

	type TopLevelListCommand r m = TopLevelCommand r m (m [Object r m]);

	instance (HasReturn m) =>
	 HasNothing (TopLevelListCommand r m) where
		{
		nothing = return (return nothing);
		};

	newtype TopLevelMacro cm r m = MkTopLevelMacro (
		(
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m))
		) =>
	 [Object r m] -> cm (TopLevelListCommand r m));

	remonadTopLevelMacro :: (forall a. cm1 a -> cm2 a) -> TopLevelMacro cm1 r m -> TopLevelMacro cm2 r m;
	remonadTopLevelMacro map (MkTopLevelMacro tlm) = MkTopLevelMacro (map . tlm);

	newtype TopLevelBinder r m = MkTopLevelBinder
	 {unTopLevelBinder :: forall a. [(Symbol,ObjectSchemeExpression r m)] -> SchemeExpression r m (m a) -> SchemeExpression r m (m a)};

	assembleTopLevelListCommand ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 Object r m -> cm (TopLevelListCommand r m);
	assembleTopLevelListCommand obj = do
		{
		mpair <- fromObject obj;
		case mpair of
			{
			SuccessResult (sym,args) -> case ?toplevelbindings sym of
				{
				Just (MkTopLevelMacro tlm) -> tlm args;
				Nothing -> compileExprTopLevel;
				};
			_ -> compileExprTopLevel;
			};
		} where
		{
		compileExprTopLevel = do
			{
			expr <- assembleExpression obj;
			return (expressionCommand expr);
			};
		};

	assembleTopLevelExpression ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 Object r m -> cm (ListSchemeExpression r m);
	assembleTopLevelExpression obj = do
		{
		MkTopLevelCommand expr _ _ <- assembleTopLevelListCommand obj;
		return expr;
		};

	beginCommand ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 a ->
	 (m [Object r m] -> a) ->
	 (m [Object r m] -> a -> a) ->
	 TopLevelListCommand r m ->
	 [Object r m] ->
	 cm (TopLevelCommand r m a);
	beginCommand none one conn command1 objs = do
		{
		commandr <- let
		 {?syntacticbindings = newBindings ?syntacticbindings (tleSyntaxes command1)} in
		 begin none one conn objs;
		return (liftF2 conn command1 commandr);
		};

	begin ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 a -> 
	 (m [Object r m] -> a) ->
	 (m [Object r m] -> a -> a) ->
	 [Object r m] ->
	 cm (TopLevelCommand r m a);
	begin none one conn [] = return (return none);
	begin none one conn [obj] = do
		{
		command <- assembleTopLevelListCommand obj;
		return (fmap one command);
		};
	begin none one conn (obj:objs) = do
		{
		command <- assembleTopLevelListCommand obj;
		beginCommand none one conn command objs;
		};

	assembleTopLevelExpressions ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 m a -> 
	 (m [Object r m] -> m a) ->
	 (m [Object r m] -> m a -> m a) ->
	 TopLevelListCommand r m ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m a));
	assembleTopLevelExpressions none one conn command objs = do
		{
		MkTopLevelCommand expr binds _ <- beginCommand none one conn command objs;
		return (unTopLevelBinder ?binder binds expr);
		};

	assembleTopLevelExpressionsList ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 TopLevelListCommand r m ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m [Object r m]));
	assembleTopLevelExpressionsList = assembleTopLevelExpressions
	 (return [])
	 id
	 (\mr mrs -> do
		{
		r <- mr;
		rs <- mrs;
		return (r ++ rs);
		});

	assembleTopLevelExpressionsEat ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?binder :: TopLevelBinder r m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r m),
		?syntacticbindings :: SymbolBindings (Syntax r (Object r m)),
		?macrobindings :: Symbol -> Maybe (Macro cm r m)
		) =>
	 (Object r m -> m ()) ->
	 TopLevelListCommand r m ->
	 [Object r m] ->
	 cm (SchemeExpression r m (m ()));
	assembleTopLevelExpressionsEat eat = assembleTopLevelExpressions
	 (return ())
	 (\mr -> do
	 	{
		r <- mr;
		forDo eat r;
	 	})
	 (\mr mrs -> do
		{
		r <- mr;
		forDo eat r;
		mrs;
		});

{--
top-level: define, load, define-syntax
macro: quote, lambda, if, let, let*, syntax-rules, case-match, letrec, begin, set!
--}
	}
