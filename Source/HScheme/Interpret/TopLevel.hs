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
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	data TopLevelCommand r obj m a = MkTopLevelCommand
		{
		tleExpression :: SchemeExpression r obj a,
		tleInitialBindings :: [(Symbol,ObjectSchemeExpression r obj m)],
		tleSyntaxes :: [(Symbol,Syntax r obj m)]
		};

	expressionCommand :: SchemeExpression r obj a -> TopLevelCommand r obj m a;
	expressionCommand expr = MkTopLevelCommand expr [] [];

	instance HasReturn (TopLevelCommand r obj m) where
		{
		return a = expressionCommand (return a);
		};

	instance Functor (TopLevelCommand r obj m) where
		{
		fmap ab (MkTopLevelCommand expr binds syntax) = MkTopLevelCommand (fmap ab expr) binds syntax;
		};

	instance FunctorApply (TopLevelCommand r obj m) where
		{
		fapply (MkTopLevelCommand ab binds1 syntax1) (MkTopLevelCommand a binds2 syntax2) = 
		 MkTopLevelCommand (fapply ab a) (binds1 ++ binds2) (syntax1 ++ syntax2);

		fpassto (MkTopLevelCommand a binds1 syntax1) (MkTopLevelCommand ab binds2 syntax2) = 
		 MkTopLevelCommand (fpassto a ab) (binds1 ++ binds2) (syntax1 ++ syntax2);

		fseq (MkTopLevelCommand a binds1 syntax1) (MkTopLevelCommand b binds2 syntax2) = 
		 MkTopLevelCommand (fseq a b) (binds1 ++ binds2) (syntax1 ++ syntax2);
		};

	type TopLevelListCommand r obj m = TopLevelCommand r obj m (m [obj]);

	instance (HasReturn m) =>
	 HasNothing (TopLevelListCommand r obj m) where
		{
		nothing = return (return nothing);
		};

	newtype TopLevelMacro cm r obj m = MkTopLevelMacro (
		(
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	 [obj] -> cm (TopLevelListCommand r obj m));

	remonadTopLevelMacro :: (forall a. cm1 a -> cm2 a) -> TopLevelMacro cm1 r obj m -> TopLevelMacro cm2 r obj m;
	remonadTopLevelMacro map (MkTopLevelMacro tlm) = MkTopLevelMacro (map . tlm);

	newtype TopLevelBinder r obj m = MkTopLevelBinder
	 {unTopLevelBinder :: forall a. [(Symbol,ObjectSchemeExpression r obj m)] -> SchemeExpression r obj (m a) -> SchemeExpression r obj (m a)};

	assembleTopLevelListCommand ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> cm (TopLevelListCommand r obj m);
	assembleTopLevelListCommand obj = do
		{
		mpair <- resultFromObject obj;
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
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 obj -> cm (ListSchemeExpression r obj m);
	assembleTopLevelExpression obj = do
		{
		MkTopLevelCommand expr _ _ <- assembleTopLevelListCommand obj;
		return expr;
		};

	beginCommand ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 a ->
	 (m [obj] -> a) ->
	 (m [obj] -> a -> a) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 cm (TopLevelCommand r obj m a);
	beginCommand noResult oneResult conn command1 objs = do
		{
		commandr <- let
		 {?syntacticbindings = newBindings ?syntacticbindings (tleSyntaxes command1)} in
		 begin noResult oneResult conn objs;
		return (liftF2 conn command1 commandr);
		};

	begin ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 a -> 
	 (m [obj] -> a) ->
	 (m [obj] -> a -> a) ->
	 [obj] ->
	 cm (TopLevelCommand r obj m a);
	begin noResult oneResult conn [] = return (return noResult);
	begin noResult oneResult conn [obj] = do
		{
		command <- assembleTopLevelListCommand obj;
		return (fmap oneResult command);
		};
	begin noResult oneResult conn (obj:objs) = do
		{
		command <- assembleTopLevelListCommand obj;
		beginCommand noResult oneResult conn command objs;
		};

	assembleTopLevelExpressions ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 m a -> 
	 (m [obj] -> m a) ->
	 (m [obj] -> m a -> m a) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 cm (SchemeExpression r obj (m a));
	assembleTopLevelExpressions noResult oneResult conn command objs = do
		{
		MkTopLevelCommand expr binds _ <- beginCommand noResult oneResult conn command objs;
		return (unTopLevelBinder ?binder binds expr);
		};

	assembleTopLevelExpressionsList ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 cm (SchemeExpression r obj (m [obj]));
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
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?binder :: TopLevelBinder r obj m,
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 (obj -> m ()) ->
	 TopLevelListCommand r obj m ->
	 [obj] ->
	 cm (SchemeExpression r obj (m ()));
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
