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

module Org.Org.Semantic.HScheme.Bind.Add where
	{
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	addBinding :: sym -> a -> Bindings sym a -> Bindings sym a;
	addBinding sym a b = newBinding b sym a;

	type RefBindings cm r obj =
		(
		?objType :: Type obj
		) =>
	 SymbolBindings (r obj) -> cm (SymbolBindings (r obj));

	addLocationBinding :: (Build cm r) =>
	 Symbol ->
	 obj ->
	 RefBindings cm r obj;
	addLocationBinding name obj b = do
		{
		(_,b') <- newRefBinding b name obj;
		return b';
		};

	addProcBinding ::
		(
		Build cm r,
		ArgumentList r obj args,
		ObjectSubtype r obj ret,
		InterpretObject m r obj
		) =>
	 String ->
	 ((?objType :: Type obj) => args -> m ret) ->
	 RefBindings cm r obj;
	addProcBinding name p b = do
		{
		procObj <- getObject (convertToProcedure p);
		addLocationBinding (MkSymbol name) procObj b;
		};

	addProcNBinding ::
		(
		Build cm r,
		InterpretObject m r obj,
		ArgumentList r obj args
		) =>
	 String ->
	 ((?objType :: Type obj) => args -> m ()) ->
	 RefBindings cm r obj;
	addProcNBinding name p b = do
		{
		procObj <- getObject (convertPNToProcedure p);
		addLocationBinding (MkSymbol name) procObj b;
		};

	addProcLBinding ::
		(
		Build cm r,
		InterpretObject m r obj,
		ArgumentList r obj args
		) =>
	 String ->
	 ((?objType :: Type obj) => args -> m [obj]) ->
	 RefBindings cm r obj;
	addProcLBinding name p b = do
		{
		procObj <- getObject (convertPLToProcedure p);
		addLocationBinding (MkSymbol name) procObj b;
		};

	convertToMacro ::
		(
		ProcedureError cm obj,
		Build cm r,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 (
		(
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	  args -> cm (ListSchemeExpression r obj m)) ->
	 Macro cm r obj m;
	convertToMacro foo = MkMacro (\objs -> do
		{
		args <- convertFromObjects objs;
		foo args;
		});

	convertToTopLevelMacro ::
		(
		ProcedureError cm obj,
		Build cm r,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 (
		(
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	 args -> cm (TopLevelListCommand r obj m)) ->
	 TopLevelMacro cm r obj m;
	convertToTopLevelMacro foo = MkTopLevelMacro (\argObjs -> do
		{
		args <- convertFromObjects argObjs;
		foo args;
		});

	type MacroBindings cm r obj m =
		(
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 SymbolBindings (Macro cm r obj m) -> SymbolBindings (Macro cm r obj m);

	addMacroBinding ::
		(
		ProcedureError cm obj,
		Build cm r,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 String ->
	 (
		(
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	  args -> cm (ListSchemeExpression r obj m)) ->
	 MacroBindings cm r obj m;
	addMacroBinding name p b = addBinding (MkSymbol name) (convertToMacro p) b;

	type TopLevelBindings cm r obj m =
		(
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m),
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m)
		) =>
	 SymbolBindings (TopLevelMacro cm r obj m) -> SymbolBindings (TopLevelMacro cm r obj m);

	addTopLevelMacroBinding ::
		(
		ProcedureError cm obj,
		Build cm r,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 String ->
	 (
		(
		?syntacticbindings :: SymbolBindings (Syntax r obj m)
		) =>
	 args -> cm (TopLevelListCommand r obj m)) ->
	 TopLevelBindings cm r obj m;
	addTopLevelMacroBinding name p b = addBinding (MkSymbol name) (convertToTopLevelMacro p) b;

	exitFuncProc :: (Monad m) => (a -> m b) -> (a -> m ());
	exitFuncProc exitFunc a = do
		{
		exitFunc a;
		return ();
		};
	}
