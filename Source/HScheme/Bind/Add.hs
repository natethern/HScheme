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

	addLocationBinding :: (Build cm r) =>
	 Symbol ->
	 Object r m ->
	 SymbolBindings (ObjLocation r m) ->
	 cm (SymbolBindings (ObjLocation r m));
	addLocationBinding name obj b = do
		{
		(_,b') <- newRefBinding b name obj;
		return b';
		};

	addProcBinding ::
		(
		Build cm r,
		ArgumentList m m r args,
		MonadIsA m (Object r m) ret,
		?objType :: Type (Object r m)
		) =>
	 String ->
	 ((?objType :: Type (Object r m)) => args -> m ret) ->
	 SymbolBindings (ObjLocation r m) ->
	 cm (SymbolBindings (ObjLocation r m));
	addProcBinding name p b = addLocationBinding (MkSymbol name)
	 (ProcedureObject (convertToProcedure p)) b;

	convertToMacro ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 ((
		?syntacticbindings :: SymbolBindings (Syntax cm r m),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	  args -> cm (ObjectSchemeExpression r m)) ->
	 (Macro cm r m);
	convertToMacro foo = MkMacro (\objs -> do
		{
		args <- convertFromObjects objs;
		foo args;
		});

	convertToTopLevelMacro ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 ((
		?syntacticbindings :: SymbolBindings (Syntax cm r m),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 args -> cm (TopLevelObjectCommand cm r m)) ->
	 (TopLevelMacro cm r m);
	convertToTopLevelMacro foo = MkTopLevelMacro (\argObjs -> do
		{
		args <- convertFromObjects argObjs;
		foo args;
		});

	addMacroBinding ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 String ->
	 ((
		?syntacticbindings :: SymbolBindings (Syntax cm r m),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	  args -> cm (ObjectSchemeExpression r m)) ->
	 SymbolBindings (Macro cm r m) ->
	 SymbolBindings (Macro cm r m);
	addMacroBinding name p b = addBinding (MkSymbol name) (convertToMacro p) b;

	addTopLevelMacroBinding ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 String ->
	 ((
		?syntacticbindings :: SymbolBindings (Syntax cm r m),
		?macrobindings :: SymbolBindings (Macro cm r m)
		) =>
	 args -> cm (TopLevelObjectCommand cm r m)) ->
	 SymbolBindings (TopLevelMacro cm r m) ->
	 SymbolBindings (TopLevelMacro cm r m);
	addTopLevelMacroBinding name p b = addBinding (MkSymbol name) (convertToTopLevelMacro p) b;

	exitFuncProc :: (Monad m) => (a -> m b) -> (a -> m ());
	exitFuncProc exitFunc a = do
		{
		exitFunc a;
		return ();
		};
	}
