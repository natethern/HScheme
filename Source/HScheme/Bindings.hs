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

module Org.Org.Semantic.HScheme.Bindings where
	{
	import Org.Org.Semantic.HScheme.ArgumentList;
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.Compile;
--	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	addBinding :: sym -> a -> Binds sym a -> Binds sym a;
	addBinding sym a b = newBinding b sym a;

	addLocationBinding ::
		(
		Build cm r
		) =>
	 Symbol -> Object r m -> Bindings r m -> cm (Bindings r m);
	addLocationBinding name obj b = do
		{
		(_,b') <- newObjBinding b name obj;
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
	 Bindings r m ->
	 cm (Bindings r m);
	addProcBinding name p b = addLocationBinding (MkSymbol name)
	 (ProcedureObject (convertToProcedure p)) b;

	convertToTopLevelMacro ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 (args -> cm (TopLevelObjectCommand cm r m)) ->
	 (TopLevelMacro cm r m);
	convertToTopLevelMacro foo argObjs = do
		{
		args <- convertFromObjects argObjs;
		foo args;
		};

	addMacroBinding ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 String ->
	 (args -> cm result) ->
	 Binds Symbol ([Object r m] -> cm result) ->
	 Binds Symbol ([Object r m] -> cm result);
	addMacroBinding name p b = addBinding (MkSymbol name) (convertToMacro p) b;

	addTopLevelMacroBinding ::
		(
		ArgumentList cm m r args,
		?objType :: Type (Object r m)
		) =>
	 String ->
	 (args -> cm (TopLevelObjectCommand cm r m)) ->
	 Binds Symbol (TopLevelMacro cm r m) ->
	 Binds Symbol (TopLevelMacro cm r m);
	addTopLevelMacroBinding name p b = addBinding (MkSymbol name) (convertToTopLevelMacro p) b;

	exitFuncProc :: (Monad m) => (a -> m b) -> (a -> m ());
	exitFuncProc exitFunc a = do
		{
		exitFunc a;
		return ();
		};
	}
