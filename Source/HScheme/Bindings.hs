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
	import Org.Org.Semantic.HScheme.Compile;
--	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	addBinding :: sym -> a -> Binds sym a -> Binds sym a;
	addBinding sym a b = newBinding b sym a;

	addLocationBinding ::
		(
		Scheme m r
		) =>
	 Symbol -> Object r m -> Bindings r m -> m (Bindings r m);
	addLocationBinding name obj b = do
		{
		(_,b') <- newObjBinding b name obj;
		return b';
		};

	addProcBinding ::
		(
		ArgumentList m r args,
		MonadIsA m (Object r m) ret,
		?refType :: Type (r ())
		) =>
	 String ->
	 ((?refType :: Type (r ())) => args -> m ret) ->
	 Bindings r m ->
	 m (Bindings r m);
	addProcBinding name p b = addLocationBinding (MkSymbol name)
	 (ProcedureObject (convertToProcedure p)) b;

	convertToTopLevelMacro ::
		(
		ArgumentList m r args,
		?refType :: Type (r ())
		) =>
	 (args -> TopLevelAction r m) ->
	 (TopLevelMacro r m);
	convertToTopLevelMacro foo argObjs = MkTopLevelAction (\beg restObjs -> do
		{
		args <- convertFromObjects argObjs;
		unTopLevelAction (foo args) beg restObjs;
		});

	addMacroBinding ::
		(
		ArgumentList m r args,
		?refType :: Type (r ())
		) =>
	 String ->
	 (args -> m result) ->
	 Binds Symbol ([Object r m] -> m result) ->
	 Binds Symbol ([Object r m] -> m result);
	addMacroBinding name p b = addBinding (MkSymbol name) (convertToMacro p) b;

	addTopLevelMacroBinding ::
		(
		ArgumentList m r args,
		?refType :: Type (r ())
		) =>
	 String ->
	 (args -> TopLevelAction r m) ->
	 Binds Symbol ([Object r m] -> TopLevelAction r m) ->
	 Binds Symbol ([Object r m] -> TopLevelAction r m);
	addTopLevelMacroBinding name p b = addBinding (MkSymbol name) (convertToTopLevelMacro p) b;

	exitFuncProc :: (Monad m) => (a -> m b) -> (a -> m ());
	exitFuncProc exitFunc a = do
		{
		exitFunc a;
		return ();
		};
	}
