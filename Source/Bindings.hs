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

module Bindings where
	{
	import Conversions;
	import Object;
	import Subtype;
	import Type;

	addBinding ::
		(
		Scheme x m r
		) =>
	 String -> Object r m -> Bindings r m -> m (Bindings r m);
	addBinding name obj b = do
		{
		(_,b) <- newObjBinding b (MkSymbol name) obj;
		return b;
		};

	addProcBinding ::
		(
		ArgumentList x m r args,
		MonadIsA m (Object r m) ret
		) =>
	 String ->
	 ((?bindings :: Bindings r m) => Type (r ()) -> args -> m ret) ->
	 Bindings r m ->
	 m (Bindings r m);
	addProcBinding name p b = do
		{
		addBinding name (ProcedureObject (convertToProcedure (p Type))) b;
		};

	addMacroBinding ::
		(
		Scheme x m r,
		MonadMaybeA m args (Object r m),
		MonadIsA m (Object r m) ret
		) =>
	 String ->
	 ((?bindings :: Bindings r m) => Type (r ()) -> args -> m ret) ->
	 Bindings r m ->
	 m (Bindings r m);
	addMacroBinding name p b = do
		{
		addBinding name (MacroObject (convertToMacro (p Type))) b;
		};

	exitFuncProc :: (() -> m ()) -> (Type (r ()) -> () -> m ());
	exitFuncProc exitFunc Type = exitFunc;

	chain :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c);
	chain p1 p2 a = (p1 a) >>= p2;

	chainList list = foldl chain return list;
	}
