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

module Org.Org.Semantic.HScheme.RunLib.Control where
	{
	import Org.Org.Semantic.HScheme.RunLib.ArgumentList;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	-- 6.4 Control Features
	isProcedure :: (Scheme m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isProcedureP (obj,()) = isProcedure obj;

	applyPL :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Procedure (Object r m) m,([Object r m],())) -> m [Object r m];
	applyPL (proc,(args,())) = proc args;

	callCCPL ::
		(
		Scheme m r,
		?objType :: Type (Object r m),
		MonadCont m
		) =>
	 (Procedure (Object r m) m,()) -> m [Object r m];
	callCCPL (proc,()) = callCC (\cont -> proc [ProcedureObject cont]);

	dynamicWindPL ::
		(
		Scheme m r,
		?objType :: Type (Object r m),
		MonadGuard m
		) =>
	 (Procedure (Object r m) m,(Procedure (Object r m) m,(Procedure (Object r m) m,()))) ->
	 m [Object r m];
	dynamicWindPL (before,(proc,(after,()))) = bracket (before [] >> return ()) (after [] >> return ()) (proc []);

	fixPL ::
		(
		?objType :: Type obj,
		MonadFix m
		) =>
	 (Procedure obj m,()) -> m [obj];
	fixPL (proc,()) = mfix proc;

	valuesPL :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [Object r m] -> m [Object r m];
	valuesPL = return;

	callWithValuesPL ::
		(
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 (Procedure (Object r m) m,(Procedure (Object r m) m,())) -> m [Object r m];
	callWithValuesPL (producer,(consumer,())) = do
		{
		vals <- producer [];
		consumer vals;
		};

	lastResortThrowP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m (Object r m);
	lastResortThrowP (obj,()) = lastResortThrowObject obj;

{--
	catchM :: (Scheme m r,MonadException (Object r m) m,?objType :: Type (Object r m)) =>
	 (Procedure (Object r m) m,()) -> m (Object r m);
	catchM = catch;
--}
	}
