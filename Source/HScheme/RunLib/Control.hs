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
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	-- 6.4 Control Features
	isProcedure ::
		(
		ObjectSubtype r obj (Procedure obj m),
		Build m r
		) =>
	 Type (Procedure obj m) -> obj -> m Bool;
	isProcedure = getObjectIs;

	isProcedureP ::
		(
		ObjectSubtype r obj (Procedure obj m),
		Build m r,
		?objType :: Type obj
		) =>
	 (obj,()) -> m Bool;
	isProcedureP (obj,()) = isProcedure MkType obj;

	applyPL :: (?objType :: Type obj) =>
	 (Procedure obj m,([obj],())) -> m [obj];
	applyPL (proc,(args,())) = proc args;

	procedureObject ::
		(
		ObjectSubtype r obj (Procedure obj m),
		Build m r
		) =>
	 Procedure obj m -> m obj;
	procedureObject = getObject;

	callCCPL ::
		(
		ObjectSubtype r obj (Procedure obj m),
		Build m r,
		?objType :: Type obj,
		MonadCont m
		) =>
	 (Procedure obj m,()) -> m [obj];
	callCCPL (proc,()) = callCC (\cont -> do
		{
		contObj <- procedureObject cont;
		proc [contObj];
		});

	dynamicWindPL ::
		(
		?objType :: Type obj,
		MonadGuard m
		) =>
	 (Procedure obj m,(Procedure obj m,(Procedure obj m,()))) ->
	 m [obj];
	dynamicWindPL (before,(proc,(after,()))) = bracket (before [] >> return ()) (after [] >> return ()) (proc []);

	fixPL ::
		(
		InterpretObject m r obj,
		?objType :: Type obj,
		MonadFix m
		) =>
	 (Procedure obj m,()) -> m [obj];
	fixPL (proc,()) = do
		{
		obj <- mfix (\obj -> do
			{
			vals <- proc [obj];
			singleValue vals;
			});
		return [obj];
		};

	valuesPL :: (Monad m,?objType :: Type obj) =>
	 [obj] -> m [obj];
	valuesPL = return;

	callWithValuesPL ::
		(
		Monad m,
		?objType :: Type obj
		) =>
	 (Procedure obj m,(Procedure obj m,())) -> m [obj];
	callWithValuesPL (producer,(consumer,())) = do
		{
		vals <- producer [];
		consumer vals;
		};

	lastResortThrowP :: (MonadThrow obj m,?objType :: Type obj) =>
	 (obj,()) -> m obj;
	lastResortThrowP (obj,()) = lastResortThrowObject obj;

{--
	catchM :: (Scheme m r,MonadException (Object r m) m,?objType :: Type (Object r m)) =>
	 (Procedure (Object r m) m,()) -> m (Object r m);
	catchM = catch;
--}
	}
