-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>

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

module Org.Org.Semantic.HScheme.Core.Procedure where
	{
	import Org.Org.Semantic.HScheme.Core.Conversions;
	import Org.Org.Semantic.HScheme.Core.Mismatch;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HBase;

	type Procedure obj m = [obj] -> m [obj];

	class (Monad m) =>
	 ProcedureError m obj where
		{
		throwMismatchError :: forall a. (?objType :: Type obj) =>
		 Mismatch obj -> m a;
		throwArgumentListMismatchError :: forall a. (?objType :: Type obj) =>
		 ArgumentMismatch obj -> m a;
		};
	
	fromObject ::
		(
		Build m r,
		ObjectSubtype r obj a,
		ProcedureError m obj,
		?objType :: Type obj
		) =>
	 obj -> m a;
	fromObject obj = do
		{
		ma <- resultFromObject obj;
		case ma of
			{
			SuccessResult a -> return a;
			ExceptionResult mismatch -> throwMismatchError mismatch;
			};
		};

	class (ProcedureError m obj) =>
	 RunError m obj | obj -> m where
		{
		throwWrongContextError :: forall a. (?objType :: Type obj) =>
		 [obj] -> m a;
		throwTooFewArgumentsError :: forall a. (?objType :: Type obj) =>
		 m a;
		throwTooManyArgumentsError :: forall a. (?objType :: Type obj) =>
		 [obj] -> m a;
		throwBadApplyFormError :: forall a. (?objType :: Type obj) =>
		 obj -> m a;
		throwArrayRangeError :: forall a. (?objType :: Type obj) =>
		 (Integer,Integer) -> Integer -> m a;
		};

	class
		(
		Build m r,
		ObjectSubtype r obj (Procedure obj m),
		RunError m obj
		) =>
	 ApplyObject m r obj | obj -> m r;

	instance
		(
		Build m r,
		ObjectSubtype r obj (Procedure obj m),
		RunError m obj
		) =>
	 ApplyObject m r obj;

	doApply ::
		(
		ApplyObject m r obj,
		?objType :: Type obj
		) =>
	 m obj -> m [obj] -> m [obj];
	doApply mf margs = do
		{
		f <- mf;
		mproc <- resultFromObject f;
		case mproc of
			{
			SuccessResult proc -> do
				{
				args <- margs;
				proc args;
				};
			_ -> throwBadApplyFormError f;
			};
		};

	singleValue ::
		(
		ApplyObject m r obj,
		?objType :: Type obj
		) =>
	 [obj] -> m obj;

	singleValue [obj] = return obj;
	singleValue list = throwWrongContextError list;
{-
	singleValue (obj:_) = return obj;
	singleValue [] = return VoidObject;
-}
	}
