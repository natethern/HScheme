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

module Org.Org.Semantic.HScheme.ArgumentList where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
--	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	class (Scheme m r) => ArgumentList m r a where
		{
		convertFromObjects' :: (?refType :: Type (r ())) => [Object r m] -> m a;
		};

	convertFromObjects :: (ArgumentList m r a,?refType :: Type (r ())) =>
	 [Object r m] -> m a;
	convertFromObjects = convertFromObjects';

	instance (Scheme m r) => ArgumentList m r () where
		{
		convertFromObjects' [] = return ();
		convertFromObjects' (_:_) = throwSimpleError "too-many-args";
		};

	convertFromObject :: (Scheme m r,MonadMaybeA m to (Object r m),?refType :: Type (r ())) =>
	 (Object r m) -> m to;
	convertFromObject from = do
		{
		mto <- getMaybeConvert from;
		case mto of
			{
			Just to -> return to;
			Nothing -> throwArgError "wrong-type-arg" [from];
			};
		};

	instance
		(
		MonadMaybeA m a (Object r m),
		ArgumentList m r b
		) =>
	 ArgumentList m r (a,b) where
		{
		convertFromObjects' [] = do
			{
			throwSimpleError "too-few-args";
			};
		convertFromObjects' (obj:objs) = do
			{
			a <- convertFromObject obj;
			b <- convertFromObjects objs;
			return (a,b);
			};
		};

	instance
		(
		Scheme m r,
		MonadMaybeA m a (Object r m)
		) =>
	 ArgumentList m r (Maybe a) where
		{
		convertFromObjects' [] = return Nothing;
		convertFromObjects' [obj] = do
			{
			a <- convertFromObject obj;
			return (Just a);
			};
		convertFromObjects' _ = throwSimpleError "too-many-args";
		};

	instance
		(
		Scheme m r,
		MonadMaybeA m a (Object r m)
		) =>
	 ArgumentList m r [a] where
		{
		convertFromObjects' [] = return [];
		convertFromObjects' (obj:objs) = do
			{
			a <- convertFromObject obj;
			as <- convertFromObjects objs;
			return (a:as);
			};
		};

	convertToMacro ::
		(
		ArgumentList m r args,
		?refType :: Type (r ())
		) =>
	 (args -> m result) ->
	 ([Object r m] -> m result);
	convertToMacro foo objs = do
		{
		args <- convertFromObjects objs;
		foo args;
		};

	convertToProcedure ::
		(
		ArgumentList m r args,
		MonadIsA m (Object r m) ret,
		?refType :: Type (r ())
		) =>
	 (args -> m ret) -> Procedure r m;
	convertToProcedure foo objs = do
		{
		r <- convertToMacro foo objs;
		getConvert r;
		};
	}
