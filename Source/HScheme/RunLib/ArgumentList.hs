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

module Org.Org.Semantic.HScheme.RunLib.ArgumentList where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class (Build cm r,MonadThrow (Object r m) cm) => ArgumentList cm m r a where
		{
		convertFromObjects' :: (?objType :: Type (Object r m)) => [Object r m] -> cm a;
		};

	convertFromObjects :: (ArgumentList cm m r a,?objType :: Type (Object r m)) =>
	 [Object r m] -> cm a;
	convertFromObjects = convertFromObjects';

	instance (Build cm r,MonadThrow (Object r m) cm) => ArgumentList cm m r () where
		{
		convertFromObjects' [] = return ();
		convertFromObjects' (_:_) = throwSimpleError "too-many-args";
		};

	convertFromObject :: (Build cm r,MonadThrow (Object r m) cm,MonadMaybeA cm to (Object r m),?objType :: Type (Object r m)) =>
	 (Object r m) -> cm to;
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
		MonadMaybeA cm a (Object r m),
		ArgumentList cm m r b
		) =>
	 ArgumentList cm m r (a,b) where
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
		Build cm r,
		MonadThrow (Object r m) cm,
		MonadMaybeA cm a (Object r m)
		) =>
	 ArgumentList cm m r (Maybe a) where
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
		Build cm r,
		MonadThrow (Object r m) cm,
		MonadMaybeA cm a (Object r m)
		) =>
	 ArgumentList cm m r [a] where
		{
		convertFromObjects' [] = return [];
		convertFromObjects' (obj:objs) = do
			{
			a <- convertFromObject obj;
			as <- convertFromObjects objs;
			return (a:as);
			};
		};

	convertToProcedure ::
		(
		ArgumentList m m r args,
		MonadIsA m (Object r m) ret,
		?objType :: Type (Object r m)
		) =>
	 (args -> m ret) -> Procedure r m;
	convertToProcedure foo objs = do
		{
		args <- convertFromObjects objs;
		r <- foo args;
		getConvert r;
		};
	}
