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
		maybeConvertFromObjects :: (?objType :: Type (Object r m)) => [Object r m] -> cm (Maybe a);
		};

	convertFromObjects :: (ArgumentList cm m r a,?objType :: Type (Object r m)) =>
	 [Object r m] -> cm a;
	convertFromObjects args = do
		{
		ma <- maybeConvertFromObjects args;
		case ma of
			{
			Just a -> return a;
			Nothing -> throwArgError "arg-list-mismatch" args;
			};
		};

	instance (Build cm r,MonadThrow (Object r m) cm) => ArgumentList cm m r () where
		{
		maybeConvertFromObjects [] = return (Just ());
		maybeConvertFromObjects args = return Nothing;	-- throwArgError "too-many-args" args;
		};

	instance
		(
		MonadMaybeA cm a (Object r m),
		ArgumentList cm m r b
		) =>
	 ArgumentList cm m r (a,b) where
		{
		maybeConvertFromObjects [] = return Nothing;	-- throwSimpleError "too-few-args";
		maybeConvertFromObjects (obj:objs) = do
			{
			ma <- getMaybeConvert obj;
			mb <- maybeConvertFromObjects objs;
			return (liftF2 (,) ma mb);
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
		maybeConvertFromObjects [] = return (Just Nothing);
		maybeConvertFromObjects [obj] = do
			{
			ma <- getMaybeConvert obj;
			return (fmap Just ma);
			};
		maybeConvertFromObjects args = return Nothing;	-- throwArgError "too-many-args" args;
		};

	instance
		(
		Build cm r,
		MonadThrow (Object r m) cm,
		MonadMaybeA cm a (Object r m)
		) =>
	 ArgumentList cm m r [a] where
		{
		maybeConvertFromObjects [] = return (Just []);
		maybeConvertFromObjects (obj:objs) = do
			{
			ma <- getMaybeConvert obj;
			mas <- maybeConvertFromObjects objs;
			return (liftF2 (:) ma mas);
			};
		};

	instance
		(
		ArgumentList cm m r a,
		ArgumentList cm m r b
		) =>
	 ArgumentList cm m r (Either a b) where
		{
		maybeConvertFromObjects args = do
			{
			ma <- maybeConvertFromObjects args;
			case ma of
				{
				Just a -> return (Just (Left a));
				_ -> do
					{
					mb <- maybeConvertFromObjects args;
					case mb of
						{
						Just b -> return (Just (Right b));
						_ -> return Nothing;
						};
					};
				};
			};
		};

	convertToProcedure ::
		(
		ArgumentList m m r args,
		MonadIsA m (Object r m) ret,
		?objType :: Type (Object r m)
		) =>
	 (args -> m ret) -> Procedure (Object r m) m;
	convertToProcedure foo objs = do
		{
		args <- convertFromObjects objs;
		r <- foo args;
		getConvert r;
		};
	}
