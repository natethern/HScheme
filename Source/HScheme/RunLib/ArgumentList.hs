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

	class (SchemeObject r obj) =>
	 ArgumentList r obj a | obj -> r where
		{
		maybeConvertFromObjects :: forall cm. (Build cm r,?objType :: Type obj) => [obj] -> cm (Maybe a);
		};

	convertFromObjects ::
		(
		BuildThrow cm (Object r m) r,
		ArgumentList r (Object r m) a,
		?objType :: Type (Object r m)
		) =>
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

	instance (SchemeObject r obj) =>
	 ArgumentList r obj () where
		{
		maybeConvertFromObjects [] = return (Just ());
		maybeConvertFromObjects args = return Nothing;	-- throwArgError "too-many-args" args;
		};

	instance
		(
		ObjectSubtype r obj a,
		ArgumentList r obj b
		) =>
 	 ArgumentList r obj (a,b) where
		{
		maybeConvertFromObjects [] = return Nothing;	-- throwSimpleError "too-few-args";
		maybeConvertFromObjects (obj:objs) = do
			{
			ma <- fromObject obj;
			mb <- maybeConvertFromObjects objs;
			return (liftF2 (,) ma mb);
			};
		};

	instance
		(
		ObjectSubtype r obj a
		) =>
	 ArgumentList r obj (Maybe a) where
		{
		maybeConvertFromObjects [] = return (Just Nothing);
		maybeConvertFromObjects [obj] = do
			{
			ma <- fromObject obj;
			return (fmap Just ma);
			};
		maybeConvertFromObjects args = return Nothing;	-- throwArgError "too-many-args" args;
		};

	instance
		(
		ObjectSubtype r obj a
		) =>
	 ArgumentList r obj [a] where
		{
		maybeConvertFromObjects [] = return (Just []);
		maybeConvertFromObjects (obj:objs) = do
			{
			ma <- fromObject obj;
			mas <- maybeConvertFromObjects objs;
			return (liftF2 (:) ma mas);
			};
		};

	instance
		(
		ArgumentList r obj a,
		ArgumentList r obj b
		) =>
	 ArgumentList r obj (Either a b) where
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
		BuildThrow m (Object r m) r,
		ArgumentList r (Object r m) args,
		ObjectSubtype r (Object r m) ret,
		?objType :: Type (Object r m)
		) =>
	 (args -> m ret) -> Procedure (Object r m) m;
	convertToProcedure foo objs = do
		{
		args <- convertFromObjects objs;
		r <- foo args;
		getObject r;
		};
	}
