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
	import Org.Org.Semantic.HScheme.Interpret.Assemble;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	type ArgumentMatchMonad obj = Result (ArgumentMismatch obj);

	class
		(
		ListObject r obj
		) =>
	 ArgumentList r obj a | obj -> r where
		{
		maybeConvertFromObjects :: forall cm. (Build cm r,?objType :: Type obj) =>
		 Int -> [obj] -> cm (ArgumentMatchMonad obj a);
		};

	convertFromObjects ::
		(
		ProcedureError cm obj,
		Build cm r,
		ArgumentList r obj a,
		?objType :: Type obj
		) =>
	 [obj] -> cm a;
	convertFromObjects args = do
		{
		ma <- maybeConvertFromObjects 0 args;
		case ma of
			{
			SuccessResult a -> return a;
			ExceptionResult am -> throwArgumentListMismatchError am;
			};
		};

	instance (ListObject r obj) =>
	 ArgumentList r obj () where
		{
		maybeConvertFromObjects _ [] = return (return ());
		maybeConvertFromObjects pos args = return (throwSingle (TooManyArguments pos args));
		};

	instance
		(
		ObjectSubtype r obj a,
		ArgumentList r obj b
		) =>
 	 ArgumentList r obj (a,b) where
		{
		maybeConvertFromObjects pos [] = return (throwSingle (TooFewArguments pos));
		maybeConvertFromObjects pos (obj:objs) = do
			{
			ma <- resultFromObject obj;
			mb <- maybeConvertFromObjects (pos + 1) objs;
			return (liftF2 (,) (fmap2 (WrongArgumentType pos) ma) mb);
			};
		};

	instance
		(
		ObjectSubtype r obj a
		) =>
	 ArgumentList r obj (Maybe a) where
		{
		maybeConvertFromObjects pos [] = return (return Nothing);
		maybeConvertFromObjects pos [obj] = do
			{
			ma <- resultFromObject obj;
			return (fmap2 (WrongArgumentType pos) (fmap Just ma));
			};
		maybeConvertFromObjects pos args = return (throwSingle (TooManyArguments pos args));
		};

	instance
		(
		ObjectSubtype r obj a
		) =>
	 ArgumentList r obj [a] where
		{
		maybeConvertFromObjects pos [] = return (return []);
		maybeConvertFromObjects pos (obj:objs) = do
			{
			ma <- resultFromObject obj;
			mas <- maybeConvertFromObjects (pos + 1) objs;
			return (liftF2 (:) (fmap2 (WrongArgumentType pos) ma) mas);
			};
		};

	instance
		(
		ArgumentList r obj a,
		ArgumentList r obj b
		) =>
	 ArgumentList r obj (Either a b) where
		{
		maybeConvertFromObjects pos args = do
			{
			ma <- maybeConvertFromObjects pos args;
			case ma of
				{
				SuccessResult a -> return (SuccessResult (Left a));
				ExceptionResult exa -> do
					{
					mb <- maybeConvertFromObjects pos args;
					case mb of
						{
						SuccessResult b -> return (SuccessResult (Right b));
						ExceptionResult exb -> return (ExceptionResult (UnionArgMismatch exa exb));
						};
					};
				};
			};
		};

	convertPLToProcedure ::
		(
		InterpretObject m r obj,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 (args -> m [obj]) -> Procedure obj m;
	convertPLToProcedure foo argObjs = do
		{
		args <- convertFromObjects argObjs;
		foo args;
		};

	convertPNToProcedure ::
		(
		InterpretObject m r obj,
		ArgumentList r obj args,
		?objType :: Type obj
		) =>
	 (args -> m ()) -> Procedure obj m;
	convertPNToProcedure foo = convertPLToProcedure (\args -> do
		{
		foo args;
		return [];
		});

	convertToProcedure ::
		(
		InterpretObject m r obj,
		ArgumentList r obj args,
		ObjectSubtype r obj ret,
		?objType :: Type obj
		) =>
	 (args -> m ret) -> Procedure obj m;
	convertToProcedure foo = convertPLToProcedure (\args -> do
		{
		r <- foo args;
		resultObj <- getObject r;
		return [resultObj];
		});
	}
