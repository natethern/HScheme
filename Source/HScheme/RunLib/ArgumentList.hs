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

	data ArgumentMismatch obj =
	 UnionArgMismatch (ArgumentMismatch obj) (ArgumentMismatch obj)	|
	 TooFewArguments Int					|
	 TooManyArguments Int [obj]				|
	 WrongArgumentType Int (Mismatch obj)	;

	instance (Build cm r) =>
	 MonadIsA cm (Object r m) (ArgumentMismatch (Object r m)) where
		{
		getConvert (UnionArgMismatch am1 am2) = do
			{
			am1Obj <- getConvert am1;
			am2Obj <- getConvert am2;
			makeList [am1Obj,am2Obj];
			};
		getConvert (TooFewArguments pos) = getObject (MkSymbol "too-few-arguments",(pos,()));
		getConvert (TooManyArguments pos list) = getObject (MkSymbol "too-few-arguments",(pos,(list,())));
		getConvert (WrongArgumentType pos mm) = do
			{
			(mObj :: Object r m) <- getConvert mm;
			getObject (MkSymbol "wrong-argument-type",(pos,(mObj,())))
			};
		};

	type ArgumentMatchMonad obj = Result (ArgumentMismatch obj);

	class (SchemeObject r obj) =>
	 ArgumentList r obj a | obj -> r where
		{
		maybeConvertFromObjects :: forall cm. (Build cm r,?objType :: Type obj) =>
		 Int -> [obj] -> cm (ArgumentMatchMonad obj a);
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
		ma <- maybeConvertFromObjects 0 args;
		case ma of
			{
			SuccessResult a -> return a;
			ExceptionResult am -> do
				{
				amObj <- getConvert am;
				throwArgError "arg-list-mismatch" [amObj];
				};
			};
		};

	instance (SchemeObject r obj) =>
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
			ma <- fromObject obj;
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
			ma <- fromObject obj;
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
			ma <- fromObject obj;
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
		BuildThrow m (Object r m) r,
		ArgumentList r (Object r m) args,
		?objType :: Type (Object r m)
		) =>
	 (args -> m [Object r m]) -> Procedure (Object r m) m;
	convertPLToProcedure foo argObjs = do
		{
		args <- convertFromObjects argObjs;
		foo args;
		};

	convertPNToProcedure ::
		(
		BuildThrow m (Object r m) r,
		ArgumentList r (Object r m) args,
		?objType :: Type (Object r m)
		) =>
	 (args -> m ()) -> Procedure (Object r m) m;
	convertPNToProcedure foo = convertPLToProcedure (\args -> do
		{
		foo args;
		return [];
		});

	convertToProcedure ::
		(
		BuildThrow m (Object r m) r,
		ArgumentList r (Object r m) args,
		ObjectSubtype r (Object r m) ret,
		?objType :: Type (Object r m)
		) =>
	 (args -> m ret) -> Procedure (Object r m) m;
	convertToProcedure foo = convertPLToProcedure (\args -> do
		{
		r <- foo args;
		resultObj <- getObject r;
		return [resultObj];
		});
	}
