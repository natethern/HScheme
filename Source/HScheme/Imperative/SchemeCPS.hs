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

module Org.Org.Semantic.HScheme.Imperative.SchemeCPS where
	{
	import Org.Org.Semantic.HScheme.MainProg;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	type SchemeCPS r p = ExceptionContinuationPass p (SchemeCPSError r p);

	type SchemeCPSObject r p = Object r (SchemeCPS r p);

	data SchemeCPSError r p = 
		StringError String				|
		ExceptionError Exception		|
		ObjError (SchemeCPSObject r p)	;

	instance Show (SchemeCPSError r p) where
		{
		show (StringError s) = s;
		show (ExceptionError ex) = show ex;
		show (ObjError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeCPSObject r p) (SchemeCPS r p) where
		{
		throw = throw . ObjError;
		};

	instance
		(
		MonadCreatable (SchemeCPS r p) r,
		MonadGettableReference (SchemeCPS r p) r
		) =>
	 MonadException (SchemeCPSObject r p) (SchemeCPS r p) where
		{
		catch foo cc = catchSingle foo (\ex -> do
			{
			obj <- getConvert ex;
			cc obj;
			});
		};

	instance
		(
		MonadCreatable (SchemeCPS r p) r,
		MonadGettableReference (SchemeCPS r p) r
		) =>
	 MonadIsA (SchemeCPS r p) (SchemeCPSError r p) (SchemeCPSObject r p) where
		{
		getConvert = return . ObjError;
		};

	instance
		(
		MonadCreatable (SchemeCPS r p) r,
		MonadGettableReference (SchemeCPS r p) r
		) =>
	 MonadIsA (SchemeCPS r p) (SchemeCPSObject r p) (SchemeCPSError r p) where
		{
		getConvert (ObjError a) = return a;
		getConvert (ExceptionError x) = getConvert (MkSymbol "failure",MkSList (show x));
		getConvert (StringError s) = getConvert (MkSymbol "failure",MkSList s);
		};

	instance MaybeA (SchemeCPSError r p) String where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeCPSError r p) String where
		{
		convert = StringError;
		};

	instance MaybeA (SchemeCPSError r p) Exception where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeCPSError r p) Exception where
		{
		convert = ExceptionError;
		};

	instance
		(
		MonadGettableReference m r
		) =>
	 MonadGettableReference (SchemeCPS r (m p)) r where
		{
		get r = lift ((get :: r a -> m a) r);
		};

	instance
		(
		MonadSettableReference m r
		) =>
	 MonadSettableReference (SchemeCPS r (m p)) r where
		{
		set r v = lift ((set :: r a -> a -> m ()) r v);
		};

	instance
		(
		MonadCreatable m r
		) =>
	 MonadCreatable (SchemeCPS r (m p)) r where
		{
		new a = lift ((new :: a -> m (r a)) a);
		};

	instance
		(
		MonadEqualReference m r
		) =>
	 MonadEqualReference (SchemeCPS r (m p)) r where
		{
		getEqualReference a b = lift ((getEqualReference :: r a -> r a -> m Bool) a b);
		};

	instance
		(
		MonadStandardReference m r
		) =>
	 MonadStandardReference (SchemeCPS r (m a)) r;

	instance
		(
		FunctorApplyReturn m,
		MonadGettableReference m r,
		MonadCreatable m r,
		MonadException (SchemeCPSObject r (m ())) m
		) =>
	 RunnableScheme m (SchemeCPS r (m ())) r where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . outproc);
			rsRun (mrun program);
			} where
			{
			rsRun ma = runExceptionContinuationPass
			 (\err -> case err of
				{
				StringError s -> fail s;
				ExceptionError ex -> fail (show ex);
				ObjError obj -> throw obj;
				}) return ma;
			};
		};
	}
