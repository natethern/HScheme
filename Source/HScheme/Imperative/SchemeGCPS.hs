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

module Org.Org.Semantic.HScheme.Imperative.SchemeGCPS where
	{
	import Org.Org.Semantic.HScheme.MainProg;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	type SchemeGCPS r p = ExceptionMonad (GuardContinuationPass Unique p) (SchemeGCPSError r p);

	type SchemeGCPSObject r p = Object r (SchemeGCPS r p);

	data SchemeGCPSError r p = 
		GCPSStringError String				|
		GCPSExceptionError Exception		|
		GCPSObjError (SchemeGCPSObject r p)	;

--	type SchemeGCPSError r p = SchemeError (SchemeGCPSObject r p) r p;

	instance Show (SchemeGCPSError r p) where
		{
		show (GCPSStringError s) = s;
		show (GCPSExceptionError ex) = show ex;
		show (GCPSObjError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeGCPSObject r p) (SchemeGCPS r p) where
		{
		throw = throw . GCPSObjError;
		};

	instance
		(
		MonadCreatable (SchemeGCPS r p) r,
		MonadGettableReference (SchemeGCPS r p) r
		) =>
	 MonadException (SchemeGCPSObject r p) (SchemeGCPS r p) where
		{
		catch foo cc = catchSingle foo (\ex -> do
			{
			obj <- getConvert ex;
			cc obj;
			});
		};

	instance
		(
		MonadCreatable (SchemeGCPS r p) r,
		MonadGettableReference (SchemeGCPS r p) r
		) =>
	 MonadIsA (SchemeGCPS r p) (SchemeGCPSError r p) (SchemeGCPSObject r p) where
		{
		getConvert = return . GCPSObjError;
		};

	instance
		(
		MonadCreatable (SchemeGCPS r p) r,
		MonadGettableReference (SchemeGCPS r p) r
		) =>
	 MonadIsA (SchemeGCPS r p) (SchemeGCPSObject r p) (SchemeGCPSError r p) where
		{
		getConvert (GCPSObjError a) = return a;
		getConvert (GCPSExceptionError x) = getObject (MkSymbol "failure",MkSList (show x));
		getConvert (GCPSStringError s) = getObject (MkSymbol "failure",MkSList s);
		};

	instance MaybeA (SchemeGCPSError r p) String where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeGCPSError r p) String where
		{
		convert = GCPSStringError;
		};

	instance MaybeA (SchemeGCPSError r p) Exception where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeGCPSError r p) Exception where
		{
		convert = GCPSExceptionError;
		};

	instance
		(
		MonadGettableReference m r
		) =>
	 MonadGettableReference (SchemeGCPS r (m p)) r where
		{
		get r = lift (lift ((get :: r a -> m a) r));
		};

	instance
		(
		MonadSettableReference m r
		) =>
	 MonadSettableReference (SchemeGCPS r (m p)) r where
		{
		set r v = lift (lift ((set :: r a -> a -> m ()) r v));
		};

	instance
		(
		MonadCreatable m r
		) =>
	 MonadCreatable (SchemeGCPS r (m p)) r where
		{
		new a = lift (lift ((new :: a -> m (r a)) a));
		};

	instance
		(
		MonadEqualReference m r
		) =>
	 MonadEqualReference (SchemeGCPS r (m p)) r where
		{
		getEqualReference a b = lift (lift ((getEqualReference :: r a -> r a -> m Bool) a b));
		};

	instance
		(
		MonadStandardReference m r
		) =>
	 MonadStandardReference (SchemeGCPS r (m a)) r;

	instance
		(
		MonadException (SchemeGCPSObject r (m ())) m
		) =>
	 Runnable m (SchemeGCPS r (m ())) where
		{
		rsRun ma = runGuardContinuationPass return
		 (exRun (\err -> lift (case err of
			{
			GCPSStringError s -> fail s;
			GCPSExceptionError ex -> fail (show ex);
			GCPSObjError obj -> throw obj;
			})) ma);
		};

	instance
		(
		MonadGettableReference m r,
		MonadCreatable m r,
		MonadException (SchemeGCPSObject r (m ())) m
		) =>
	 RunnableScheme m (SchemeGCPS r (m ())) r where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . lift . outproc);
			rsRun (mrun program);
			};
		};
	}
