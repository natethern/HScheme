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

	data SchemeGCPSError p mobj = 
		GCPSStringError String				|
		GCPSExceptionError Exception		|
		GCPSObjError (mobj (SchemeGCPS p mobj))	;

	type SchemeGCPS p mobj = ExceptionMonad (GuardContinuationPass Unique p) (SchemeGCPSError p mobj);

	type SchemeGCPSObject p mobj = mobj (SchemeGCPS p mobj);

	instance Show (SchemeGCPSError p mobj) where
		{
		show (GCPSStringError s) = s;
		show (GCPSExceptionError ex) = show ex;
		show (GCPSObjError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeGCPSObject p mobj) (SchemeGCPS p mobj) where
		{
		throw = throw . GCPSObjError;
		};

	instance
		(
		ObjectSubtype r (SchemeGCPSObject p mobj) Symbol,
		ObjectSubtype r (SchemeGCPSObject p mobj) (SList Char),
		Build (SchemeGCPS p mobj) r
		) =>
	 MonadException (SchemeGCPSObject p mobj) (SchemeGCPS p mobj) where
		{
		catch foo cc = catchSingle foo (\ex -> do
			{
			obj <- getConvert ex;
			cc obj;
			});
		};

	instance (Monad m) =>
	 MonadIsA m (SchemeGCPSError p mobj) (SchemeGCPSObject p mobj) where
		{
		getConvert = return . GCPSObjError;
		};

	instance
		(
		ObjectSubtype r (SchemeGCPSObject p mobj) Symbol,
		ObjectSubtype r (SchemeGCPSObject p mobj) (SList Char),
		Build (SchemeGCPS p mobj) r
		) =>
	 MonadIsA (SchemeGCPS p mobj) (SchemeGCPSObject p mobj) (SchemeGCPSError p mobj) where
		{
		getConvert (GCPSObjError a) = return a;
		getConvert (GCPSExceptionError x) = getObject (MkSymbol "failure",MkSList (show x));
		getConvert (GCPSStringError s) = getObject (MkSymbol "failure",MkSList s);
		};

	instance MaybeA (SchemeGCPSError p mobj) String where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeGCPSError p mobj) String where
		{
		convert = GCPSStringError;
		};

	instance MaybeA (SchemeGCPSError p mobj) Exception where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeGCPSError p mobj) Exception where
		{
		convert = GCPSExceptionError;
		};

	instance (MonadException (SchemeGCPSObject (cm ()) mobj) cm) =>
	 Runnable cm (SchemeGCPS (cm ()) mobj) where
		{
		rsRun ma = runMonad return
		 (exRun (\err -> lift (case err of
			{
			GCPSStringError s -> fail s;
			GCPSExceptionError ex -> fail (show ex);
			GCPSObjError obj -> throw obj;
			})) ma);
		};

	instance
		(
		Build cm r,
		MonadException (SchemeGCPSObject (cm ()) mobj) cm,
		ListObject r (SchemeGCPSObject (cm ()) mobj)
		) =>
	 RunnableScheme cm (SchemeGCPS (cm ()) mobj) r (SchemeGCPSObject (cm ()) mobj) where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . lift . outproc);
			rsRun (mrun program);
			};
		};
	}
