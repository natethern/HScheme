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

	data SchemeCPSError p mobj = 
		StringError String				|
		ExceptionError Exception		|
		ObjError (mobj (SchemeCPS p mobj))	;

	type SchemeCPS p mobj = ExceptionContinuationPass p (SchemeCPSError p mobj);

	type SchemeCPSObject p mobj = mobj (SchemeCPS p mobj);

	instance Show (SchemeCPSError p mobj) where
		{
		show (StringError s) = s;
		show (ExceptionError ex) = show ex;
		show (ObjError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeCPSObject p mobj) (SchemeCPS p mobj) where
		{
		throw = throw . ObjError;
		};

	instance
		(
		ObjectSubtype r (SchemeCPSObject p mobj) Symbol,
		ObjectSubtype r (SchemeCPSObject p mobj) (SList Char),
		Build (SchemeCPS p mobj) r
		) =>
	 MonadException (SchemeCPSObject p mobj) (SchemeCPS p mobj) where
		{
		catch foo cc = catchSingle foo (\ex -> do
			{
			obj <- getConvert ex;
			cc obj;
			});
		};

	instance (Monad m) =>
	 MonadIsA m (SchemeCPSError p mobj) (SchemeCPSObject p mobj) where
		{
		getConvert = return . ObjError;
		};

	instance
		(
		ObjectSubtype r (SchemeCPSObject p mobj) Symbol,
		ObjectSubtype r (SchemeCPSObject p mobj) (SList Char),
		Build (SchemeCPS p mobj) r
		) =>
	 MonadIsA (SchemeCPS p mobj) (SchemeCPSObject p mobj) (SchemeCPSError p mobj) where
		{
		getConvert (ObjError a) = return a;
		getConvert (ExceptionError x) = getObject (MkSymbol "failure",MkSList (show x));
		getConvert (StringError s) = getObject (MkSymbol "failure",MkSList s);
		};

	instance MaybeA (SchemeCPSError p mobj) String where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeCPSError p mobj) String where
		{
		convert = StringError;
		};

	instance MaybeA (SchemeCPSError p mobj) Exception where
		{
		maybeConvert = Just . convert;
		};

	instance IsA (SchemeCPSError p mobj) Exception where
		{
		convert = ExceptionError;
		};

	instance (MonadException (SchemeCPSObject (cm ()) mobj) cm) =>
	 Runnable cm (SchemeCPS (cm ()) mobj) where
		{
		rsRun ma = runExceptionContinuationPass
		 (\err -> case err of
			{
			StringError s -> fail s;
			ExceptionError ex -> fail (show ex);
			ObjError obj -> throw obj;
			}) return ma;
		};

	instance
		(
		Build cm r,
		MonadException (SchemeCPSObject (cm ()) mobj) cm,
		ListObject r (SchemeCPSObject (cm ()) mobj)
		) =>
	 RunnableScheme cm (SchemeCPS (cm ()) mobj) r (SchemeCPSObject (cm ()) mobj) where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . outproc);
			rsRun (mrun program);
			};
		};
	}
