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

	newtype SchemeGCPSError p mobj = MkSchemeGCPSError (mobj (SchemeGCPS p mobj));

	type SchemeGCPS p mobj = ExceptionMonad (GuardContinuationPass Unique p) (SchemeGCPSError p mobj);

	type SchemeGCPSObject p mobj = mobj (SchemeGCPS p mobj);

	instance Show (SchemeGCPSError p mobj) where
		{
		show (MkSchemeGCPSError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeGCPSObject p mobj) (SchemeGCPS p mobj) where
		{
		throw = throw . MkSchemeGCPSError;
		};

	instance MonadException (SchemeGCPSObject p mobj) (SchemeGCPS p mobj) where
		{
		catch foo cc = catchSingle foo (\(MkSchemeGCPSError obj) -> cc obj);
		};

	instance (MonadThrow (mobj (SchemeGCPS (IO ()) mobj)) IO) =>
	 MonadThrow (SchemeGCPSError (IO ()) mobj) IO where
		{
		throw (MkSchemeGCPSError obj) = throw obj;
		};

	instance
		(
		Build cm r,
		MonadThrow (SchemeGCPSError (cm ()) mobj) cm,
		ListObject r (SchemeGCPSObject (cm ()) mobj)
		) =>
	 RunnableScheme cm (SchemeGCPS (cm ()) mobj) r (SchemeGCPSObject (cm ()) mobj) where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . lift . outproc);
			convertMonad (mrun program);
			};
		};
	}
