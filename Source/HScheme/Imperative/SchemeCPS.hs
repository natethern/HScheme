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

	newtype SchemeCPSError p mobj = MkSchemeCPSError (mobj (SchemeCPS p mobj));

	type SchemeCPS p mobj = ExceptionContinuationPass p (SchemeCPSError p mobj);

	type SchemeCPSObject p mobj = mobj (SchemeCPS p mobj);

	instance Show (SchemeCPSError p mobj) where
		{
		show (MkSchemeCPSError ex) = "Scheme object error";
		};

	instance MonadThrow (SchemeCPSObject p mobj) (SchemeCPS p mobj) where
		{
		throw = throw . MkSchemeCPSError;
		};

	instance MonadException (SchemeCPSObject p mobj) (SchemeCPS p mobj) where
		{
		catch foo cc = catchSingle foo (\(MkSchemeCPSError obj) -> cc obj);
		};

	instance (MonadThrow (mobj (SchemeCPS (IO ()) mobj)) IO) =>
	 MonadThrow (SchemeCPSError (IO ()) mobj) IO where
		{
		throw (MkSchemeCPSError obj) = throw obj;
		};

	instance
		(
		Build cm r,
		MonadThrow (SchemeCPSError (cm ()) mobj) cm,
		ListObject r (SchemeCPSObject (cm ()) mobj)
		) =>
	 RunnableScheme cm (SchemeCPS (cm ()) mobj) r (SchemeCPSObject (cm ()) mobj) where
		{
		rsRunInterp outproc mrun interpList interpEat = do
			{
			program <- interpEat (lift . outproc);
			convertMonad (mrun program);
			};
		};
	}
