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

module Org.Org.Semantic.HScheme.Lambda where
	{
	import Org.Org.Semantic.HScheme.ArgumentList;
--	import Org.Org.Semantic.HScheme.TopLevel;
--	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	-- 6.4 Control Features
	isProcedure :: (Scheme m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isProcedureP (obj,()) = isProcedure obj;
	
	callCCP ::
		(
		Scheme m r,?objType :: Type (Object r m),
		MonadCont m
		) =>
	 (Procedure r m,()) -> m (Object r m);
	callCCP (proc,()) = callCC (\cont -> proc
	  [ProcedureObject (\args -> do
	  	{
	  	(resultArg,()) <- convertFromObjects args;
	  	cont resultArg;
	  	})]);
	
	fixP ::
		(
		Scheme m r,?objType :: Type (Object r m),
		MonadFix m
		) =>
	 (Procedure r m,()) -> m (Object r m);
	fixP (proc,()) = mfix (\a -> proc [a]);
	}
