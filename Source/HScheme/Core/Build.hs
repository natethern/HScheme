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

module Org.Org.Semantic.HScheme.Core.Build where
	{
	import Org.Org.Semantic.HBase;

	class
		(
		MonadCreatable m r,
		MonadGettableReference m r
		) =>
	 Build m r;

	instance
		(
		MonadCreatable m r,
		MonadGettableReference m r
		) =>
	 Build m r;

	class
		(
		Build m r,
		MonadThrow obj m
		) =>
	 BuildThrow m obj r;

	instance
		(
		Build m r,
		MonadThrow obj m
		) =>
	 BuildThrow m obj r;


	-- throw, etc.

	lastResortThrowObject :: (MonadThrow obj m,?objType :: Type obj) =>
	 obj -> m a;
	lastResortThrowObject = throw;

	throwObject :: (MonadThrow obj m,?objType :: Type obj) =>
	 obj -> m a;
	throwObject = lastResortThrowObject;
	}
