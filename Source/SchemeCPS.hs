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

module SchemeCPS where
	{
	import Conversions;
	import ContinuationPassing;
	import Object;
	import HBase;

	type SchemeCPS r p = ContinuationPass p (SchemeCPSError r p);

	type SchemeCPSObject r p = Object r (SchemeCPS r p);

	data SchemeCPSError r p = 
		StringError String				|
		ExceptionError Exception		|
		ObjError (SchemeCPSObject r p)	;

	instance (Location (SchemeCPS r p) r) =>
	 MonadIsA (SchemeCPS r p) (SchemeCPSError r p) (SchemeCPSObject r p) where
		{
		getConvert = return . ObjError;
		};

	instance (Location (SchemeCPS r p) r) =>
	 MonadIsA (SchemeCPS r p) (SchemeCPSObject r p) (SchemeCPSError r p) where
		{
		getConvert (ObjError a) = return a;
		getConvert (ExceptionError x) = getConvert (MkStringType (show x));
		getConvert (StringError s) = getConvert (MkStringType s);
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
		MonadReference m r
		) =>
	 MonadReference (SchemeCPS r (m p)) r where
		{
		get r = call ((get :: r a -> m a) r);
		set r v = call ((set :: r a -> a -> m ()) r v);
		};

	instance
		(
		MonadCreatableReference m r
		) =>
	 MonadCreatableReference (SchemeCPS r (m p)) r where
		{
		newReference a = call ((newReference :: a -> m (r a)) a);
		};

	instance
		(
		MonadEqualReference m r
		) =>
	 MonadEqualReference (SchemeCPS r (m p)) r where
		{
		getEqualReference a b = call ((getEqualReference :: r a -> r a -> m Bool) a b);
		};

	instance
		(
		MonadStandardReference m r
		) =>
	 MonadStandardReference (SchemeCPS r (m a)) r;
	}
