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

module Org.Org.Semantic.HScheme.Core.Conversions where
	{
	import Org.Org.Semantic.HScheme.Core.Mismatch;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HBase;

	class HasBooleanType obj where
		{
		isBooleanType :: obj -> Bool;
		};

	class (ListObject r obj) =>
	 ObjectSubtype r obj a | obj -> r where
		{
		getObject :: forall cm. (Build cm r) => a -> cm obj;
		resultFromObject :: forall cm. (Build cm r) => obj -> cm (MatchMonad obj a);

		getObjectIs :: forall cm. (Build cm r) => Type a -> obj -> cm Bool;
		getObjectIs _ obj = do
			{
			(ma :: MatchMonad obj a) <- resultFromObject obj;
			return (isSuccessResult ma);
			};
		};

	maybeToMatch :: Expected -> obj -> Maybe a -> MatchMonad obj a;
	maybeToMatch exp obj ma = case ma of
		{
		Just a -> return a;
		Nothing -> throwSingle (MkMismatch exp obj);
		};

	getMaybeToMatch :: (Functor m) =>
	 Expected -> (obj -> m (Maybe a)) -> obj -> m (MatchMonad obj a);
	getMaybeToMatch exp mc obj = fmap (maybeToMatch exp obj) (mc obj);

	testObject :: (ObjectSubtype r obj a,Build m r) =>
	 (a -> m Bool) -> obj -> m Bool;
	testObject test obj = do
		{
		ma <- resultFromObject obj;
		case ma of
			{
			SuccessResult a -> test a;
			_ -> return False;
			};
		};


	-- Either

	instance
		(
		ObjectSubtype r obj a,
		ObjectSubtype r obj b
		) =>
	 ObjectSubtype r obj (Either a b) where
		{
		getObject (Left a) = getObject a;
		getObject (Right b) = getObject b;
		resultFromObject obj = do
			{
			ma <- resultFromObject obj;
			case ma of
				{
				SuccessResult a -> return (return (Left a));
				ExceptionResult (MkMismatch exa _) -> do
					{
					mb <- resultFromObject obj;
					return (case mb of
						{
						SuccessResult b -> SuccessResult (Right b);
						ExceptionResult (MkMismatch exb _) -> ExceptionResult (MkMismatch (EitherExpected exa exb) obj);
						});
					};
				};
			};
		};

	
	-- NilType

	type NilType = ();

	instance (ListObject r obj) =>
	 ObjectSubtype r obj NilType where
		{
		getObject () = return nilObject;
		resultFromObject obj | Just Nothing <- objectCell obj = return (SuccessResult ());
		resultFromObject obj = mismatch NullExpected obj;
		};

	
	-- Maybe a

	instance (ObjectSubtype r obj a) =>
	 ObjectSubtype r obj (Maybe a) where
		{
		getObject Nothing = getObject ();
		getObject (Just a) = getObject (a,());

		resultFromObject obj = case objectCell obj of
			{
			Just (Just (hloc,tloc)) -> do
				{
				mh <- resultFromObject obj;
				return (do
					{
					(h,()) <- mh;
					return (return h);
					});
				};
			Just Nothing -> return (return Nothing);
			Nothing -> mismatch listExpected obj;
			};
		};

	
	-- []

	instance (ObjectSubtype r obj a) =>
	 ObjectSubtype r obj [a] where
		{
		getObject [] = getObject ();
		getObject (ah:at) = getObject (ah,at);

		resultFromObject obj = case objectCell obj of
			{
			Just (Just (hloc,tloc)) -> do
				{
				mht <- resultFromObject obj;
				return (do
					{
					(h,t) <- mht;
					return (h:t);
					});
				};
			Just Nothing -> return (return []);
			Nothing -> mismatch listExpected obj;
			};
		};

	
	-- PairType

	type PairType = (,);

	instance
		(
		ObjectSubtype r obj ah,
		ObjectSubtype r obj at
		) =>
	 ObjectSubtype r obj (PairType ah at) where
		{
		getObject (ah,at) = do
			{
			objH <- getObject ah;
			objT <- getObject at;
			cons objH objT;
			};
		resultFromObject obj | Just (Just (hloc,tloc)) <- objectCell obj = do
			{
			h <- get hloc;
			t <- get tloc;
			mobjH <- resultFromObject h;
			mobjT <- resultFromObject t;
			return (do
				{	-- this one's in Maybe. Clever, huh?
				objH <- mobjH;
				objT <- mobjT;
				return (objH,objT);
				});
			};
		resultFromObject obj = mismatch PairTypeExpected obj;
		};
	}
