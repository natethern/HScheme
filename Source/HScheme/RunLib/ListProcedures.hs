-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>

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

module Org.Org.Semantic.HScheme.RunLib.ListProcedures where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;


	-- 6.3.2 Pairs and Lists

	isPairP :: (Monad m,ListObject r obj,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isPairP (obj,()) = return (case (objectCell obj) of
		{
		Just (Just _) -> True;
		_ -> False;
		});

	consP :: (Build m r,ListObject r obj,?objType :: Type obj) =>
	 (obj,(obj,())) -> m obj;
	consP (h,(t,())) = cons h t;

	carP :: (Monad m,?objType :: Type obj) =>
	 ((obj,obj),()) -> m obj;
	carP ((h,_),()) = return h;

	cdrP :: (Monad m,?objType :: Type obj) =>
	 ((obj,obj),()) -> m obj;
	cdrP ((_,t),()) = return t;

	isNilP :: (Monad m,ListObject r obj,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isNilP (obj,()) = return (case objectCell obj of
		{
		Just Nothing -> True;
		_ -> False;
		});

	listP ::  (Monad m,?objType :: Type obj) =>
	 [obj] -> m [obj];
	listP list = return list;

	isListP ::
		(
		Build m r,
		Eq (r obj),
		ListObject r obj,
		?objType :: Type obj
		) =>
	 (obj,()) -> m Bool;
	isListP (obj,()) = case (getTailRef obj) of
		{
		SuccessResult rt -> checkLoop rt rt;
		ExceptionResult res -> return res;
		} where
		{
		getTailRef :: (ListObject r obj) => obj -> Result Bool (r obj);
		getTailRef x = case (objectCell x) of
			{
			Just (Just (_,r)) -> return r;
			Just _ -> throw True;	-- finite list
			_ -> throw False;		-- improper
			};

		checkLoop ::
			(
			Build m r,
			Eq (r obj),
			ListObject r obj,
			?objType :: Type obj
			) =>
		 r obj -> r obj -> m Bool;
		checkLoop ra rb = do
			{
			b <- get rb;
			case (getTailRef b) of
				{
				SuccessResult rb' -> if (ra == rb')
				 then return False		-- circular
				 else do
					{
					b' <- get rb';
					case (getTailRef b') of
						{
						SuccessResult rb'' -> do
							{
							a <- get ra;
							case (getTailRef a) of
								{
								SuccessResult ra' -> checkLoop ra' rb'';
								ExceptionResult res -> return res;
								}
							};
						ExceptionResult res -> return res;
						}
					};
				ExceptionResult res -> return res;
				}
			};
		};
	}
