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

module Syntax where
	{
--	import Evaluate;
	import Conversions;
	import Object;
	import Subtype;
	import Type;

	substitute :: (Scheme x m r) =>
	 [(Symbol,Object r m)] -> Object r m -> m (Object r m);
	substitute [] x = return x;
	substitute subs (PairObject hloc tloc) = do
		{
		head <- getLocation hloc;
		head' <- substitute subs head;
		hloc' <- newLocation head';
		tail <- getLocation tloc;
		tail' <- substitute subs tail;
		tloc' <- newLocation tail';
		return (PairObject hloc' tloc')
		};
	substitute subs (VectorObject (loc:locs)) = do
		{
		head <- getLocation loc;
		head' <- substitute subs head;
		loc' <- newLocation head';
		VectorObject locs' <- substitute subs (VectorObject locs);
		return (VectorObject (loc':locs'));
		};
	substitute ((patvar,sub):subs) (SymbolObject name) =
	 if name == patvar
	 then return sub
	 else substitute subs (SymbolObject name);
	substitute _ x = return x;

	matchBinding :: (Scheme x m r) =>
	 [Symbol] -> Object r m -> Object r m -> m (Maybe [(Symbol,Object r m)]);
	matchBinding literals NilObject NilObject = return (Just []);
	matchBinding literals NilObject _ = return Nothing;
	matchBinding literals (PairObject phl ptl) (PairObject ahl atl) = do
		{
		ph <- getLocation phl;
		ah <- getLocation ahl;
		mhmatch <- matchBinding literals ph ah;
		case mhmatch of
			{
			Nothing -> return Nothing;
			Just hmatch -> do
				{
				pt <- getLocation ptl;
				at <- getLocation atl;
				mtmatch <- matchBinding literals pt at;
				case mtmatch of
					{
					Nothing -> return Nothing;
					Just tmatch -> return (Just (hmatch ++ tmatch));
					};
				};
			};
		};
	matchBinding literals (PairObject _ _) _ = return Nothing;
	matchBinding literals (SymbolObject name) args =
	 if elem name literals
	 then case args of
	 	{
	 	SymbolObject name' | name == name' -> return (Just []);
	 	_ -> return Nothing;
	 	}
	 else return (Just [(name,args)]);
	matchBinding literals _ _ = return Nothing;
	
	matchBindings :: (Scheme x m r) =>
	 [Symbol] -> Object r m -> [Object r m] -> m (Maybe [(Symbol,Object r m)]);
	matchBindings literals NilObject [] = return (Just []);
	matchBindings literals (PairObject phl ptl) (a1:as) = do
		{
		ph <- getLocation phl;
		mmatch1 <- matchBinding literals ph a1;
		case mmatch1 of
			{
			Nothing -> return Nothing;
			Just match1 -> do
				{
				pt <- getLocation ptl;
				mmatchs <- matchBindings literals pt as;
				case mmatchs of
					{
					Nothing -> return Nothing;
					Just matchs -> return (Just (match1 ++ matchs));
					};
				};
			};
		};
	matchBindings literals (SymbolObject name) args =
	 if elem name literals
	 then return Nothing
	 else do
		{
		argList <- getConvert args;
		return (Just [(name,argList)]);
		};
	matchBindings literals _ _ = return Nothing;

	syntaxRulesM :: (Scheme x m r) =>
	 Type (r ()) -> ([Symbol],[((Symbol,Object r m),(Object r m,()))]) -> m (Syntax r m);
	syntaxRulesM Type (literals,rules) = do
		{
		return (\args -> let
			{
			transform [] = fail "can't match args";
			transform (((_,pattern),(template,())):rs) = do
				{
				msubs <- matchBindings literals pattern args;
				case msubs of
					{
					Nothing -> transform rs;
					Just subs -> substitute subs template;
					};
				};
			} in transform rules);
		};
	}
