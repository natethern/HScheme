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
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	-- 4.2.3 Sequencing
	begin :: (Scheme m r) =>
	 Bindings r m -> [Object r m] -> m (Object r m);
	begin bindings objs@[] = let {?bindings=bindings} in throwSimpleError "too-few-args-in-begin";
	begin bindings [obj] = do
		{
		(_,result) <- topLevelEvaluate bindings obj;
		return result;
		};
	begin bindings (obj:objs) = do
		{
		(newBindings,_) <- topLevelEvaluate bindings obj;
		begin newBindings objs;
		};

	beginM ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 [Object r m] -> m (Object r m);
	beginM objs = begin ?bindings objs;

	-- 4.2.2 Binding Constructs
	accrueBindings ::
		(
		Scheme m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> m (Bindings r m);
	accrueBindings original s = foo original s where
		{
		foo bindings [] = return bindings;
		foo bindings ((name,(object,())):binds) = do
			{
			result <- let {?bindings=original} in evaluate object;
			loc <- new result;
			foo (newBinding bindings name loc) binds;
			};
		};

	accrueBindingsStar ::
		(
		Scheme m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> m (Bindings r m);
	accrueBindingsStar bindings [] = return bindings;
	accrueBindingsStar bindings ((name,(object,())):binds) = do
		{
		result <- let {?bindings=bindings} in evaluate object;
		loc <- new result;
		accrueBindingsStar (newBinding bindings name loc) binds;
		};
	
	letf ::
		(
		Scheme m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> [Object r m] -> m (Object r m);
	letf bindings newbinds body = do
		{
		bindings' <- accrueBindings bindings newbinds;
		begin bindings' body;
		};
	
	letStar ::
		(
		Scheme m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> [Object r m] -> m (Object r m);
	letStar bindings newbinds body = do
		{
		bindings' <- accrueBindingsStar bindings newbinds;
		begin bindings' body;
		};
	
	letM ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letM (newbinds,body) = letf ?bindings newbinds body;
	
	letStarM ::
		(
		Scheme m r,
		?bindings		:: Bindings r m
		) =>
	 ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letStarM (newbinds,body) = letStar ?bindings newbinds body;

	-- 4.1.4 Procedures
	matchBinding :: (Scheme m r) =>
	 Bindings r m -> Object r m -> Object r m -> m (Bindings r m);
	matchBinding bindings (SymbolObject name) arg = do
		{
		loc <- new arg;
		return (newBinding bindings name loc);
		};
	matchBinding bindings NilObject arg = do
		{
		() <- let {?bindings=bindings} in convertFromObject arg;
		return bindings;
		};
	matchBinding bindings (PairObject hloc tloc) arg = do
		{
		(argh,argt) <- let {?bindings=bindings} in convertFromObject arg;
		head <- get hloc;
		bindings' <- matchBinding bindings head argh;
		tail <- get tloc;
		bindings'' <- matchBinding bindings' tail argt;
		return bindings'';
		};
	matchBinding bindings _ arg = let {?bindings=bindings} in throwSimpleError "no-match-to-bindings";

	matchBindings :: (Scheme m r) =>
	 Bindings r m -> Object r m -> [Object r m] -> m (Bindings r m);
	matchBindings bindings (SymbolObject name) args = do
		{
		argList <- getConvert args;
		argListLoc <- new argList;
		return (newBinding bindings name argListLoc);
		};
	matchBindings bindings NilObject args = do
		{
		() <- let {?bindings=bindings} in convertFromObjects args;
		return bindings;
		};
	matchBindings bindings (PairObject hloc tloc) args = do
		{
		(argh,argt) <- let {?bindings=bindings} in convertFromObjects args;
		head <- get hloc;
		bindings' <- matchBinding bindings head argh;
		tail <- get tloc;
		bindings'' <- matchBindings bindings' tail argt;
		return bindings'';
		};
	matchBindings bindings _ args = let {?bindings=bindings} in throwSimpleError "no-match-to-bindings";

	mergeBindings :: Bindings r m -> Bindings r m -> Bindings r m;
	mergeBindings b1 b2 = MkBindings
		{
		newBinding = \sym loc -> mergeBindings (newBinding b1 sym loc) b2,
		getBinding = \sym -> case (getBinding b1 sym) of
			{
			Nothing -> getBinding b2 sym;
			r -> r;
			}
		};

	lambda :: (Scheme m r,?bindings :: Bindings r m) =>
	 Object r m -> [Object r m] -> m (Procedure r m);
	lambda argNames body = do
		{
		return (\bindings args -> do
			{
			-- lambda-time bindings take priority over runtime
			bindings' <- matchBindings (mergeBindings ?bindings bindings) argNames args;
			begin bindings' body;
			});
		};

	lambdaM :: (Scheme m r,?bindings :: Bindings r m) =>
	 (Object r m,[Object r m]) -> m (Procedure r m);
	lambdaM (argBindings,body) = lambda argBindings body;

	-- 6.4 Control Features
	isProcedure :: (Scheme m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureP :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) -> m Bool;
	isProcedureP (obj,()) = isProcedure obj;
	
	callCCP ::
		(
		Scheme m r,?refType :: Type (r ()),
		MonadCont m,
		?bindings :: Bindings r m
		) =>
	 (Procedure r m,()) -> m (Object r m);
	callCCP (proc,()) = callCC (\cont -> proc ?bindings
	  [ProcedureObject (\_ args -> do
	  	{
	  	(resultArg,()) <- convertFromObjects args;
	  	cont resultArg;
	  	})]);
	
	fixP ::
		(
		Scheme m r,?refType :: Type (r ()),
		MonadFix m,
		?bindings :: Bindings r m
		) =>
	 (Procedure r m,()) -> m (Object r m);
	fixP (proc,()) = mfix (\a -> proc ?bindings [a]);
	}
