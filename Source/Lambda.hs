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

module Lambda where
	{
	import TopLevel;
	import Evaluate;
	import Procedures;
	import Conversions;
	import Object;
	import HBase;

	-- 4.2.3 Sequencing
	begin ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [Object r m] -> m (Object r m);
	begin bindings [] = fail "not enough arguments";
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
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	beginM Type objs = begin ?bindings objs;

	-- 4.2.2 Binding Constructs
	accrueBindings ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> m (Bindings r m);
	accrueBindings original s = foo original s where
		{
		foo bindings [] = return bindings;
		foo bindings ((name,(object,())):binds) = do
			{
			result <- evaluate object with {?bindings=original};
			loc <- new result;
			foo (newBinding bindings name loc) binds;
			};
		};

	accrueBindingsStar ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> m (Bindings r m);
	accrueBindingsStar bindings [] = return bindings;
	accrueBindingsStar bindings ((name,(object,())):binds) = do
		{
		result <- evaluate object with {?bindings=bindings};
		loc <- new result;
		accrueBindingsStar (newBinding bindings name loc) binds;
		};
	
	letf ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> [Object r m] -> m (Object r m);
	letf bindings newbinds body = do
		{
		bindings' <- accrueBindings bindings newbinds;
		begin bindings' body;
		};
	
	letStar ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [(Symbol,(Object r m,()))] -> [Object r m] -> m (Object r m);
	letStar bindings newbinds body = do
		{
		bindings' <- accrueBindingsStar bindings newbinds;
		begin bindings' body;
		};
	
	letM ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letM Type (newbinds,body) = letf ?bindings newbinds body;
	
	letStarM ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letStarM Type (newbinds,body) = letStar ?bindings newbinds body;

	-- 4.1.4 Procedures
	matchBinding :: (Scheme x m r) =>
	 Bindings r m -> Object r m -> Object r m -> m (Bindings r m);
	matchBinding bindings (SymbolObject name) arg = do
		{
		loc <- new arg;
		return (newBinding bindings name loc);
		};
	matchBinding bindings NilObject arg = do
		{
		() <- convertFromObject arg;
		return bindings;
		};
	matchBinding bindings (PairObject hloc tloc) arg = do
		{
		(argh,argt) <- convertFromObject arg;
		head <- get hloc;
		bindings' <- matchBinding bindings head argh;
		tail <- get tloc;
		bindings'' <- matchBinding bindings' tail argt;
		return bindings'';
		};

	matchBindings :: (Scheme x m r) =>
	 Bindings r m -> Object r m -> [Object r m] -> m (Bindings r m);
	matchBindings bindings (SymbolObject name) args = do
		{
		argList <- getConvert args;
		argListLoc <- new argList;
		return (newBinding bindings name argListLoc);
		};
	matchBindings bindings NilObject args = do
		{
		() <- convertFromObjects args;
		return bindings;
		};
	matchBindings bindings (PairObject hloc tloc) args = do
		{
		(argh,argt) <- convertFromObjects args;
		head <- get hloc;
		bindings' <- matchBinding bindings head argh;
		tail <- get tloc;
		bindings'' <- matchBindings bindings' tail argt;
		return bindings'';
		};

	lambda :: (Scheme x m r) => Object r m -> [Object r m] -> m (Procedure r m);
	lambda argNames body = do
		{
		return (\bindings args -> do
			{
			bindings' <- matchBindings bindings argNames args;
			begin bindings' body;
			});
		};

	lambdaM :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,[Object r m]) -> m (Procedure r m);
	lambdaM Type (argBindings,body) = lambda argBindings body;

	-- 6.4 Control Features
	isProcedure :: (Scheme x m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureP :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isProcedureP Type (obj,()) = isProcedure obj;
	
	callCCP ::
		(
		Scheme x m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> (Procedure r m,()) -> m (Object r m);
	callCCP Type (proc,()) = callCC (\cont -> proc ?bindings
	  [ProcedureObject (\_ args -> do
	  	{
	  	(resultArg,()) <- convertFromObjects args;
	  	cont resultArg;
	  	})]);
	}
