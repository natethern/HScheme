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
	import Evaluate;
	import Procedures;
	import Object;
	import Subtype;
	import Type;

	begin ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> [Object r m] -> m (Object r m);
	begin bindings [] = fail "not enough arguments";
	begin bindings [obj] = do
		{
		(_,result) <- defineEvaluate bindings obj;
		return result;
		};
	begin bindings (obj:objs) = do
		{
		(newBindings,_) <- defineEvaluate bindings obj;
		begin newBindings objs;
		};

	beginS ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	beginS Type objs = begin ?bindings objs;

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
			loc <- newLocation result;
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
		loc <- newLocation result;
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
	
	letS ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letS Type (newbinds,body) = letf ?bindings newbinds body;
	
	letStarS ::
		(
		Scheme x m r,
		?bindings		:: Bindings r m
		) =>
	 Type (r ()) -> ([(Symbol,(Object r m,()))],[Object r m]) -> m (Object r m);
	letStarS Type (newbinds,body) = letStar ?bindings newbinds body;
{--
	bindArgList :: (Scheme x m r) =>
	 Object r m -> m (Object r m) -> Procedure r m;
	bindArgList (SymbolObject f) action a = inScope (flist,makeList args) action;
	bindArgList NilObject action [] = action;
	bindArgList NilObject _ (_:_) = fail "";
	bindArgList (PairObject _ _) _ [] = fail "";
	bindArgList (PairObject (SymbolObject f1) fr) action (a1:ar) = inScope (f1,a1) (bindArgList fr ar action);
	bindArgList (PairObject _ _) _ _ = fail "";
	bindArgList _ _ _ = fail "";
--}
	lambda :: (Scheme x m r) => Object r m -> [Object r m] -> m (Procedure r m);
	lambda (SymbolObject argName) body = do
		{
		return (\bindings args -> do
			{
			argList <- getConvert args;
			argListLoc <- newLocation argList;
			begin (newBinding bindings argName argListLoc) body;
			});
		};
	
--	return (bindArgList argBindings (evaluate expr));

	lambdaS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,[Object r m]) -> m (Procedure r m);
	lambdaS Type (argBindings,body) = lambda argBindings body;

	isProcedure :: (Scheme x m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isProcedureS Type (obj,()) = isProcedure obj;
	}
