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

module Org.Org.Semantic.HScheme.TopLevel
	(
	pureSetLoc,fullSetLoc,
	defineT,
	topLevelEvaluate
	) where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	pureSetLoc :: (Scheme m r) =>
	 ObjLocation r m -> Object r m -> m ();
	pureSetLoc _ _ = fail "can't redefine symbol";

	fullSetLoc :: (FullScheme m r) =>
	 ObjLocation r m -> Object r m -> m ();
	fullSetLoc = set;

	-- 5.2 Definitions
	defineT :: (Scheme m r) =>
	 (ObjLocation r m -> Object r m -> m ()) ->
	 Bindings r m -> (Object r m,Object r m) -> m (Bindings r m,ArgNoneType);
	defineT setLoc bindings (h,t) = case h of
		{
		SymbolObject name -> case t of
			{
			PairObject thead ttail -> do
				{
				tt <- get ttail;
				case tt of
					{
					NilObject ->  do
						{
						th <- get thead;
						result <- let {?bindings=bindings} in evaluate th;
						case (getBinding bindings name) of
							{
							Nothing -> do
								{
								loc <- new result;
							 	return (newBinding bindings name loc,MkArgNoneType);
							 	};
							Just loc -> do
								{
								setLoc loc result;
							 	return (bindings,MkArgNoneType);
								};
							};
						};
					_ -> fail "bad define form (too many arguments)";
					};
				};
			NilObject -> fail "bad define form (only one argument)";
			_ -> fail "bad define form (dotted pair)";
			};
		PairObject _ _ -> fail "this define form NYI";
		_ -> fail "bad define form (wrong type for label)";
		};
	
	topLevelApplyEval ::
		(
		Scheme m r
		) =>
	 Bindings r m -> Object r m -> Object r m -> m (Bindings r m,Object r m);
	topLevelApplyEval bindings (TopLevelMacroObject f) arglist = do
		{
		f bindings arglist;
		};
	topLevelApplyEval bindings obj arglist = do
		{
		result <- let {?bindings=bindings;} in applyEval obj arglist;
		return (bindings,result);
		};

	topLevelEvaluate ::
		(
		Scheme m r
		) =>
	 TopLevelMacro r m;
	
	topLevelEvaluate bindings a = do
		{
		case a of
			{
			(PairObject head tail) -> do
				{
				h <- get head;
				t <- get tail;
				(bindings',f) <- topLevelEvaluate bindings h;
				topLevelApplyEval bindings' f t;
				};
			_ -> do
				{
				r <- let {?bindings = bindings} in evaluate a;
				return (bindings,r);
				};
			};
		};
{--
	topLevelEvaluateList ::
		(
		Scheme m r
		) =>
	 Bindings r m -> [Object r m] -> m (Bindings r m);
	
	topLevelEvaluateList bindings [] = return bindings;
	topLevelEvaluateList bindings (obj:objs) = do
		{
		(bindings',_) <- topLevelEvaluate bindings obj;
		topLevelEvaluateList bindings' objs;
		};
--}
	}
