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

module TopLevel
	(
	defineT,
	topLevelEvaluate
	) where
	{
	import Evaluate;
	import Conversions;
	import Object;
	import HBase;
	
	-- 5.2 Definitions
	defineT ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> (Object r m,Object r m) -> m (Bindings r m,ArgNoneType);
	defineT bindings (h,t) = do
		{
		case h of
			{
			SymbolObject name -> do
				{
				case t of
					{
					PairObject thead ttail -> do
						{
						tt <- getLocation ttail;
						case tt of
							{
							NilObject -> do
								{
								th <- getLocation thead;
								result <- evaluate th with {?bindings=bindings};
								loc <- newLocation result;
								return (newBinding bindings name loc,MkArgNoneType);
								};
							_ -> fail "bad define form (too many arguments)";
							};
						};
					NilObject -> fail "bad define form (only one argument)";
					_ -> fail "bad define form (dotted pair)";
					};
				};
			PairObject _ _ -> fail "this define form NYI";
			_ -> fail "bad define form (wrong type for label)";
			};
		};
	
	topLevelApplyEval ::
		(
		Scheme x m r
		) =>
	 Bindings r m -> Object r m -> Object r m -> m (Bindings r m,Object r m);
	topLevelApplyEval bindings (TopLevelMacroObject f) arglist = do
		{
		f bindings arglist;
		};
	topLevelApplyEval bindings obj arglist = do
		{
		result <- applyEval obj arglist with {?bindings=bindings;};
		return (bindings,result);
		};

	topLevelEvaluate ::
		(
		Scheme x m r
		) =>
	 TopLevelMacro r m;
	
	topLevelEvaluate bindings a = do
		{
		case a of
			{
			(PairObject head tail) -> do
				{
				h <- getLocation head;
				t <- getLocation tail;
				(bindings',f) <- topLevelEvaluate bindings h;
				topLevelApplyEval bindings' f t;
				};
			_ -> do
				{
				r <- evaluate a with {?bindings = bindings};
				return (bindings,r);
				};
			};
		};
{--
	topLevelEvaluateList ::
		(
		Scheme x m r
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
