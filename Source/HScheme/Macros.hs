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

module Org.Org.Semantic.HScheme.Macros where
	{
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.SymbolExpression;
	import Org.Org.Semantic.HScheme.FunctorLambda;
	import Org.Org.Semantic.HBase;

	-- 4.1.2 Literal Expressions
	quoteM :: (Scheme m r,?refType :: Type (r ())) =>
	 (Object r m,()) ->
	 m (SchemeExpression r m (m (Object r m)));
	quoteM (q,()) = return (return' (return q));

	liftF3 :: (FunctorApply f) =>
	 (a -> b -> c -> r) ->
	 (f a -> f b -> f c -> f r);
	liftF3 func fa fb fc = fApply (liftF2 func fa fb) fc;

	-- 4.1.5 Conditionals
	ifM ::
		(
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)
		) =>
	 (Object r m,(Object r m,Maybe (Object r m))) ->
	 m (SchemeExpression r m (m (Object r m)));
	ifM (condObj,(thenObj,mElseObj)) = do
		{
		condExpr <- compile condObj;
		thenExpr <- compile thenObj;
		elseExpr <- case mElseObj of
			{
			Nothing -> return (return' (return nullObject));
			Just elseObj -> compile elseObj;
			};
		return (liftF3 (\mcond mthen melse -> do
			{
			condObject <- mcond;
			cond <- getConvert condObject;
			if cond
			 then mthen
			 else melse;
			}) condExpr thenExpr elseExpr);
		};

	-- 4.1.6 Assignments
	setBangM ::
		(
		FullScheme m r,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)
		) =>
	 (Symbol,(Object r m,())) -> m (SchemeExpression r m (m (Object r m)));
	setBangM (sym,(obj,())) = do
		{
		expr <- compile obj;
		return (liftF2 (\mloc mval -> do
			{
			loc <- mloc;
			val <- mval;
			set loc val;
			return nullObject;
			}) (fSymbol sym) expr);
		};
	}
