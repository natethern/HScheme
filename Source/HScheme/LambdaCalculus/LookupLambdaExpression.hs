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

module Org.Org.Semantic.HScheme.LambdaCalculus.LookupLambdaExpression(LookupLambdaExpression) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	-- list lengths always the same size
	newtype LookupLambdaExpression sym val a = MkExpression ((sym -> val) -> a);

	instance HasReturn (LookupLambdaExpression sym val) where
		{
		return a = MkExpression (const a);
		};

	instance Functor (LookupLambdaExpression sym val) where
		{
		fmap map (MkExpression func) =
		 {-# SCC "fmap" #-}
		 MkExpression (map . func);
		};

	instance FunctorApply (LookupLambdaExpression sym val) where
		{
		fapply (MkExpression svab) (MkExpression sva) = MkExpression
			(\sv -> svab sv (sva sv));
		};

	instance (Eq sym) => LambdaExpression sym val (LookupLambdaExpression sym val) where
		{
		exprSymbol sym = MkExpression (\sv -> sv sym);

		exprAbstract abssym (MkExpression sva) = MkExpression (\sv v -> sva (\s -> if s == abssym then v else sv s));

--		exprLet abssym (MkExpression sva) (MkExpression body)= MkExpression
--			(\sv -> body (\s -> if s == abssym then (sva sv) else sv s));

		exprLetSym abssym sym (MkExpression body)= MkExpression
			(\sv -> body (\s -> sv (if s == abssym then sym else s)));
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (LookupLambdaExpression sym val) where
		{
		runLambda resolve (MkExpression sva) = sva resolve;
		};

	unExpression :: LookupLambdaExpression sym val a -> (sym -> val) -> a;
	unExpression (MkExpression sva) = sva;

	instance Monad (LookupLambdaExpression sym val) where
		{
		mbind (MkExpression sva) afb = MkExpression (\sv -> unExpression (afb (sva sv)) sv);
		};
	}
