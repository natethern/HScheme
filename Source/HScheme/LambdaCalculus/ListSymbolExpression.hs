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

module Org.Org.Semantic.HScheme.LambdaCalculus.ListSymbolExpression
	(
--	ListSymbolExpression		currently broken
	) where
	{
	import Org.Org.Semantic.HScheme.LambdaCalculus.LambdaExpression;
	import Org.Org.Semantic.HBase;

	-- list lengths always the same size
	data ListSymbolExpression sym val a =
	 MkListSymbolExpression [sym] ([val] -> a);

	instance HasReturn (ListSymbolExpression sym val) where
		{
		return a = MkListSymbolExpression [] (const a);
		};

	instance Functor (ListSymbolExpression sym val) where
		{
		fmap map (MkListSymbolExpression syms func) = MkListSymbolExpression syms (map . func);
		};

	instance (Eq sym) => FunctorApply (ListSymbolExpression sym val) where
		{
		fapply (MkListSymbolExpression [] func) rda = fmap (func []) rda;
		fapply (MkListSymbolExpression (sym:syms) valsab) rda =
			MkListSymbolExpression (sym:syms') valsb where
			{
			MkListSymbolExpression syms' valsva = (exprAbstract sym rda);
			valsb ~(val:vals) = valsab vals (valsva vals val);
			};
		};

	instance (Eq sym) => LambdaExpression sym val (ListSymbolExpression sym val) where
		{
		exprSymbol sym = MkListSymbolExpression [sym] (\[val] -> val);

		exprAbstract abssym (MkListSymbolExpression syms func) = MkListSymbolExpression syms' (map func) where
			{
			(syms',map) = findSym ((==) abssym) syms;

			findSym :: (sym -> Bool) -> [sym] -> ([sym],([val] -> r) -> [val] -> val -> r);
			findSym match [] = ([],\f -> const . f);
			findSym match (s:ss) | match s = (ss,\f vals absval -> f (absval:vals));
			findSym match (s:ss) = (s:ss',\f (val:vals) -> map (\vals' -> f (val:vals')) vals) where
				{
				(ss',_) = findSym match ss;
				};
			};
		};

	instance (Eq sym) => RunnableLambdaExpression sym val (ListSymbolExpression sym val) where
		{
		runLambda resolve (MkListSymbolExpression syms func) = func (fmap resolve syms);
		};

	instance (Eq sym) => FreeSymbolLambdaExpression sym val (ListSymbolExpression sym val) where
		{
		exprFreeSymbols (MkListSymbolExpression syms _) = syms;
		};
	}
