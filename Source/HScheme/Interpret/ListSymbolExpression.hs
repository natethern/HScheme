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

module Org.Org.Semantic.HScheme.Interpret.ListSymbolExpression
	(
--	ListSymbolExpression		currently broken
	) where
	{
	import Org.Org.Semantic.HScheme.Interpret.FunctorLambda;
	import Org.Org.Semantic.HBase;

	-- list lengths always the same size
	data ListSymbolExpression sym val a =
	 MkListSymbolExpression [sym] ([val] -> a);

	instance HasReturn (ListSymbolExpression sym val) where
		{
		return' a = MkListSymbolExpression [] (const a);
		};

	instance Functor (ListSymbolExpression sym val) where
		{
		fmap map (MkListSymbolExpression syms func) = MkListSymbolExpression syms (map . func);
		};

	instance (Eq sym) => FunctorApply (ListSymbolExpression sym val) where
		{
		fApply (MkListSymbolExpression [] func) rda = fmap (func []) rda;
		fApply (MkListSymbolExpression (sym:syms) valsab) rda =
			MkListSymbolExpression (sym:syms') valsb where
			{
			MkListSymbolExpression syms' valsva = (fAbstract sym rda);
			valsb (val:vals) = valsab vals (valsva vals val);
			};
		};

	instance (Eq sym) => FunctorLambda sym val (ListSymbolExpression sym val) where
		{
		fSymbol sym = MkListSymbolExpression [sym] (\[val] -> val);

		fAbstract abssym (MkListSymbolExpression syms func) = MkListSymbolExpression syms' (map func) where
			{
			(syms',map) = findSym ((==) abssym) syms;

			findSym :: (sym -> Bool) -> [sym] -> ([sym],([val] -> r) -> [val] -> val -> r);
			findSym match [] = ([],\func -> const . func);
			findSym match (sym:syms) | match sym = (syms,\func vals absval -> func (absval:vals));
			findSym match (sym:syms) = (sym:syms',\func (val:vals) -> map (\vals' -> func (val:vals')) vals) where
				{
				(syms',map) = findSym match syms;
				};
			};
		};

	instance (Eq sym) => RunnableFunctorLambda sym val (ListSymbolExpression sym val) where
		{
		runLambda resolve (MkListSymbolExpression syms func) = func (fmap resolve syms);
		};
	}
