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

module Org.Org.Semantic.HScheme.MacroLib.Load where
	{
	import Org.Org.Semantic.HScheme.MacroLib.Macros;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	readFiles ::
		(
		Monad cm,
		?read :: String -> cm [obj]
		) =>
	 [String] -> cm [obj];
	readFiles [] = return [];
	readFiles (name:names) = do
		{
		objs1 <- ?read name;
		objsr <- readFiles names;
		return (objs1 ++ objsr);
		};

	readLoad ::
		(
		AssembleError cm obj,
		Build cm r,
		InterpretObject m r obj,
		?objType :: Type obj,
		?read :: String -> cm [obj],
		?macrobindings :: Symbol -> Maybe (Macro cm r obj m),
		?syntacticbindings :: SymbolBindings (Syntax r obj),
		?toplevelbindings :: Symbol -> Maybe (TopLevelMacro cm r obj m)
		) =>
	 String -> cm (TopLevelListCommand r obj m);
	readLoad filename = do
		{
		readObjects <- ?read filename;
		beginT readObjects;
		};

	secureRead :: (Monad cm) =>
	 (String -> cm a) -> (String -> Bool) -> (String -> cm a);
	secureRead loader match filename = if match filename
	 then loader filename
	 else fail ("file \""++filename++"\" not found");

	matchSecureRead :: (Monad cm) =>
	 (String -> cm a) -> [String] -> (String -> cm a);
	matchSecureRead loader allowed = secureRead loader (\f -> hasElement f allowed);

	loadT ::
		(
		?objType :: Type obj
		) =>
	 (String -> cm (TopLevelListCommand r obj m)) ->
	 (SList Char,()) -> cm (TopLevelListCommand r obj m);
	loadT load (MkSList filename,()) = load filename;
	}
