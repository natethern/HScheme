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

module Org.Org.Semantic.HScheme.Bind.TopLevel where
	{
	import Org.Org.Semantic.HScheme.Bind.Add;
	import Org.Org.Semantic.HScheme.MacroLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	commonTopLevelBindings ::
		(
		InterpretObject m r obj,
		MapObjects r obj,
		AssembleError cm obj,
		SyntaxError cm obj,
		Build cm r,
		?objType :: Type obj
		) =>
	 TopLevelBindings cm r obj m;
	commonTopLevelBindings = concatenateList
		[
		-- 4.2.3 Sequencing
		addTopLevelMacroBinding	"begin"				beginT,

		-- 5.3 Syntax Definitions
		addTopLevelMacroBinding	"define-syntax"		defineSyntaxT
		];

	pureTopLevelBindings ::
		(
		InterpretObject m r obj,
		MapObjects r obj,
		AssembleError cm obj,
		SyntaxError cm obj,
		Build cm r,
		?binder :: TopLevelBinder r obj m,
		?objType :: Type obj
		) =>
	 TopLevelBindings cm r obj m;
	pureTopLevelBindings = concatenateList
		[
		commonTopLevelBindings,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"			(defineT pureDefine)
		];

	fullTopLevelBindings ::
		(
		FullBuild m r,
		InterpretObject m r obj,
		MapObjects r obj,
		AssembleError cm obj,
		SyntaxError cm obj,
		Build cm r,
		?binder :: TopLevelBinder r obj m,
		?objType :: Type obj
		) =>
	 TopLevelBindings cm r obj m;
	fullTopLevelBindings = concatenateList
		[
		commonTopLevelBindings,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"			(defineT fullDefine)
		];

	loadTopLevelBindings ::
		(
		Build cm r,
		AssembleError cm obj,
		InterpretObject m r obj,
		ObjectSubtype r obj (SList Char),
		?objType :: Type obj
		) =>
	 (String -> cm (TopLevelListCommand r obj m)) ->
	 TopLevelBindings cm r obj m;
	loadTopLevelBindings load = concatenateList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT load)
		];
	}
