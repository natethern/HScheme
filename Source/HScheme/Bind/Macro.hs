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

module Org.Org.Semantic.HScheme.Bind.Macro where
	{
	import Org.Org.Semantic.HScheme.Bind.Add;
	import Org.Org.Semantic.HScheme.MacroLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	macroBindings ::
		(
		IsA Bool obj,
		MapObjects r obj,
		Build cm r,
		SyntaxError cm obj,
		AssembleError cm obj,
		MonadFix m,
		PatternError m obj,
		InterpretObject m r obj,
		?binder :: TopLevelBinder r obj m,
		?objType :: Type obj
		) =>
	 MacroBindings cm r obj m;
	macroBindings = concatenateList
		[
		-- 4.1.2 Literal Expressions
		addMacroBinding	"quote"				quoteM,

		-- 4.1.4 Procedures
		addMacroBinding	"lambda"			lambdaM,

		-- 4.1.5 Conditionals
		addMacroBinding	"if"				ifM,

		-- 4.2.2 Binding Constructs
		addMacroBinding	"let"				(addM letSeparateM namedLetM),
		addMacroBinding	"let*"				letSequentialM,
		addMacroBinding	"letrec"			letRecursiveBinderM,
		addMacroBinding	"letfix"			letRecursiveFixM,

		-- 4.2.4 Iteration
		addMacroBinding	"do"				doM,


		-- 4.3.1 Binding constructs for syntactic keywords
		addMacroBinding	"let-syntax"		letSyntaxSeparateM,
		addMacroBinding	"letrec-syntax"		letSyntaxRecursiveM,

		-- Misc
		addMacroBinding	"case-match"		caseMatchM		-- nonstandard
		];

	pureMacroBindings ::
		(
		IsA Bool obj,
		MapObjects r obj,
		Build cm r,
		SyntaxError cm obj,
		AssembleError cm obj,
		MonadFix m,
		PatternError m obj,
		InterpretObject m r obj,
		?binder :: TopLevelBinder r obj m,
		?objType :: Type obj
		) =>
	 MacroBindings cm r obj m;
	pureMacroBindings = concatenateList
		[
		macroBindings,

		-- 4.2.3 Sequencing
		addMacroBinding	"begin"				bodyM,

		-- Misc
		addMacroBinding	"body"				bodyM,				-- nonstandard
		addMacroBinding	"body-list"			bodyListM			-- nonstandard
		];

	fullMacroBindings ::
		(
		IsA Bool obj,
		MapObjects r obj,
		Build cm r,
		SyntaxError cm obj,
		AssembleError cm obj,
		MonadFix m,
		PatternError m obj,
		InterpretObject m r obj,
		FullBuild m r,
		?binder :: TopLevelBinder r obj m,
		?objType :: Type obj
		) =>
	 MacroBindings cm r obj m;
	fullMacroBindings = concatenateList
		[
		pureMacroBindings,

		-- 4.1.6 Assignments
		addMacroBinding			"set!"				setBangM
		];
	}
