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

module StandardBindings where
	{
	import Syntax;
	import PortProcedures;
	import Lambda;
	import Equality;
	import NumericProcedures;
	import Procedures;
	import TopLevel;
	import Evaluate;
	import Bindings;
	import Object;
	import Numerics;

	stdBindings :: (Scheme x m r) => Bindings r m -> m (Bindings r m);
	stdBindings = chainList
		[
		addBinding		"<nothing>"				nullObject,
		
		-- 4.1.2 Literal Expressions
		addMacroBinding	"quote"					quoteM,

		-- 4.1.4 Procedures
		addMacroBinding	"lambda"				lambdaM,

		-- 4.1.5 Conditionals
		addMacroBinding	"if"					ifM,

		-- 4.2.2 Binding Constructs
		addMacroBinding	"let"					letM,
		addMacroBinding	"let*"					letStarM,

		-- 4.3.2 Pattern Language
		addMacroBinding	"syntax-rules"			syntaxRulesM,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"		defineT,
		
		-- 6.1 Equivalence Predicates
		addProcBinding	"equal?"				equalP,
		
		-- 6.2.5 Numerical Operations
		addProcBinding	"+"						(foldingLP (+) 0),
		addProcBinding	"-"						subtractP,
		addProcBinding	"*"						(foldingLP (*) 1),
		addProcBinding	"/"						divideP,

		-- 6.3.1 Booleans
		addProcBinding	"not"					notP,
		addProcBinding	"boolean?"				isBooleanP,

		-- 6.3.2 Pairs and Lists
		addProcBinding	"cons"					consP,
		addProcBinding	"car"					carP,
		addProcBinding	"cdr"					cdrP,
		addProcBinding	"list"					listP,
		addProcBinding	"append"				appendP,

		-- 6.4 Control Features
		addProcBinding	"procedure?"			isProcedureP,
		addProcBinding	"apply"					applyP,
		addProcBinding	"values"				valuesP,
		addProcBinding	"values->list"			valuesToListP,

		-- 6.5 Eval
		addProcBinding	"eval"					evaluateP,
		addProcBinding	"current-environment"	currentEnvironmentP, -- nonstandard

		-- Misc
		addProcBinding	"to-string"				toStringP
		];

	monadicStdBindings :: (Scheme x m r) => Bindings r m -> m (Bindings r m);
	monadicStdBindings = chainList
		[
		stdBindings,

		-- 4.2.3 Sequencing
		addMacroBinding	"begin"								beginM,

		-- 6.4 Control Features
		addProcBinding	"call-with-current-continuation"	callCCP,

		-- 6.6.1 Ports
		addProcBinding	"input-port?"						isInputPortP,
		addProcBinding	"output-port?"						isOutputPortP,
		addProcBinding	"close-input-port"					inputPortCloseP,
		addProcBinding	"close-output-port"					outputPortCloseP,

		-- 6.6.2 Input
		addProcBinding	"eof-object?"						isEOFObjectP,

		-- 6.6.3 Output
		addProcBinding	"port-write-char"					portWriteCharP -- nonstandard
		];
	}
