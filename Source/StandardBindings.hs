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
	import HBase;

	loop :: a;
	loop = loop;

	stdBindings :: (Scheme x m r) => Bindings r m -> m (Bindings r m);
	stdBindings = chainList
		[
		addBinding		(MkSymbol "<nothing>")			nullObject,								-- nonstandard
		addBinding		(MkSymbol "<loop>")				loop,									-- test
		addBinding		(MkSymbol "<undefined>")		undefined,									-- test
		
		-- 4.1.2 Literal Expressions
		addMacroBinding	"quote"							quoteM,

		-- 4.1.4 Procedures
		addMacroBinding	"lambda"						lambdaM,

		-- 4.1.5 Conditionals
		addMacroBinding	"if"							ifM,

		-- 4.2.2 Binding Constructs
		addMacroBinding	"let"							letM,
		addMacroBinding	"let*"							letStarM,

		-- 4.3.2 Pattern Language
		addMacroBinding	"syntax-rules"					syntaxRulesM,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"				defineT,
		
		-- 6.1 Equivalence Predicates
		addProcBinding	"equal?"						equalP,
		
		-- 6.2.5 Numerical Operations
		addProcBinding	"number?"						isNumberP,
		addProcBinding	"exact?"						isExactP,
		addProcBinding	"inexact?"						isInexactP,
		addProcBinding	"zero?"							isZeroP,
		addProcBinding	"+"								(foldingLP (+) 0),
		addProcBinding	"-"								subtractP,
		addProcBinding	"*"								(foldingLP (*) 1),
--		addProcBinding	"/"								divideP,
		addProcBinding	"real-part"						realPartP,
		addProcBinding	"imag-part"						imagPartP,

		-- 6.3.1 Booleans
		addProcBinding	"not"							notP,
		addProcBinding	"boolean?"						isBooleanP,

		-- 6.3.2 Pairs and Lists
		addProcBinding	"pair?"							isPairP,
		addProcBinding	"cons"							consP,
		addProcBinding	"car"							carP,
		addProcBinding	"cdr"							cdrP,
		addProcBinding	"null?"							isNilP,
		addProcBinding	"list"							listP,
		addProcBinding	"append"						appendP,

		-- 6.3.3 Symbols
		addProcBinding	"symbol?"						isSymbolP,
		addProcBinding	"string->symbol"				makeSymbolP,

		-- 6.3.4 Characters
		addProcBinding	"char?"							(charTestP (const True)),
		addProcBinding	"char-alphabetic?"				(charTestP isAlphabetic),
		addProcBinding	"char-whitespace?"				(charTestP isWhitespace),
		addProcBinding	"char-upper-case?"				(charTestP isUppercase),
		addProcBinding	"char-lower-case?"				(charTestP isLowercase),
		addProcBinding	"char-title-case?"				(charTestP isTitlecase),				-- nonstandard
		addProcBinding	"char-bidirectional-control?"	(charTestP isBidiControl),				-- nonstandard
		addProcBinding	"char-join-control?"			(charTestP isJoinControl),				-- nonstandard
		addProcBinding	"char-dash?"					(charTestP isDash),						-- nonstandard
		addProcBinding	"char-hyphen?"					(charTestP isHyphen),					-- nonstandard
		addProcBinding	"char-quotation-mark?"			(charTestP isQuotationMark),			-- nonstandard
		addProcBinding	"char-terminal-punctuation?"	(charTestP isTerminalPunctuation),		-- nonstandard
		addProcBinding	"char-math?"					(charTestP isMath),						-- nonstandard
		addProcBinding	"char-hex-digit?"				(charTestP isHexDigit),					-- nonstandard
		addProcBinding	"char-ideographic?"				(charTestP isIdeographic),				-- nonstandard
		addProcBinding	"char-diacritic?"				(charTestP isDiacritic),				-- nonstandard
		addProcBinding	"char-extender?"				(charTestP isExtender),					-- nonstandard
		addProcBinding	"char-noncharacter?"			(charTestP isNoncharacterCodePoint),	-- nonstandard
		addProcBinding	"char-upcase"					(charFuncP toUpperCase),
		addProcBinding	"char-downcase"					(charFuncP toLowerCase),
		addProcBinding	"char-titlecase"				(charFuncP toTitleCase),				-- nonstandard
		addProcBinding	"char-number"					(charFuncP getNumber),					-- nonstandard
		addProcBinding	"char-decimal-digit"			(charFuncP getDecimalDigit),			-- nonstandard
		addProcBinding	"char->integer"					(charFuncP valueNthFromStart),
		addProcBinding	"integer->char"					(\_ (i,()) -> return (unJust (nthValueFromStart i) :: Char)),

		-- 6.4 Control Features
		addProcBinding	"procedure?"					isProcedureP,
		addProcBinding	"apply"							applyP,
		addProcBinding	"values"						valuesP,
		addProcBinding	"values->list"					valuesToListP,

		-- 6.5 Eval
		addProcBinding	"eval"							evaluateP,
		addProcBinding	"current-environment"			currentEnvironmentP,					-- nonstandard

		-- Misc
		addProcBinding	"to-string"						toStringP,								-- nonstandard
		addMacroBinding	"case-match"					caseMatchM								-- nonstandard
		] where
		{
		};

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
