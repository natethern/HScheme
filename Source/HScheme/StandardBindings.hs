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

module Org.Org.Semantic.HScheme.StandardBindings where
	{
	import Org.Org.Semantic.HScheme.SExpParser;
	import Org.Org.Semantic.HScheme.Syntax;
	import Org.Org.Semantic.HScheme.PortProcedures;
	import Org.Org.Semantic.HScheme.Lambda;
	import Org.Org.Semantic.HScheme.Equality;
	import Org.Org.Semantic.HScheme.NumericProcedures;
	import Org.Org.Semantic.HScheme.Macros;
	import Org.Org.Semantic.HScheme.Procedures;
--	import Org.Org.Semantic.HScheme.TopLevel;
--	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	loop :: a;
	loop = loop;

	macroBindings ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro r m),
		?macrobindings :: Binds Symbol (Macro r m),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?refType :: Type (r ())
		) =>
	 Binds Symbol (Macro r m) ->
	 Binds Symbol (Macro r m);
	macroBindings = concatenateList
		[
		-- 4.1.2 Literal Expressions
		addMacroBinding	"quote"					quoteM,

		-- 4.1.4 Procedures
		addMacroBinding	"lambda"				lambdaM,

		-- 4.1.5 Conditionals
		addMacroBinding	"if"					ifM,

		-- 4.2.2 Binding Constructs
		addMacroBinding	"let"					letSeparateM,
		addMacroBinding	"let*"					letSequentialM,
		addMacroBinding	"letrec"				letRecursiveM
		];

	pureMacroBindings ::
		(
		Scheme m r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro r m),
		?macrobindings :: Binds Symbol (Macro r m),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?refType :: Type (r ())
		) =>
	 Binds Symbol (Macro r m) ->
	 Binds Symbol (Macro r m);
	pureMacroBindings = concatenateList
		[
		macroBindings,

		-- 4.2.3 Sequencing
		addMacroBinding	"begin"					beginM,
		addMacroBinding	"begin-list"			beginListM
		];

	topLevelBindings ::
		(
		Scheme m r,
		?macrobindings :: Binds Symbol (Macro r m),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?refType :: Type (r ())
		) =>
	 Binds Symbol (TopLevelMacro r m) ->
	 Binds Symbol (TopLevelMacro r m);
	topLevelBindings = concatenateList
		[
		-- 4.3.2 Pattern Language
--		addSyntaxMakerMacroBinding	"syntax-rules"			syntaxRulesM,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"			defineT,
--		addTopLevelMacroBinding	"define"			(defineT pureSetLoc),

		-- 5.3 Syntax Definitions
		addTopLevelMacroBinding	"define-syntax"		defineSyntaxT
		];

	commonStrictPureBindings :: (Scheme m r,?refType :: Type (r ())) => Bindings r m -> m (Bindings r m);
	commonStrictPureBindings = concatenateList
		[
		addLocationBinding		(MkSymbol "<nothing>")			nullObject,								-- nonstandard
		addLocationBinding		(MkSymbol "<loop>")				loop,									-- test
		addLocationBinding		(MkSymbol "<undefined>")		undefined,								-- test

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
		addProcBinding	"char-whitespace?"				(charTestP isWhiteSpace),
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
		addProcBinding	"char->integer"					(charFuncP ordFromStart),
		addProcBinding	"integer->char"					integerToCharP,

		-- 6.3.5 Strings
		addProcBinding	"string?"						isStringP,
		addProcBinding	"make-string"					makeStringP,
		addProcBinding	"string"						stringP,
		addProcBinding	"string-length"					stringLengthP,
		addProcBinding	"string-ref"					stringRefP,
		addProcBinding	"string-append"					stringAppendP,
		addProcBinding	"string-chars"					stringCharsP,

		-- Byte Arrays (all nonstandard)
		addProcBinding	"byte-array?"					isByteArrayP,
		addProcBinding	"make-byte-array"				makeByteArrayP,
		addProcBinding	"byte-array"					byteArrayP,
		addProcBinding	"byte-array-length"				byteArrayLengthP,
		addProcBinding	"byte-array-ref"				byteArrayRefP,
		addProcBinding	"byte-array-append"				byteArrayAppendP,
		addProcBinding	"byte-array-bytes"				byteArrayBytesP,

		-- Encoding (all nonstandard)
		addProcBinding	"encode-utf8"					(encodeP encodeUTF8),
		addProcBinding	"decode-utf8"					(decodeP decodeUTF8),
		addProcBinding	"parse-utf8"					parseUTF8P,
		addProcBinding	"encode-latin1"					(encodeP encodeLatin1),
		addProcBinding	"decode-latin1"					(decodeP (return . decodeLatin1)),

		-- 6.4 Control Features
		addProcBinding	"procedure?"					isProcedureP,
		addProcBinding	"apply"							applyP,
		addProcBinding	"values"						valuesP,
		addProcBinding	"values->list"					valuesToListP,							-- nonstandard
		addProcBinding	"throw"							lastResortThrowP,						-- nonstandard

		-- 6.5 Eval
--		addProcBinding	"eval"							evaluateP,
--		addProcBinding	"current-environment"			currentEnvironmentP,					-- nonstandard

		-- Misc
		addProcBinding	"to-string"						toStringP								-- nonstandard
--		,addMacroBinding	"case-match"					caseMatchM								-- nonstandard
		];

	simpleStrictPureBindings :: (Scheme m r,?refType :: Type (r ())) => Bindings r m -> m (Bindings r m);
	simpleStrictPureBindings = commonStrictPureBindings;

	commonPureBindings :: (Scheme m r,?refType :: Type (r ())) => Bindings r m -> m (Bindings r m);
	commonPureBindings = concatenateList
		[
		commonStrictPureBindings,

		-- 6.6.1 Ports
		addProcBinding	"input-port?"					isInputPortP,
		addProcBinding	"output-port?"					isOutputPortP,
		addProcBinding	"close-input-port"				inputPortCloseP,
		addProcBinding	"close-output-port"				outputPortCloseP,

		-- 6.6.2 Input
		addProcBinding	"port-read"						portReadP,								-- nonstandard
		addProcBinding	"port-read-byte"				portReadByteP,							-- nonstandard
		addProcBinding	"port-peek-byte"				portPeekByteP,							-- nonstandard
		addProcBinding	"eof-object?"					isEOFObjectP,
		addProcBinding	"port-byte-ready?"				portByteReadyP,							-- nonstandard

		-- 6.6.3 Output
		addProcBinding	"port-write-byte"				portWriteByteP							-- nonstandard
		];

	simplePureBindings :: (Scheme m r,?refType :: Type (r ())) => Bindings r m -> m (Bindings r m);
	simplePureBindings = commonPureBindings;

	monadContBindings :: (Scheme m r,?refType :: Type (r ()),MonadCont m) => Bindings r m -> m (Bindings r m);
	monadContBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-current-continuation"	callCCP
		];

	monadFixBindings :: (Scheme m r,?refType :: Type (r ()),MonadFix m) => Bindings r m -> m (Bindings r m);
	monadFixBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-result"				fixP									-- nonstandard
		];

	monadFixStrictPureBindings :: (Scheme m r,?refType :: Type (r ()),MonadFix m) =>
	 Bindings r m -> m (Bindings r m);
	monadFixStrictPureBindings = simpleStrictPureBindings ++ monadFixBindings;

	monadContStrictPureBindings :: (Scheme m r,?refType :: Type (r ()),MonadCont m) =>
	 Bindings r m -> m (Bindings r m);
	monadContStrictPureBindings = simpleStrictPureBindings ++ monadContBindings;

	monadFixPureBindings :: (Scheme m r,?refType :: Type (r ()),MonadFix m) =>
	 Bindings r m -> m (Bindings r m);
	monadFixPureBindings = simplePureBindings ++ monadFixBindings;

	monadContPureBindings :: (Scheme m r,?refType :: Type (r ()),MonadCont m) =>
	 Bindings r m -> m (Bindings r m);
	monadContPureBindings = simplePureBindings ++ monadContBindings;
	}
