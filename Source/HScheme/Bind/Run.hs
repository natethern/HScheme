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

module Org.Org.Semantic.HScheme.Bind.Run where
	{
	import Org.Org.Semantic.HScheme.Bind.Add;
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	loop :: a;
	loop = loop;

	commonStrictPureBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
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

		-- 6.3.6 Vectors
		addProcBinding	"vector?"						isVectorP,
		addProcBinding	"make-vector"					makeVectorP,
		addProcBinding	"vector"						vectorP,
		addProcBinding	"vector-length"					vectorLengthP,
		addProcBinding	"vector-ref"					vectorRefP,
		addProcBinding	"vector->list"					vectorToListP,
		addProcBinding	"list->vector"					listToVectorP,

		-- 6.4 Control Features
		addProcBinding	"procedure?"					isProcedureP,
		addProcBinding	"apply"							applyP,
		addProcBinding	"values"						valuesP,
		addProcBinding	"values->list"					valuesToListP,							-- nonstandard
		addProcBinding	"throw"							lastResortThrowP,						-- nonstandard

		-- 6.5 Eval
--		addProcBinding	"current-environment"			currentEnvironmentP,					-- nonstandard

		-- Misc
		addProcBinding	"to-string"						toStringP								-- nonstandard
		];

	evalBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Bindings Symbol (Macro m r m),
        ?toplevelbindings :: Bindings Symbol (TopLevelMacro m r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	evalBindings = concatenateList
		[
		-- 6.5 Eval
		addProcBinding	"eval"							evaluateP
		];

	simpleStrictPureBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	simpleStrictPureBindings = commonStrictPureBindings;

	commonPureBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
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

	simplePureBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	simplePureBindings = commonPureBindings;

	monadContBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadCont m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadContBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-current-continuation"	callCCP
		];

	monadFixBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadFix m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadFixBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-result"				fixP									-- nonstandard
		];

	monadFixStrictPureBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadFix m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadFixStrictPureBindings = simpleStrictPureBindings ++ monadFixBindings;

	monadContStrictPureBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadCont m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadContStrictPureBindings = simpleStrictPureBindings ++ monadContBindings;

	monadFixPureBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadFix m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadFixPureBindings = simplePureBindings ++ monadFixBindings;

	monadContPureBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadCont m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadContPureBindings = simplePureBindings ++ monadContBindings;

	simpleFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	simpleFullBindings = concatenateList
		[
		commonPureBindings,

		-- 6.1 Equivalence Predicates
		addProcBinding			"eqv?"				eqvP,
		addProcBinding			"eq?"				eqP,

		-- 6.3.2 Pairs and Lists
		addProcBinding			"set-car!"			setCarP,
		addProcBinding			"set-cdr!"			setCdrP,

		-- 6.3.5 Strings
		addProcBinding			"string-set!"		stringSetP,

		-- Byte Arrays
		addProcBinding			"byte-array-set!"	byteArraySetP,

		-- 6.3.6 Vectors
		addProcBinding			"vector-set!"		vectorSetP,
		addProcBinding			"vector-fill!"		vectorFillP
		];

	monadFixFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		MonadFix m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadFixFullBindings = simpleFullBindings ++ monadFixBindings;

	-- this one is closest to R5RS
	monadContFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		MonadCont m,
		?objType :: Type (Object r m)
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	monadContFullBindings = simpleFullBindings ++ monadContBindings;

	fullSystemBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m),
		?system :: System m
		) =>
	 SymbolBindings (ObjLocation r m) -> cm (SymbolBindings (ObjLocation r m));
	fullSystemBindings = concatenateList
		[
		-- 6.6.1 Ports
		addProcBinding	"current-input-port"	currentInputPortP,
		addProcBinding	"current-output-port"	currentOutputPortP,
		addProcBinding	"current-error-port"	currentErrorPortP, -- nonstandard
		addProcBinding	"open-input-file"		openInputFileP,
		addProcBinding	"open-output-file"		openOutputFileP
		];
	}
