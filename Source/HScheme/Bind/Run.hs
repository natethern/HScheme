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
	import Org.Org.Semantic.HBase.Text.Properties.Case;
	import Org.Org.Semantic.HBase.Text.Properties.Misc;
	import Org.Org.Semantic.HBase;

	loop :: a;
	loop = loop;

	foldingLP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (a -> b -> a) ->
	 a ->
	 [b] -> m a;
	foldingLP op a ns = return (foldl op a ns);

	foldingLP1 :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (a -> b -> a) ->
	 (a,[b]) -> m a;
	foldingLP1 op (a,ns) = foldingLP op a ns;

	unaryP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (a -> x) -> (a,()) -> m x;
	unaryP f (a,()) = return (f a);

	objPropertyP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m -> m Bool) -> (Object r m,()) -> m Bool;
	objPropertyP f (a,()) = f a;

	binaryP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (a -> b -> x) -> (a,(b,())) -> m x;
	binaryP f (a,(b,())) = return (f a b);

	listaryP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ([a] -> x) -> [a] -> m x;
	listaryP f l = return (f l);

	pairCheckP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (a -> a -> Bool) ->
	 [a] -> m Bool;
	pairCheckP op [] = return True;
	pairCheckP op [x] = return True;
	pairCheckP op (x:r@(y:_)) = if (op x y) then pairCheckP op r else return False;

	unaryNumberP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Number -> Number) ->
	 (Number,()) -> m Number;
	unaryNumberP = unaryP;

	switchArgs :: (a -> b -> c) -> (b -> a -> c);
	switchArgs foo b a = foo a b;

	baseBindings ::
		(
		Build cm r,
		Scheme m r
		) =>
	 LocationBindings cm r m;
	baseBindings = concatenateList
		[
		addLocationBinding		(MkSymbol "<nothing>")			nullObject,								-- nonstandard
		addLocationBinding		(MkSymbol "<loop>")				loop,									-- test
		addLocationBinding		(MkSymbol "<undefined>")		undefined,								-- test

		-- 6.1 Equivalence Predicates
		addProcBinding	"equal?"						equalP,

		-- 6.2.5 Numerical Operations
		addProcBinding	"number?"						isNumberP,
		--				"complex?"						init.pure.scm
		--				"real?"							init.pure.scm
		--				"rational?"						init.pure.scm
		addProcBinding	"integer?"						(objPropertyP (getObjectIs (MkType :: Type Integer))),
		addProcBinding	"exact?"						isExactP,
		addProcBinding	"inexact?"						isInexactP,
		addProcBinding	"="								(pairCheckP ((==) :: Number -> Number -> Bool)),
		addProcBinding	"<"								(pairCheckP ((<) :: EIReal -> EIReal -> Bool)),
		addProcBinding	">"								(pairCheckP ((>) :: EIReal -> EIReal -> Bool)),
		addProcBinding	"<="							(pairCheckP ((<=) :: EIReal -> EIReal -> Bool)),
		addProcBinding	">="							(pairCheckP ((>=) :: EIReal -> EIReal -> Bool)),
		addProcBinding	"zero?"							(unaryP ((==) zero :: Number -> Bool)),
		addProcBinding	"positive?"						(unaryP (isPositiveStrict :: EIReal -> Bool)),
		addProcBinding	"negative?"						(unaryP (isNegativeStrict :: EIReal -> Bool)),
		addProcBinding	"odd?"							(unaryP (isOdd :: Integer -> Bool)),
		addProcBinding	"even?"							(unaryP (isEven :: Integer -> Bool)),
		addProcBinding	"max"							(foldingLP1 (max :: EIReal -> EIReal -> EIReal)),
		addProcBinding	"min"							(foldingLP1 (min :: EIReal -> EIReal -> EIReal)),
		addProcBinding	"+"								(foldingLP ((+) :: Number -> Number -> Number) 0),
		addProcBinding	"*"								(foldingLP ((*) :: Number -> Number -> Number) 1),
		addProcBinding	"-"								subtractP,
		addProcBinding	"/"								divideP,
		addProcBinding	"abs"							(unaryP (abs :: Number -> EIReal)),
		addProcBinding	"quotient"						(binaryP (let {?rounding = roundTowardZero} in switchArgs failingIntegerDivideModal :: EIReal -> EIReal -> Integer)),
		addProcBinding	"remainder"						(binaryP (let {?rounding = roundTowardZero} in switchArgs failingModuloModal :: EIReal -> EIReal -> EIReal)),
		addProcBinding	"modulo"						(binaryP (switchArgs failingModulo :: EIReal -> EIReal -> EIReal)),
		addProcBinding	"gcd"							(listaryP (gcd :: [Integer] -> Integer)),
		addProcBinding	"lcm"							(listaryP (lcm :: [Integer] -> Integer)),
		addProcBinding	"numerator"						(unaryP (numerator :: Rational -> Integer)),
		addProcBinding	"denominator"					(unaryP (denominator :: Rational -> Integer)),
		addProcBinding	"floor"							(unaryP (failingFloor :: EIReal -> Integer)),
		addProcBinding	"ceiling"						(unaryP (failingCeiling :: EIReal -> Integer)),
		addProcBinding	"truncate"						(unaryP (let {?rounding = roundTowardZero} in failingRound :: EIReal -> Integer)),
		addProcBinding	"round"							(unaryP (let {?rounding = roundHalfEven} in failingRound :: EIReal -> Integer)),
		addProcBinding	"rationalize"					rationalizeP,
		addProcBinding	"exp"							(unaryNumberP exp),
		addProcBinding	"log"							(unaryNumberP log),
		addProcBinding	"sin"							(unaryNumberP sin),
		addProcBinding	"cos"							(unaryNumberP cos),
		addProcBinding	"tan"							(unaryNumberP tan),
		addProcBinding	"sinh"							(unaryNumberP sinh),
		addProcBinding	"cosh"							(unaryNumberP cosh),
		addProcBinding	"tanh"							(unaryNumberP tanh),
		addProcBinding	"asin"							(unaryNumberP asin),
		addProcBinding	"acos"							(unaryNumberP acos),
		addProcBinding	"atan"							(unaryNumberP atan),
		addProcBinding	"asinh"							(unaryNumberP asinh),
		addProcBinding	"acosh"							(unaryNumberP acosh),
		addProcBinding	"atanh"							(unaryNumberP atanh),
		addProcBinding	"sqrt"							(unaryNumberP sqrt),
		addProcBinding	"expt"							(binaryP ((**) :: Number -> Number -> Number)),
		addProcBinding	"make-rectangular"				(binaryP ((:+) :: EIReal -> EIReal -> Number)),
		addProcBinding	"make-polar"					(binaryP (polarComplex :: EIReal -> EIReal -> Number)),
		addProcBinding	"real-part"						realPartP,
		addProcBinding	"imag-part"						imagPartP,
		addProcBinding	"magnitude"						(unaryP (abs :: Number -> EIReal)),
		addProcBinding	"angle"							(unaryP (phase :: Number -> EIReal)),
		addProcBinding	"exact->inexact"				(unaryP (toInexactN :: Number -> Number)),
		addProcBinding	"inexact->exact"				(unaryP (toExactN :: Number -> Number)),

		-- 6.2.6 Numerical input and output
		addProcBinding	"number->string"				(unaryP showNumber),
		addProcBinding	"string->number"				stringToNumberP,

		-- 6.3.1 Booleans
		addProcBinding	"not"							notP,
		addProcBinding	"boolean?"						isBooleanP,

		-- 6.3.2 Pairs and Lists
		addProcBinding	"pair?"							isPairP,
		addProcBinding	"cons"							consP,
		addProcBinding	"car"							carP,
		addProcBinding	"cdr"							cdrP,
		--				"set-car!" 						Full
		--				"set-cdr!" 						Full
		--				"caar" etc.						init.pure.scm
		addProcBinding	"null?"							isNilP,
		--				"list?"							init.pure.scm
		addProcBinding	"list"							listP,
		--				"length" 						init.pure.scm
		--				"append"						init.pure.scm
		--				"reverse" 						init.pure.scm
		--				"list-tail" 					init.pure.scm
		--				"list-ref" 						init.pure.scm
		--				"memq" 							init.full.scm
		--				"memv" 							init.full.scm
		--				"member" 						init.pure.scm
		--				"assq" 							init.full.scm
		--				"assv" 							init.full.scm
		--				"assoc" 						init.pure.scm

		-- 6.3.3 Symbols
		addProcBinding	"symbol?"						isSymbolP,
		--				"symbol->string" 				init.pure.scm
		addProcBinding	"string->symbol"				makeSymbolP,

		-- 6.3.4 Characters
		addProcBinding	"char?"							(charTestP (const True)),
		--				"char=?" 						init.pure.scm
		--				"char<?" 						init.pure.scm
		--				"char>?" 						init.pure.scm
		--				"char<=?" 						init.pure.scm
		--				"char>=?" 						init.pure.scm
		--				"char-ci=?" 					init.pure.scm
		--				"char-ci<?" 					init.pure.scm
		--				"char-ci>?" 					init.pure.scm
		--				"char-ci<=?" 					init.pure.scm
		--				"char-ci>=?" 					init.pure.scm
		addProcBinding	"char-alphabetic?"				(charTestP isAlphabetic),
		--				"char-numeric?" 				init.pure.scm
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
		--				"string-set!"					Full
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

	monadContBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadCont m
		) =>
	 LocationBindings cm r m;
	monadContBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-current-continuation"	callCCP
		];

	monadGuardBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadGuard m
		) =>
	 LocationBindings cm r m;
	monadGuardBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"dynamic-wind"					dynamicWindP
		];

	monadFixBindings ::
		(
		Build cm r,
		Scheme m r,
		MonadFix m
		) =>
	 LocationBindings cm r m;
	monadFixBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcBinding	"call-with-result"				fixP									-- nonstandard
		];

	evalBindings ::
		(
		Build cm r,
		Scheme m r,
		?macrobindings :: Symbol -> Maybe (Macro im r m),
        ?toplevelbindings :: Symbol -> Maybe (TopLevelMacro im r m)
		) =>
	 (forall a. im a -> m a) ->
	 LocationBindings cm r m;
	evalBindings remonad = concatenateList
		[
		-- 6.5 Eval
		addProcBinding	"eval"							(evaluateP remonad)
		];

	portBindings ::
		(
		Build cm r,
		Scheme m r
		) =>
	 LocationBindings cm r m;
	portBindings = concatenateList
		[
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

	setBindings ::
		(
		Build cm r,
		FullScheme m r
		) =>
	 LocationBindings cm r m;
	setBindings = concatenateList
		[
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

	systemPortBindings ::
		(
		Build cm r,
		Scheme m r,
		?system :: System m
		) =>
	 LocationBindings cm r m;
	systemPortBindings = concatenateList
		[
		-- 6.6.1 Ports
		addProcBinding	"current-input-port"	currentInputPortP,
		addProcBinding	"current-output-port"	currentOutputPortP,
		addProcBinding	"current-error-port"	currentErrorPortP, -- nonstandard
		addProcBinding	"open-input-file"		openInputFileP,
		addProcBinding	"open-output-file"		openOutputFileP
		];
	}
