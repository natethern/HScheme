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

	foldingLP :: (Monad m) =>
	 (a -> b -> a) ->
	 a ->
	 [b] -> m a;
	foldingLP op a ns = return (foldl op a ns);

	foldingLP1 :: (Monad m) =>
	 (a -> b -> a) ->
	 (a,[b]) -> m a;
	foldingLP1 op (a,ns) = foldingLP op a ns;

	unaryP :: (Monad m) =>
	 (a -> x) -> (a,()) -> m x;
	unaryP f (a,()) = return (f a);

	unaryMRoundP :: (Monad m) =>
	 (EIReal -> m Integer) -> (EIReal,()) -> m EIReal;
	unaryMRoundP f (a@(ExactReal _),()) = fmap (ExactReal . convert) (f a);
	unaryMRoundP f (a@(InexactReal _),()) = fmap (InexactReal . fromInteger) (f a);

	unaryRoundP :: (Monad m) =>
	 (EIReal -> Integer) -> (EIReal,()) -> m EIReal;
	unaryRoundP f = unaryMRoundP (return . f);

	unaryRationalRoundP ::
		(
		Build m r,
		ObjectSubtype r obj EIReal,
		ProcedureError m obj,
		?objType :: Type obj
		) =>
	 (Rational -> Integer) -> (EIReal,()) -> m EIReal;
	unaryRationalRoundP f = unaryMRoundP (\eir -> case maybeApproximate eir of
		{
		Just r -> return (f r);
		Nothing -> do
			{
			obj <- getObject eir;
			throwArgumentListMismatchError (WrongArgumentType 1 (MkMismatch RationalTypeExpected obj));
			};
		});

	checkInexact :: (EIReal -> EIReal -> EIReal) -> EIReal -> EIReal -> EIReal;
	checkInexact f a@(ExactReal _) b@(ExactReal _) = f a b;
	checkInexact f a b = InexactReal (toInexact (f a b));

	objPropertyP :: (Monad m,?objType :: Type obj) =>
	 (obj -> m Bool) -> (obj,()) -> m Bool;
	objPropertyP f (a,()) = f a;

	binaryP :: (Monad m) =>
	 (a -> b -> x) -> (a,(b,())) -> m x;
	binaryP f (a,(b,())) = return (f a b);

	listaryP :: (Monad m) =>
	 ([a] -> x) -> [a] -> m x;
	listaryP f l = return (f l);

	pairCheckP :: (Monad m) =>
	 (a -> a -> Bool) ->
	 [a] -> m Bool;
	pairCheckP op [] = return True;
	pairCheckP op [x] = return True;
	pairCheckP op (x:r@(y:_)) = if (op x y) then pairCheckP op r else return False;

	unaryNumberP :: (Monad m) =>
	 (Number -> Number) ->
	 (Number,()) -> m Number;
	unaryNumberP = unaryP;

	switchArgs :: (a -> b -> c) -> (b -> a -> c);
	switchArgs foo b a = foo a b;


	baseBindings ::
		(
		HasBooleanType obj,
		ObjectSubtype r obj Bool,
		ObjectSubtype r obj Number,
		ObjectSubtype r obj EIReal,
		ObjectSubtype r obj Rational,
		ObjectSubtype r obj Int,
		ObjectSubtype r obj Integer,
		ObjectSubtype r obj Word8,
		ObjectSubtype r obj (SList Word8),
		ObjectSubtype r obj (SRefList r Word8),
		ObjectSubtype r obj (SRefArray r Word8),
		ObjectSubtype r obj Char,
		ObjectSubtype r obj (SList Char),
		ObjectSubtype r obj (SRefList r Char),
		ObjectSubtype r obj (SRefArray r Char),
		ObjectSubtype r obj (SList obj),
		ObjectSubtype r obj (SRefArray r obj),
		ObjectSubtype r obj EOFObjType,
		ParseObject r obj,
		ParserError m obj,
		Equal m obj,
		ToString m obj,
		InterpretObject m r obj,
		Build cm r
		) =>
	 RefBindings cm r obj;
	baseBindings = concatenateList
		[
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
		addProcBinding	"max"							(foldingLP1 (checkInexact (max :: EIReal -> EIReal -> EIReal))),
		addProcBinding	"min"							(foldingLP1 (checkInexact (min :: EIReal -> EIReal -> EIReal))),
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
		addProcBinding	"numerator"						(unaryRationalRoundP (numerator :: Rational -> Integer)),
		addProcBinding	"denominator"					(unaryRationalRoundP (denominator :: Rational -> Integer)),
		addProcBinding	"floor"							(unaryRoundP (failingFloor :: EIReal -> Integer)),
		addProcBinding	"ceiling"						(unaryRoundP (failingCeiling :: EIReal -> Integer)),
		addProcBinding	"truncate"						(unaryRoundP (let {?rounding = roundTowardZero} in failingRound :: EIReal -> Integer)),
		addProcBinding	"round"							(unaryRoundP (let {?rounding = roundHalfEven} in failingRound :: EIReal -> Integer)),
		addProcBinding	"rationalize"					rationalizeP,
		addProcBinding	"rationalize-exact"				rationalizeExactP,
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
		addProcLBinding	"apply"							applyPL,
		addProcLBinding	"values"						valuesPL,
		addProcLBinding	"call-with-values"				callWithValuesPL,
--		addProcBinding	"raise"							lastResortThrowP,						-- nonstandard

		-- 6.5 Eval
--		addProcBinding	"current-environment"			currentEnvironmentP,					-- nonstandard

		-- Misc
		addProcBinding	"to-string"						toStringP,								-- nonstandard
		addProcBinding	"to-display"					toDisplayP								-- nonstandard
		];

	monadContBindings ::
		(
		InterpretObject m r obj,
		MonadCont m,
		Build cm r
		) =>
	 RefBindings cm r obj;
	monadContBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcLBinding	"call-with-current-continuation"	callCCPL
		];

	monadGuardBindings ::
		(
		InterpretObject m r obj,
		MonadGuard m,
		Build cm r
		) =>
	 RefBindings cm r obj;
	monadGuardBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcLBinding	"dynamic-wind"					dynamicWindPL
		];

	monadFixBindings ::
		(
		InterpretObject m r obj,
		MonadFix m,
		Build cm r
		) =>
	 RefBindings cm r obj;
	monadFixBindings = concatenateList
		[
		-- 6.4 Control Features
		addProcLBinding	"call-with-result"				fixPL									-- nonstandard
		];

	evalBindings ::
		(
		ObjectSubtype r obj (Environment r obj m),
		MonadThrow obj m,
		AssembleError m obj,
		InterpretObject m r obj,
		Build cm r,
		?macrobindings :: Symbol -> Maybe (Macro im r obj m),
        ?toplevelbindings :: Symbol -> Maybe (TopLevelMacro im r obj m)
		) =>
	 (forall a. im a -> m a) ->
	 RefBindings cm r obj;
	evalBindings remonad = concatenateList
		[
		-- 6.5 Eval
		addProcLBinding	"eval"							(evaluatePL remonad)
		];

	portBindings ::
		(
		ObjectSubtype r obj Bool,
		ObjectSubtype r obj Word8,
		ObjectSubtype r obj Char,
		ObjectSubtype r obj (SList Word8),
		ObjectSubtype r obj (InputPort Word8 m),
		ObjectSubtype r obj (OutputPort Word8 m),
		ObjectSubtype r obj EOFObjType,
		ParseObject r obj,
		InterpretObject m r obj,
		ParserError m obj,
		Build cm r
		) =>
	 RefBindings cm r obj;
	portBindings = concatenateList
		[
		-- 6.6.1 Ports
		addProcBinding	"input-port?"					isInputPortP,
		addProcBinding	"output-port?"					isOutputPortP,
		addProcNBinding	"close-input-port"				inputPortClosePN,
		addProcNBinding	"close-output-port"				outputPortClosePN,

		-- 6.6.2 Input
		addProcBinding	"eof-object?"					isEOFObjectP,
		addProcBinding	"port-read-byte"				portReadByteP,							-- nonstandard
		addProcBinding	"port-peek-byte"				portPeekByteP,							-- nonstandard
		addProcBinding	"port-byte-ready?"				portByteReadyP,							-- nonstandard
		addProcBinding	"port-read-char-latin1"			portReadCharLatin1P,					-- nonstandard
		addProcBinding	"port-read-char-utf8"			portReadCharUTF8P,						-- nonstandard
		addProcBinding	"port-peek-char-latin1"			portPeekCharLatin1P,					-- nonstandard
		addProcBinding	"port-read-latin1"				portReadLatin1P,						-- nonstandard
		addProcBinding	"port-read-utf8"				portReadUTF8P,							-- nonstandard

		-- 6.6.3 Output
		addProcNBinding	"port-write-byte"				portWriteBytePN,						-- nonstandard
		addProcNBinding	"port-write-byte-array"			portWriteByteArrayPN,					-- nonstandard
		addProcNBinding	"port-write-char-latin1"		portWriteCharLatin1PN,					-- nonstandard
		addProcNBinding	"port-write-char-utf8"			portWriteCharUTF8PN,					-- nonstandard
		addProcNBinding	"port-write-string-latin1"		portWriteStringLatin1PN,				-- nonstandard
		addProcNBinding	"port-write-string-utf8"		portWriteStringUTF8PN					-- nonstandard
		];

	eqBindings ::
		(
		Eqv obj,
		Eq obj,
		Eq (r obj),
		ObjectSubtype r obj Bool,
		InterpretObject m r obj,
		Build cm r
		) =>
	 RefBindings cm r obj;
	eqBindings = concatenateList
		[
		-- 6.1 Equivalence Predicates
		addProcBinding			"eqv?"				eqvP,
		addProcBinding			"eq?"				eqvP,

		-- 6.3.2 Pairs and Lists
		addProcBinding			"list?"				isListP
		];

	setBindings ::
		(
		ObjectSubtype r obj Bool,
		ObjectSubtype r obj Word8,
		ObjectSubtype r obj Char,
		ObjectSubtype r obj Integer,
		ObjectSubtype r obj (SRefArray r Word8),
		ObjectSubtype r obj (SRefArray r Char),
		ObjectSubtype r obj (SRefArray r obj),
		InterpretObject m r obj,
		FullBuild m r,
		Build cm r
		) =>
	 RefBindings cm r obj;
	setBindings = concatenateList
		[
		-- 6.3.2 Pairs and Lists
		addProcNBinding			"set-car!"			setCarPN,
		addProcNBinding			"set-cdr!"			setCdrPN,

		-- 6.3.5 Strings
		addProcNBinding			"string-set!"		stringSetPN,

		-- Byte Arrays
		addProcNBinding			"byte-array-set!"	byteArraySetPN,

		-- 6.3.6 Vectors
		addProcNBinding			"vector-set!"		vectorSetPN,
		addProcNBinding			"vector-fill!"		vectorFillPN
		];

	systemBindings ::
		(
		ObjectSubtype r obj EIReal,
		InterpretObject m r obj,
		Build cm r
		) =>
	 (forall a. IO a -> m a) ->
	 RefBindings cm r obj;
	systemBindings lifter = concatenateList
		[
		addProcBinding	"current-time"			(currentTimeP lifter)
		];

	systemPortBindings ::
		(
		ObjectSubtype r obj (SList Char),
		ObjectSubtype r obj (OutputPort Word8 m),
		ObjectSubtype r obj (InputPort Word8 m),
		InterpretObject m r obj,
		Build cm r,
		?system :: System m
		) =>
	 RefBindings cm r obj;
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
