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

module Org.Org.Semantic.HScheme.Parse.SExpParser where
	{
	import Org.Org.Semantic.HScheme.Parse.SExpChars;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase.Text.Properties.Case;
	import Org.Org.Semantic.HBase.Text.Properties.Misc;
	import Org.Org.Semantic.HBase;

	class
		(
		BuildThrow cm (Object r m) r,
		StoppableMonadOrParser (Object r m) Char p,
		LiftedParser Char cm p
		) =>
	 SchemeParser cm m r p | p -> cm;

	instance
		(
		BuildThrow cm (Object r m) r,
		StoppableMonadOrParser (Object r m) Char p,
		LiftedParser Char cm p
		) =>
	 SchemeParser cm m r p;

	runParser :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 cm (Maybe t) -> OrStreamParser cm t a -> cm a;
	runParser source parser = do
		{
		mr <- runOrStreamParser source parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwSimpleError "no-parse";
			};
		};

	runParserString :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 [t] -> OrListParser cm t a -> cm ([t],a);
	runParserString text parser = do
		{
		mr <- doOrListParser text parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwSimpleError "no-parse";
			};
		};

	runParserOnlyString :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 [t] -> OrListParser cm t a -> cm (Maybe a);
	runParserOnlyString text parser = runOrListParser text (do
		{
		a <- parser;
		streamEndParse;
		return a;
		});

	unexpectedCharError :: (SchemeParser cm m r p,?objType :: Type (Object r m)) =>
	 String -> p a;
	unexpectedCharError context = do
		{
		contextObj <- parserLift (getConvert (MkSList context));
		(do
			{
			t <- tokenParse;
			parserLift (throwArgError "inappropriate-character" [CharObject t,contextObj]);
			}) |||
		 (parserLift (throwArgError "inappropriate-stream-end" [contextObj]));
		};

	mOrUnexpectedCharError :: (SchemeParser cm m r p,?objType :: Type (Object r m)) =>
	 String -> p a -> p a;
	mOrUnexpectedCharError s ma = ma ||| (unexpectedCharError s);


	-- Parsers

	restOfLineParse :: (MonadOrParser Char p) =>
	 p ();
	restOfLineParse = do
		{
		mZeroOrMore (matchTokenParse (not . isLineBreak));
		((matchTokenParse isLineBreak) >> (return ())) ||| (return ());
		};

	commentParse :: (MonadOrParser Char p) =>
	 p ();
	commentParse = do
		{
		isTokenParse ';';
		restOfLineParse;
		};

	optionalWhitespaceParse :: (MonadOrParser Char p) =>
	 p ();
	optionalWhitespaceParse = do
		{
		mZeroOrMore (((matchTokenParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};

	whitespaceParse :: (MonadOrParser Char p) =>
	 p ();
	whitespaceParse = do
		{
		mOneOrMore (((matchTokenParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};

	identifierParse :: (MonadOrParser Char p) =>
	 p String;
	identifierParse = do
		{
		n <- matchTokenParse allowedIdentifier1;
		ns <- mZeroOrMore ((matchTokenParse allowedIdentifierR) >>= (return . toLowerCase));
		return (toLowerCase n:ns);
		};

	prefixParse :: (MonadOrParser Char p) =>
	 p Char;
	prefixParse = do
		{
		isTokenParse '#';
		tokenParse;
		};

	setMParse :: (
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 (Maybe a) -> a -> p (Maybe a);
	setMParse Nothing a = return (Just a);
	setMParse (Just _) _ = unexpectedCharError "number prefix";

	setRadix :: (
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 (Maybe Word8,Maybe Bool) -> Word8 -> p (Maybe Word8,Maybe Bool);
	setRadix (mr,me) r = do
		{
		mr' <- setMParse mr r;
		return (mr',me);
		};

	setExactness :: (
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 (Maybe Word8,Maybe Bool) -> Bool -> p (Maybe Word8,Maybe Bool);
	setExactness (mr,me) e = do
		{
		me' <- setMParse me e;
		return (mr,me');
		};

	preficesParse :: (
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 (Maybe Word8,Maybe Bool) -> p (Maybe Word8,Maybe Bool);
	preficesParse mm = (do
		{
		c <- prefixParse;
		mm' <- case c of
			{
			'b' -> setRadix mm 2;
			'o' -> setRadix mm 8;
			'd' -> setRadix mm 10;
			'x' -> setRadix mm 16;
			'i' -> setExactness mm False;
			'e' -> setExactness mm True;
			_ -> mzero;
			};
		preficesParse mm';
		}) ||| return mm;

	digitParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p Word8;
	digitParse = let {?getDecimalDigit=Just getDecimalDigit;} in readDigit;

	digitsPointParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p ([Word8],Maybe [Word8]);
	digitsPointParse = (do
		{
		isTokenParse '.';
		ds <- mZeroOrMore digitParse;
		return ([],Just ds);
		}) ||| (do
		{
		i <- digitParse;
		(is,mds) <- digitsPointParse;
		return (i:is,mds);
		}) ||| (return ([],Nothing));

	hashesPointParse :: (MonadOrParser Char p) =>
	 p ([Word8],Bool);
	hashesPointParse = (do
		{
		isTokenParse '.';
		mZeroOrMore (isTokenParse '#');
		return ([],False);
		}) ||| (do
		{
		isTokenParse '#';
		(is,mds) <- hashesPointParse;
		return (0:is,False);
		}) ||| (return ([],True));

	radixPower :: Word8 -> Int -> Integer;
	radixPower 2 n = bitShift (convert n) 1;
	radixPower 4 n = bitShift (convert n*2) 1;
	radixPower 8 n = bitShift (convert n*3) 1;
	radixPower 16 n = bitShift (convert n*4) 1;
	radixPower p n = (convert p) ^ (convert n);

	convertRational :: (?exactness :: Maybe Bool) =>
	 Bool -> NaNExtended Rational -> EIReal;
	convertRational exactnessGuess r = if unJust exactnessGuess ?exactness
	 then convert (fmap Finite r)
	 else convert (approximate (fmap Finite r) :: Double);

	uintegerParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p (Integer,Bool);
	uintegerParse = do
		{
		ds <- mOneOrMore digitParse;
		hs <- mZeroOrMore (do
			{
			isTokenParse '#';
			return 0;
			});
		return (addUpDigits 0 (ds ++ hs),case hs of
			{
			[] -> True;
			(_:_) -> False;
			});
		};

	decimalParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	decimalParse = do
		{
		(i,md) <- digitsPointParse;
		(pre,post,e) <- case md of
			{
			Just d -> do
				{
				mZeroOrMore (isTokenParse '#');
				return (i,d,False);
				};
			Nothing -> case i of
				{
				[] -> mzero;	-- because digitsPointParse didn't match anything at all
				_ -> do
					{
					(is,e) <- hashesPointParse;
					return (i++is,[],e);
					};
				};
			};
		return (convertRational e (divide (radixPower ?radix (length post)) (addUpDigits 0 (pre ++ post) :: Integer)));
		};

	urealParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	urealParse = (do
		{
		(i1,e1) <- uintegerParse;
		isTokenParse '/';
		(i2,e2) <- uintegerParse;
		return (convertRational (e1 && e2) (divide i2 i1));
		}) ||| decimalParse;

	realParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	realParse = (do
		{
		isTokenParse '+';
		urealParse;
		}) ||| (do
		{
		isTokenParse '-';
		n <- urealParse;
		return (negate n);
		}) ||| urealParse;

	imaginaryParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	imaginaryParse = do
		{
		neg <- (isTokenParse '+' >> return False) ||| (isTokenParse '-' >> return True);
		mn <- mOptional urealParse;
		isTokenParse 'i';
		return (let {n = unJust 1 mn} in if neg then negate n else n);
		};

	complexParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p Number;
	complexParse = (do
		{
		n1 <- realParse;
		(do
			{
			isTokenParse '@';
			n2 <- realParse;
			return (polarComplex n1 n2);
			}) |||
		(do
			{
			n2 <- imaginaryParse;
			return (rectComplex n1 n2);
			}) |||
		(return (rectComplex n1 zero));
		}) ||| (do
		{
		n <- imaginaryParse;
		return (rectComplex zero n);
		});

	numberParse :: (
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p Number;
	numberParse = do
		{
		(mr,me) <- preficesParse (Nothing,Nothing);
		let
			{
			?radix = unJust 10 mr;
			?exactness = me;
			} in
		 complexParse;
		};

	listContentsParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p (Object r m);
	listContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			isTokenParse '.';
			mOrUnexpectedCharError "dotted pair tail"  expressionParse; 
			}) |||
		 (do
			{
			head <- expressionParse;
			tail <- listContentsParse;
			parserLift (cons head tail);
			}) |||
		 (return NilObject);
		};
	
	listParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p (Object r m);
	listParse = do
		{
		isTokenParse '(';
		list <- listContentsParse;
		optionalWhitespaceParse;
		mOrUnexpectedCharError "list end" (isTokenParse ')');
		return list;
		};
	
	vectorContentsParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p [Object r m];
	vectorContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			head <- expressionParse;
			tail <- vectorContentsParse;
			return (head:tail);
			}) |||
		 (return []);
		};

	byteArrayContentsParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p [Word8];
	byteArrayContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			head <- readHexFixedDigits 2;
			tail <- byteArrayContentsParse;
			return (head:tail);
			}) |||
		 (return []);
		};

	specialCharParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 String -> Symbol -> p (Object r m);
	specialCharParse s symbol = do
		{
		isListParse s;
		h2 <- mOrUnexpectedCharError (s++"-form") expressionParse;
		tail <- parserLift (cons h2 NilObject);
		parserLift (cons (SymbolObject symbol) tail);
		};

	quotedParse :: 
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p (Object r m);
	quotedParse =
		(specialCharParse "'" (MkSymbol "quote")) |||
		(specialCharParse "`" (MkSymbol "quasiquote")) |||
		(specialCharParse ",@" (MkSymbol "unquote-splicing")) |||
		(specialCharParse "," (MkSymbol "unquote"));

	characterConstantParse :: (MonadOrParser Char p) =>
	 p Char;
	characterConstantParse = (do
		{
		isListParse "space";
		return ' ';
		}) ||| (do
		{
		isListParse "newline";
		return '\n';
		}) ||| (do
		{
		isListParse "tab";
		return '\t';
		}) ||| (do
		{
		(isTokenParse 'u' ||| isTokenParse 'U');
		i <- readHexDigits;
		return (nthWrap i);
		}) ||| tokenParse;

	hashLiteralParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) => 
	 p (Object r m);
	hashLiteralParse = do
		{
		isTokenParse '#';
		mOrUnexpectedCharError "# literal" ((do
			{
			isTokenParse 't';
			return (BooleanObject True);
			}) ||| (do
			{
			isTokenParse 'f';
			return (BooleanObject False);
			}) ||| (do
			{
			isTokenParse '\\';
			c <- mOrUnexpectedCharError "character literal" characterConstantParse;
			return (CharObject c);
			}) ||| (do
			{
			isTokenParse '(';
			mOrUnexpectedCharError "vector" (do
				{
				vector <- vectorContentsParse;
				optionalWhitespaceParse;
				isTokenParse ')';
				arr <- plift (makeSRefArray vector);
				return (VectorObject arr);
				});
			}) ||| (do
			{
			isTokenParse 'x';
			mOrUnexpectedCharError "byte array" (do
				{
				isTokenParse '(';
				bytes <- byteArrayContentsParse;
				optionalWhitespaceParse;
				isTokenParse ')';
				arr <- plift (makeSRefArray bytes);
				return (ByteArrayObject arr);
				});
			}));
		};

	escapedCharInStringParse :: (MonadOrParser Char p) =>
	 p Char;
	escapedCharInStringParse = do
		{
		isTokenParse '\\';
		(do
			{
			isTokenParse 'n';
			return '\n';
			})
		||| (do
			{
			isTokenParse 't';
			return '\t';
			})
		||| (do
			{
			isTokenParse 'u';
			i <- readHexFixedDigits 4;
			return (nthWrap i);
			})
		||| (do
			{
			isTokenParse 'U';
			i <- readHexFixedDigits 6;
			return (nthWrap i);
			})
		||| tokenParse;
		};

	charInStringParse :: (MonadOrParser Char p) =>
	 p Char;
	charInStringParse = escapedCharInStringParse ||| (isntTokenParse '"');

	stringLiteralParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p String;
	stringLiteralParse = do
		{
		isTokenParse '"';
		mOrUnexpectedCharError "string literal" (do
			{
			s <- mZeroOrMore charInStringParse;
			isTokenParse '"';
			return s;
			});
		};

	plift ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 cm a -> p a;
	plift = parserLift;

	expressionParse ::
		(
		?objType :: Type (Object r m),
		SchemeParser cm m r p
		) =>
	 p (Object r m);
	expressionParse = do
		{
		optionalWhitespaceParse;
		 (numberParse >>= (return . NumberObject))	|||
		 ((isTokenParse '+') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 ((isTokenParse '-') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 (identifierParse >>= (return . SymbolObject . MkSymbol))	|||
		 hashLiteralParse		|||
		 quotedParse			|||
		 (stringLiteralParse >>= (plift . getConvert . MkSList))	|||
		 listParse				|||
		 ((streamEndParse ||| (unexpectedCharError "input")) >> mzero);
		};

	expressionOrEndParse :: (SchemeParser cm m r p) =>
	 p (Maybe (Object r m));
	expressionOrEndParse = let {?objType=MkType} in 
		(expressionParse >>= (return . Just)) |||
		(optionalWhitespaceParse >> streamEndParse >> (return Nothing)) |||
		(unexpectedCharError "input");

	expressionsParse :: (SchemeParser cm m r p) =>
	 p [Object r m];
	expressionsParse = accumulateMaybeSource expressionOrEndParse;

	parseFromCharSource :: (Scheme m r,?objType :: Type (Object r m)) =>
	 m (Maybe Char) -> m (Maybe (Object r m));
	parseFromCharSource source = runParser source expressionOrEndParse;

	parseFromPort :: (Scheme m r,?objType :: Type (Object r m)) =>
	 InputPort Word8 m -> m (Maybe (Object r m));
	parseFromPort port = parseFromCharSource (parseUTF8Char (ipRead port));

	parseFromString :: (Scheme m r,?objType :: Type (Object r m)) =>
	 String -> m (String,Maybe (Object r m));
	parseFromString text = runParserString text expressionOrEndParse;

	parseNumberOnlyFromString :: (Scheme m r,?objType :: Type (Object r m)) =>
	 String -> m (Maybe Number);
	parseNumberOnlyFromString text = runParserOnlyString text numberParse;

	parseAllFromString :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 String ->
	 cm [Object r m];
	parseAllFromString text = do	
		{
		(_,objs) <- runParserString text expressionsParse;
		return objs;
		};

	parseFromPortBulk ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 InputPort Word8 cm ->
	 cm [Object r m];
	parseFromPortBulk input = do	
		{
		text <- accumulateMaybeSource (parseUTF8Char (ipRead input));
		parseAllFromString text;
		};

	readWithProcs ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 (String -> cm (InputPort Word8 cm)) ->
	 String -> cm [Object r m];
	readWithProcs oif name = do
		{
		input <- oif name;
		objects <- parseFromPortBulk input;
		ipClose input;
		return objects;
		};

	portReadP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (InputPort Word8 m,()) -> m (Object r m);
	portReadP (port,()) = do
		{		
		mobj <- parseFromPort port;
		case mobj of
			{
			Just obj -> return obj;
			Nothing -> return eofObject;
			};
		};
	}
