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

	digitParse :: (MonadOrParser Char p) =>
	 p Integer;
	digitParse = do
		{
		c <- tokenParse;
		case (getDecimalDigit c) of
			{
			Nothing -> mzero;
			Just n -> return n;
			};
		};

	assembleDigits :: Integer -> [Integer] -> Integer;
	assembleDigits i (n:ns) = assembleDigits (i*10 + n) ns;
	assembleDigits i [] = i;

	digitsParse :: (MonadOrParser Char p) =>
	 p Integer;
	digitsParse = do
		{
		digits <- mOneOrMore digitParse;
		return (assembleDigits 0 digits);
		};
	
	plusDigitsParse :: (MonadOrParser Char p) =>
	 p Integer;
	plusDigitsParse = do
		{
		isTokenParse '+';
		digitsParse;
		};
	
	minusDigitsParse :: (MonadOrParser Char p) =>
	 p Integer;
	minusDigitsParse = do
		{
		isTokenParse '-';
		n <- digitsParse;
		return (- n);
		};

	integerParse :: (MonadOrParser Char p) =>
	 p Integer;
	integerParse = digitsParse ||| plusDigitsParse ||| minusDigitsParse;
	
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
		mOrUnexpectedCharError "list" (isTokenParse ')');
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
		 (integerParse >>= (return . NumberObject . fromInteger))	|||
		 ((isTokenParse '+') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 ((isTokenParse '-') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 (identifierParse >>= (return . SymbolObject . MkSymbol))	|||
		 hashLiteralParse		|||
		 quotedParse			|||
		 (stringLiteralParse >>= (plift . getConvert . MkSList))	|||
		 listParse				;
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
