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

module Org.Org.Semantic.HScheme.SExpParser where
	{
	import Org.Org.Semantic.HScheme.PortProcedures;
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HScheme.SExpChars;
	import Org.Org.Semantic.HBase;

	class
		(
		Scheme m r,
		StoppableMonadOrParser (Object r m) Char p,
		LiftedParser Char m p
		) =>
	 SchemeParser m r p | p -> m;

	instance
		(
		Scheme m r,
		StoppableMonadOrParser (Object r m) Char p,
		LiftedParser Char m p
		) =>
	 SchemeParser m r p;

	runParser :: (Scheme m r,?refType :: Type (r ())) =>
	 m (Maybe t) -> OrStreamParser m t a -> m a;
	runParser source parser = do
		{
		mr <- runOrStreamParser source parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwSimpleError "no-parse";
			};
		};

	runParserString :: (Scheme m r,?refType :: Type (r ())) =>
	 [t] -> OrListParser m t a -> m ([t],a);
	runParserString text parser = do
		{
		mr <- doOrListParser text parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwSimpleError "no-parse";
			};
		};

	unexpectedCharError :: (SchemeParser m r p,?refType :: Type (r ())) =>
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

	mOrUnexpectedCharError :: (SchemeParser m r p,?refType :: Type (r ())) =>
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
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
		?refType :: Type (r ()),
		SchemeParser m r p
		) =>
--	 m (Object r m) -> p (Object r m);
	 m a -> p a;
	plift = parserLift;

	expressionParse ::
		(
		?refType :: Type (r ()),
		SchemeParser m r p
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

	expressionOrEndParse :: (SchemeParser m r p) =>
	 p (Maybe (Object r m));
	expressionOrEndParse = let {?refType=Type} in 
		(expressionParse >>= (return . Just)) |||
		(optionalWhitespaceParse >> streamEndParse >> (return Nothing)) |||
		(unexpectedCharError "input");

	expressionsParse :: (SchemeParser m r p) =>
	 p [Object r m];
	expressionsParse = accumulateMaybeSource expressionOrEndParse;

	parseFromCharSource :: (Scheme m r,?refType :: Type (r ())) =>
	 m (Maybe Char) -> m (Maybe (Object r m));
	parseFromCharSource source = runParser source expressionOrEndParse;

	parseFromPort :: (Scheme m r,?refType :: Type (r ())) =>
	 InputPort Word8 m -> m (Maybe (Object r m));
	parseFromPort port = parseFromCharSource (parseUTF8Char (ipRead port));

	parseFromString :: (Scheme m r,?refType :: Type (r ())) =>
	 String -> m (String,Maybe (Object r m));
	parseFromString text = runParserString text expressionOrEndParse;

	parseAllFromString :: (Scheme m r,?refType :: Type (r ())) =>
	 String ->
	 m [Object r m];
	parseAllFromString text = do	
		{
		(_,objs) <- runParserString text expressionsParse;
		return objs;
		};

	portReadP :: (Scheme m r,?refType :: Type (r ())) =>
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
