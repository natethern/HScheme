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
	import Org.Org.Semantic.HScheme.Procedures;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HScheme.SExpChars;
	import Org.Org.Semantic.HBase;


	runParser :: (Monad m) =>
	 InputPort c m -> RecoverableStreamParser m (Maybe c) a -> m a;
	runParser port parser = runRecoverableStreamParser (ipRead port) parser;
--	 InputPort c m -> AddableStreamParser m (Maybe c) a -> m a;
--	runParser port parser = runAddableStreamParser (ipRead port) parser;

	restOfLineParse :: (MonadOrParser (Maybe Char) p) =>
	 p ();
	restOfLineParse = do
		{
		mZeroOrMore (matchCharacterParse (not . isLineBreak));
		((matchCharacterParse isLineBreak) >> (return ())) ||| (return ());
		};

	commentParse :: (MonadOrParser (Maybe Char) p) =>
	 p ();
	commentParse = do
		{
		isCharacterParse ';';
		restOfLineParse;
		};

	optionalWhitespaceParse :: (MonadOrParser (Maybe Char) p) =>
	 p ();
	optionalWhitespaceParse = do
		{
		mZeroOrMore (((matchCharacterParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};

	whitespaceParse :: (MonadOrParser (Maybe Char) p) =>
	 p ();
	whitespaceParse = do
		{
		mOneOrMore (((matchCharacterParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};

	identifierParse :: (MonadOrParser (Maybe Char) p) =>
	 p String;
	identifierParse = do
		{
		n <- matchCharacterParse allowedIdentifier1;
		ns <- mZeroOrMore ((matchCharacterParse allowedIdentifierR) >>= (return . toLowerCase));
		return (toLowerCase n:ns);
		};

	digitParse :: (MonadOrParser (Maybe Char) p) =>
	 p Integer;
	digitParse = do
		{
		c <- characterParse;
		case (getDecimalDigit c) of
			{
			Nothing -> mzero;
			Just n -> return n;
			};
		};

	assembleDigits :: Integer -> [Integer] -> Integer;
	assembleDigits i (n:ns) = assembleDigits (i*10 + n) ns;
	assembleDigits i [] = i;

	digitsParse :: (MonadOrParser (Maybe Char) p) =>
	 p Integer;
	digitsParse = do
		{
		digits <- mOneOrMore digitParse;
		return (assembleDigits 0 digits);
		};
	
	plusDigitsParse :: (MonadOrParser (Maybe Char) p) =>
	 p Integer;
	plusDigitsParse = do
		{
		isCharacterParse '+';
		digitsParse;
		};
	
	minusDigitsParse :: (MonadOrParser (Maybe Char) p) =>
	 p Integer;
	minusDigitsParse = do
		{
		isCharacterParse '-';
		n <- digitsParse;
		return (- n);
		};

	integerParse :: (MonadOrParser (Maybe Char) p) =>
	 p Integer;
	integerParse = digitsParse ||| plusDigitsParse ||| minusDigitsParse;
	
	listContentsParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 p (Object r m);
	listContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			isCharacterParse '.';
			mOrFail "dotted pair tail"  expressionParse; 
			}) |||
		 (do
			{
			head <- expressionParse;
			tail <- listContentsParse;
			lift (cons head tail);
			}) |||
		 (return NilObject);
		};
	
	listParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 p (Object r m);
	listParse = do
		{
		isCharacterParse '(';
		list <- listContentsParse;
		optionalWhitespaceParse;
		mOrFail "list" (isCharacterParse ')');
		return list;
		};

	specialCharParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 String -> Symbol -> p (Object r m);
	specialCharParse s symbol = do
		{
		isStringParse s;
		h2 <- mOrFail (s++"-form") expressionParse;
		tail <- lift (cons h2 NilObject);
		lift (cons (SymbolObject symbol) tail);
		};

	quotedParse :: 
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 p (Object r m);
	quotedParse =
		(specialCharParse "'" (MkSymbol "quote")) |||
		(specialCharParse "`" (MkSymbol "quasiquote")) |||
		(specialCharParse ",@" (MkSymbol "unquote-splicing")) |||
		(specialCharParse "," (MkSymbol "unquote"));

	hashLiteralParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p
		) => 
	 p (Object r m);
	hashLiteralParse = do
		{
		isCharacterParse '#';
		mOrFail "# literal" ((do
			{
			isCharacterParse 't';
			return (BooleanObject True);
			}) ||| (do
			{
			isCharacterParse 'f';
			return (BooleanObject True);
			}) ||| (do
			{
			isCharacterParse '\\';
			c <- mOrFail "# literal" characterParse;
			return (CharObject c);
			}));
		};

	escapedCharInStringParse :: (MonadOrParser (Maybe Char) p) =>
	 p Char;
	escapedCharInStringParse = do
		{
		isCharacterParse '\\';
		characterParse;
		};

	charInStringParse :: (MonadOrParser (Maybe Char) p) =>
	 p Char;
	charInStringParse = escapedCharInStringParse ||| (isntCharacterParse '"');

	stringLiteralParse ::
		(
		StoppableMonadOrParser String (Maybe Char) p
		) =>
	 p String;
	stringLiteralParse = do
		{
		isCharacterParse '"';
		mOrFail "string literal" (do
			{
			s <- mZeroOrMore charInStringParse;
			isCharacterParse '"';
			return s;
			});
		};

	plift ::
		(
		Scheme m r,
		MonadOrParser (Maybe Char) p,
		LiftedMonad m p
		) =>
	 Type (r ()) -> m (Object r m) -> p (Object r m);
	plift t = lift;

	expressionParse' ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 Type (r ()) -> p (Object r m);
	expressionParse' t = do
		{
		optionalWhitespaceParse;
		 (integerParse >>= (return . NumberObject . fromInteger))	|||
		 ((isCharacterParse '+') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 ((isCharacterParse '-') >>= (\c -> return (SymbolObject (MkSymbol [c]))))	|||
		 (identifierParse >>= (return . SymbolObject . MkSymbol))	|||
		 hashLiteralParse		|||
		 quotedParse			|||
		 (stringLiteralParse >>= ((plift t) . getConvert . MkStringType))	|||
		 listParse				;
		};

	expressionParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 p (Object r m);
	expressionParse = expressionParse' Type;

	expressionOrEndParse ::
		(
		Scheme m r,
		StoppableMonadOrParser String (Maybe Char) p,
		LiftedMonad m p
		) =>
	 p (Maybe (Object r m));
	expressionOrEndParse =
		(expressionParse >>= (return . Just)) |||
		(optionalWhitespaceParse >> streamEndParse >> (return Nothing)) |||
		(unexpectedCharacterError "input");

	portRead :: (Scheme m r) =>
	 InputPort Char m -> m (Maybe (Object r m));
	portRead port = runParser port expressionOrEndParse;

	portReadP :: (Scheme m r) =>
	 Type (r ()) -> (InputPort Char m,()) -> m (Object r m);
	portReadP Type (port,()) = do
		{		
		mobj <- portRead port;
		case mobj of
			{
			Just obj -> return obj;
			Nothing -> return eofObject;
			};
		};
	}
