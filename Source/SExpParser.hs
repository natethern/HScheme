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

module SExpParser where
	{
	import Procedures;
	import Object;
	import Parser;
	import SExpChars;
	import Unicode;
	import LiftedMonad;

	type TextParser = Parser (Maybe Char);

	isJust :: (a -> Bool) -> (Maybe a) -> Maybe a;
	isJust test Nothing = Nothing;
	isJust test (Just a) = if test a then Just a else Nothing;

	-- returns whether it chomped some ws
	whitespaceP :: (Monad m) => TextParser m Bool;
	whitespaceP = do
		{
		mc <- currentC;
		case isJust isWhitespace mc of
			{
			(Just _) -> do
				{
				nextC;
				whitespaceP;
				return True;
				};
			Nothing -> return False;
			};
		};

	identifierRestP :: (Monad m) => TextParser m String;
	identifierRestP = do
		{
		mc <- currentC;
		case isJust allowedIdentifierR mc of
			{
			(Just c) -> do
				{
				nextC;
				rest <- identifierRestP;
				return (toLowerCase c:rest);
				};
			Nothing -> return [];
			};
		};

	identifierP :: (Monad m) => TextParser m (Maybe String);
	identifierP = do
		{
		mc <- currentC;
		case isJust allowedIdentifier1 mc of
			{
			(Just c) -> do
				{
				nextC;
				rest <- identifierRestP;
				return (Just (toLowerCase c:rest));
				};
			Nothing -> return Nothing;
			};
		};

	integerRestP :: (Monad m) => Integer -> TextParser m Integer;
	integerRestP prev = do
		{
		mc <- currentC;
		case mc of
			{
			Nothing -> return prev;
			Just c -> case (getDecimalDigit c) of
				{
				Nothing -> return prev;
				Just n -> do
					{
					nextC;
					integerRestP (prev*10 + n);
					};
				};
			};
		};

	integerP :: (Monad m) => TextParser m (Maybe Integer);
	integerP = do
		{
		mc <- currentC;
		case (mc) of
			{
			Nothing -> return Nothing;
			Just c -> case (getDecimalDigit c) of
				{
				Nothing -> return Nothing;
				Just n -> do
					{
					nextC;
					i <- integerRestP n;
					return (Just i);
					};
				};
			};
		};

	listRestP :: (Scheme x m r) => TextParser m (Object r m);
	listRestP = do
		{
		whitespaceP;
		mc <- currentC;
		case mc of
			{
			Nothing -> fail "unterminated list";
			Just ')' -> do
				{
				nextC;
				return NilObject;
				};
			Just c -> do
				{
				mhead <- expressionP;
				case mhead of
					{
					Nothing -> fail "unterminated list";
					Just head -> do
						{
						tail <- listRestP;
						mlift (cons (head,tail));
						};
					};
				};
			};
		};

	listP :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	listP = do
		{
		mc <- currentC;
		case mc of
			{
			Just '(' -> do
				{
				nextC;
				list <- listRestP;
				return (Just list);
				};
			_ -> return Nothing;
			};
		};

	specialCharP :: (Scheme x m r) => Symbol -> TextParser m (Maybe (Object r m));
	specialCharP symbol = do
		{
		nextC;
		mh2 <- expressionP;
		case mh2 of
			{
			Nothing -> fail ("unexpected EOF after "++(unSymbol symbol));
			Just h2 -> do
				{					
				tail <- mlift (cons (h2,NilObject));
				qf <- mlift (cons (SymbolObject symbol,tail));
				return (Just qf);
				};
			};
		};

	quotedP :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	quotedP = do
		{
		mc <- currentC;
		case mc of
			{
			Just '\'' -> specialCharP (MkSymbol "quote");
			Just '`' -> specialCharP (MkSymbol "quasiquote");
			Just ',' -> specialCharP (MkSymbol "unquote");
			_ -> return Nothing;
			};
		};

	ifJust :: (Monad m) =>
	 Maybe a -> (a -> m b) -> m b -> m b;
	ifJust ma justClause nothingClause = case ma of
		{
		Just a -> justClause a;
		Nothing -> nothingClause;
		};
	
	checkParse :: (Monad m) =>
	 m (Maybe a) -> (a -> b) -> m (Maybe b) -> m (Maybe b);
	checkParse mma just nothingClause = do
		{
		ma <- mma;
		ifJust ma (return . Just . just) nothingClause;
		};

	expressionP :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	expressionP = do
		{
		whitespaceP;
		checkParse integerP (NumberObject . fromInteger)
		 (checkParse identifierP (SymbolObject . MkSymbol)
		  (checkParse quotedP id
		   (checkParse listP id (do
			{
			mc <- currentC;
			case mc of
				{
				Just c -> fail ("unrecognised char, '"++[c]++"'");
				Nothing -> return Nothing;
				};
			}))));
		};
	
	manyP :: (Monad m) => Parser c m (Maybe a) -> Parser c m [a];
	manyP oneP = do
		{
		mfirst <- oneP;
		case mfirst of
			{
			Just first -> do
				{
				rest <- manyP oneP;
				return (first:rest);
				};
			Nothing -> return [];
			};
		};
	
	expressionsP :: (Scheme x m r) => TextParser m [Object r m];
	expressionsP = manyP expressionP;
	}
