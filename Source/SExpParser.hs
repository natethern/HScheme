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
	import Conversions;
	import Object;
	import Parser;
	import SExpChars;
	import Unicode;
	import LiftedMonad;
	import Subtype;
	import Type;

	type TextParser = Parser (Maybe Char);

	isJust :: (a -> Bool) -> (Maybe a) -> Maybe a;
	isJust test Nothing = Nothing;
	isJust test (Just a) = if test a then Just a else Nothing;

	commentP :: (Monad m) => TextParser m ();
	commentP = do
		{
		nextC;
		mc <- currentC;
		case mc of
			{
			Just c -> if isLineBreak c
			 then return ()
			 else commentP;
			Nothing -> return ();
			};
		};

	-- returns whether it chomped some ws
	whitespaceP :: (Monad m) => TextParser m Bool;
	whitespaceP = do
		{
		mc <- currentC;
		case mc of
			{
			Just ';' -> do
				{
				commentP;
				whitespaceP;
				return True;
				};
			Just c -> if isWhitespace c
			 then do
				{
				nextC;
				whitespaceP;
				return True;
				}
			 else return False;
			Nothing -> return False;
			};
		};

	tokenEndP :: (Monad m) => TextParser m Bool;
	tokenEndP = do
		{
		ws <- whitespaceP;
		if ws
		 then (return True)
		 else do
			{
			mc <- currentC;
			case mc of
				{
				Nothing -> return True;
				Just ')' -> return True;
				Just _ -> return False;
				};
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
			Nothing -> do
				{
				end <- tokenEndP;
				if end then (return []) else fail "junk char in identifier";
				};
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
			Nothing -> do
				{
				end <- tokenEndP;
				if end then (return prev) else fail "junk char in integer";
				};
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
			Just '.' -> do
				{
				nextC;
				mtail <- expressionP;
				case mtail of
					{
					Nothing -> fail "missing tail in dotted pair";
					Just tail -> do
						{
						whitespaceP;
						mc <- currentC;
						case mc of
							{
							Nothing -> fail "unterminated dotted pair";
							Just ')' -> do
								{
								nextC;
								return tail;
								};
							Just _ -> fail "extra junk in dotted pair";
							};
						};
					};
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
						mlift (cons head tail);
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

	plusMinusRestP :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe Integer);
	plusMinusRestP Type = do
		{
		end <- tokenEndP;
		if end
		 then return Nothing
		 else do
			{
			mi <- integerP;
			case mi of
				{
				Nothing -> fail "bad '+'/'-' token";
				Just _ -> return mi;
				};
			};
		};

	plusMinusP :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe (Object r m));
	plusMinusP t = do
		{
		mc <- currentC;
		case mc of
			{
			Just '+' -> do
				{
				nextC;
				mi <- plusMinusRestP t;
				case mi of
					{
					Nothing -> return (Just (SymbolObject (MkSymbol "+")));
					Just i -> return (Just (NumberObject (fromInteger i)));
					};
				};
			Just '-' -> do
				{
				nextC;
				mi <- plusMinusRestP t;
				case mi of
					{
					Nothing -> return (Just (SymbolObject (MkSymbol "-")));
					Just i -> return (Just (NumberObject (fromInteger (- i))));
					};
				};
			_ -> return Nothing;
			};
		};

	specialCharP :: (Scheme x m r) => Symbol -> TextParser m (Maybe (Object r m));
	specialCharP symbol = do
		{
		mh2 <- expressionP;
		case mh2 of
			{
			Nothing -> fail ("unexpected EOF after "++(unSymbol symbol));
			Just h2 -> do
				{					
				tail <- mlift (cons h2 NilObject);
				qf <- mlift (cons (SymbolObject symbol) tail);
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
			Just '\'' -> do
				{
				nextC;
				specialCharP (MkSymbol "quote");
				};
			Just '`' -> do
				{
				nextC;
				specialCharP (MkSymbol "quasiquote");
				};
			Just ',' -> do
				{
				nextC;
				mc <- currentC;
				case mc of
					{
					Just '@' -> do
						{
						nextC;
						specialCharP (MkSymbol "unquote-splicing");
						};
					_ -> specialCharP (MkSymbol "unquote");
					};
				};
			_ -> return Nothing;
			};
		};

	hashLiteralP :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	hashLiteralP = do
		{
		mc <- currentC;
		case mc of
			{
			Just '#' -> do
				{
				nextC;
				mc <- currentC;
				obj <- case mc of
					{
					Just '\\' -> do
						{
						nextC;
						mc <- currentC;
						case mc of
							{
							Just c -> do
								{
								nextC;
								return (CharObject c);
								};
							_ -> fail "unterminated # literal";
							};
						};
					Just 't' -> do
						{
						nextC;
						return (BooleanObject True);
						};
					Just 'f' -> do
						{
						nextC;
						return (BooleanObject False);
						};
					_ -> fail "unrecognised # literal";
					};
				return (Just obj);
				};
			_ -> return Nothing;
			};
		};

	stringLiteralRestP :: (Scheme x m r) => Type (r ()) -> TextParser m String;
	stringLiteralRestP t = do
		{
		mc <- currentC;
		case mc of
			{
			Just '"' -> do
				{
				nextC;
				return "";
				};
			Just c -> do
				{
				nextC;
				cs <- stringLiteralRestP t;
				return (c:cs);
				};
			Nothing -> fail "unterminated string";
			};
		};

	stringLiteralP :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe String);
	stringLiteralP t = do
		{
		mc <- currentC;
		case mc of
			{
			Just '"' -> do
				{
				nextC;
				mc <- currentC;
				str <- stringLiteralRestP t;
				return (Just str);
				};
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
	 m (Maybe a) -> (a -> m b) -> m (Maybe b) -> m (Maybe b);
	checkParse mma just nothingClause = do
		{
		ma <- mma;
		ifJust ma (\a -> do
			{
			b <- just a;
			return (Just b);
			}) nothingClause;
		};

	expressionP' :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe (Object r m));
	expressionP' t = do
		{
		whitespaceP;
		checkParse integerP (return . NumberObject . fromInteger)
		 (checkParse identifierP (return . SymbolObject . MkSymbol)
		  (checkParse hashLiteralP return
		   (checkParse quotedP return
		    (checkParse (stringLiteralP t) (mlift . getConvert . MkStringType)
		     (checkParse (plusMinusP t) return
		      (checkParse listP return (do
			{
			mc <- currentC;
			case mc of
				{
				Just c -> fail ("unrecognised char, '"++[c]++"'");
				Nothing -> return Nothing;
				};
			})))))));
		};
	
	expressionP :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	expressionP = expressionP' Type;
	
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
	
	expressionsP :: (Scheme x m r) => Type (r ()) -> TextParser m [Object r m];
	expressionsP Type = manyP expressionP;
	}
