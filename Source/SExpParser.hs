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
	import PortProcedures;
	import Procedures;
	import Conversions;
	import Object;
	import Port;
	import InputPortParser;
	import SExpChars;
	import Unicode;
	import LiftedMonad;
	import Subtype;
	import Type;

	type TextParser = Parser Char;

	isJust :: (a -> Bool) -> (Maybe a) -> Maybe a;
	isJust test Nothing = Nothing;
	isJust test (Just a) = if test a then Just a else Nothing;

	restOfLineParser :: (Monad m) => TextParser m ();
	restOfLineParser = do
		{
		mc <- currentC;
		case mc of
			{
			Just c -> if isLineBreak c
			 then return ()
			 else do
				{
				nextC;
				restOfLineParser;
				};
			Nothing -> return ();
			};
		};

	-- returns whether it chomped some ws
	whitespaceParser :: (Monad m) => TextParser m Bool;
	whitespaceParser = do
		{
		mc <- currentC;
		case mc of
			{
			Just ';' -> do
				{
				nextC;
				restOfLineParser;
				whitespaceParser;
				return True;
				};
			Just c -> if isWhitespace c
			 then do
				{
				nextC;
				whitespaceParser;
				return True;
				}
			 else return False;
			Nothing -> return False;
			};
		};

	tokenEndParser :: (Monad m) => TextParser m Bool;
	tokenEndParser = do
		{
		mc <- currentC;
		case mc of
			{
			Nothing -> return True;
			Just ')' -> return True;
			Just ';' -> return True;
			Just c -> return (isWhitespace c);
			};
		};

	identifierRestParser :: (Monad m) => TextParser m String;
	identifierRestParser = do
		{
		mc <- currentC;
		case isJust allowedIdentifierR mc of
			{
			(Just c) -> do
				{
				nextC;
				rest <- identifierRestParser;
				return (toLowerCase c:rest);
				};
			Nothing -> do
				{
				end <- tokenEndParser;
				if end then (return []) else fail "junk char in identifier";
				};
			};
		};

	identifierParser :: (Monad m) => TextParser m (Maybe String);
	identifierParser = do
		{
		mc <- currentC;
		case isJust allowedIdentifier1 mc of
			{
			(Just c) -> do
				{
				nextC;
				rest <- identifierRestParser;
				return (Just (toLowerCase c:rest));
				};
			Nothing -> return Nothing;
			};
		};

	integerRestParser :: (Monad m) => Integer -> TextParser m Integer;
	integerRestParser prev = do
		{
		mc <- currentC;
		case mc of
			{
			Nothing -> do
				{
				end <- tokenEndParser;
				if end then (return prev) else fail "junk char in integer";
				};
			Just c -> case (getDecimalDigit c) of
				{
				Nothing -> return prev;
				Just n -> do
					{
					nextC;
					integerRestParser (prev*10 + n);
					};
				};
			};
		};

	integerParser :: (Monad m) => TextParser m (Maybe Integer);
	integerParser = do
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
					i <- integerRestParser n;
					return (Just i);
					};
				};
			};
		};

	listRestParser :: (Scheme x m r) => TextParser m (Object r m);
	listRestParser = do
		{
		whitespaceParser;
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
				mtail <- expressionParser;
				case mtail of
					{
					Nothing -> fail "missing tail in dotted pair";
					Just tail -> do
						{
						whitespaceParser;
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
				mhead <- expressionParser;
				case mhead of
					{
					Nothing -> fail "unterminated list";
					Just head -> do
						{
						tail <- listRestParser;
						mlift (cons head tail);
						};
					};
				};
			};
		};

	listParser :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	listParser = do
		{
		mc <- currentC;
		case mc of
			{
			Just '(' -> do
				{
				nextC;
				list <- listRestParser;
				return (Just list);
				};
			_ -> return Nothing;
			};
		};

	plusMinusRestParser :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe Integer);
	plusMinusRestParser Type = do
		{
		end <- tokenEndParser;
		if end
		 then return Nothing
		 else do
			{
			mi <- integerParser;
			case mi of
				{
				Nothing -> fail "bad '+'/'-' token";
				Just _ -> return mi;
				};
			};
		};

	plusMinusParser :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe (Object r m));
	plusMinusParser t = do
		{
		mc <- currentC;
		case mc of
			{
			Just '+' -> do
				{
				nextC;
				mi <- plusMinusRestParser t;
				case mi of
					{
					Nothing -> return (Just (SymbolObject (MkSymbol "+")));
					Just i -> return (Just (NumberObject (fromInteger i)));
					};
				};
			Just '-' -> do
				{
				nextC;
				mi <- plusMinusRestParser t;
				case mi of
					{
					Nothing -> return (Just (SymbolObject (MkSymbol "-")));
					Just i -> return (Just (NumberObject (fromInteger (- i))));
					};
				};
			_ -> return Nothing;
			};
		};

	specialCharParser :: (Scheme x m r) => Symbol -> TextParser m (Maybe (Object r m));
	specialCharParser symbol = do
		{
		mh2 <- expressionParser;
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

	quotedParser :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	quotedParser = do
		{
		mc <- currentC;
		case mc of
			{
			Just '\'' -> do
				{
				nextC;
				specialCharParser (MkSymbol "quote");
				};
			Just '`' -> do
				{
				nextC;
				specialCharParser (MkSymbol "quasiquote");
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
						specialCharParser (MkSymbol "unquote-splicing");
						};
					_ -> specialCharParser (MkSymbol "unquote");
					};
				};
			_ -> return Nothing;
			};
		};

	hashLiteralParser :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	hashLiteralParser = do
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

	stringLiteralRestParser :: (Scheme x m r) => Type (r ()) -> TextParser m String;
	stringLiteralRestParser t = do
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
				cs <- stringLiteralRestParser t;
				return (c:cs);
				};
			Nothing -> fail "unterminated string";
			};
		};

	stringLiteralParser :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe String);
	stringLiteralParser t = do
		{
		mc <- currentC;
		case mc of
			{
			Just '"' -> do
				{
				nextC;
				mc <- currentC;
				str <- stringLiteralRestParser t;
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

	expressionParser' :: (Scheme x m r) => Type (r ()) -> TextParser m (Maybe (Object r m));
	expressionParser' t = do
		{
		whitespaceParser;
		checkParse integerParser (return . NumberObject . fromInteger)
		 (checkParse identifierParser (return . SymbolObject . MkSymbol)
		  (checkParse hashLiteralParser return
		   (checkParse quotedParser return
		    (checkParse (stringLiteralParser t) (mlift . getConvert . MkStringType)
		     (checkParse (plusMinusParser t) return
		      (checkParse listParser return (do
			{
			mc <- currentC;
			case mc of
				{
				Just c -> fail ("unrecognised char, '"++[c]++"'");
				Nothing -> return Nothing;
				};
			})))))));
		};

	expressionParser :: (Scheme x m r) => TextParser m (Maybe (Object r m));
	expressionParser = expressionParser' Type;
{--
	manyParser :: (Monad m) => Parser c m (Maybe a) -> Parser c m [a];
	manyParser oneP = do
		{
		mfirst <- oneP;
		case mfirst of
			{
			Just first -> do
				{
				rest <- manyParser oneP;
				return (first:rest);
				};
			Nothing -> return [];
			};
		};

	expressionsParser :: (Scheme x m r) => Type (r ()) -> TextParser m [Object r m];
	expressionsParser Type = manyParser expressionParser;
--}
	portRead :: (Scheme x m r) =>
	 InputPort Char m -> m (Maybe (Object r m));
	portRead port = runParser port expressionParser;

	portReadP :: (Scheme x m r) =>
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
