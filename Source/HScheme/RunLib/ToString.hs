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

module Org.Org.Semantic.HScheme.RunLib.ToString where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	printList :: (Build cm r,ListObject r obj) =>
	 (obj -> cm String) -> r obj -> r obj -> cm String;
	printList toStr hl tl = do
		{
		head <- get hl;
		htext <- toStr head;
		tail <- get tl;
		case objectCell tail of
			{
			Just Nothing -> return htext;
			Just (Just (hl',tl')) -> do
				{
				rtext <- printList toStr hl' tl';
				return (htext++" "++rtext);
				};
			Nothing -> do
				{
				rtext <- toStr tail;
				return (htext++" . "++rtext);
				};
			};
		};
	
	printValues :: (Monad cm) =>
	 (obj -> cm String) -> [obj] -> cm String;
	printValues _ [] = return "";
	printValues toStr [a] = toStr a;
	printValues toStr (a:as) = do
		{
		f <- toStr a;
		r <- printValues toStr as;
		return (f++" "++r);
		};
	
	printVector :: (Build cm r) =>
	 (obj -> cm String) -> [r obj] -> cm String;
	printVector _ [] = return "";
	printVector toStr [ar] = do
		{
		a <- get ar;
		toStr a;
		};
	printVector toStr (ar:as) = do
		{
		a <- get ar;
		f <- toStr a;
		r <- printVector toStr as;
		return (f++" "++r);
		};

	printByteArrayContents :: (Build cm r) =>
	 [r Word8] -> cm String;
	printByteArrayContents [] = return "";
	printByteArrayContents [cr] = do
		{
		b <- get cr;
		return (showFixedHex True 2 (convert b));
		};
	printByteArrayContents (cr:cs) = do
		{
		b <- get cr;
		r <- printByteArrayContents cs;
		return ((showFixedHex True 2 (convert b))++" "++r);
		};

	charToString :: Char -> String;
	charToString ' ' = "#\\space";
	charToString '\t' = "#\\tab";
	charToString '\n' = "#\\newline";
	charToString c | (ordFromStart c) >= 0x10000 = "#\\U"++(showFixedHex True 6 (ordFromStart c));
	charToString c | (ordFromStart c) > 126 = "#\\u"++(showFixedHex True 4 (ordFromStart c));
	charToString c | (ordFromStart c) < 32 = "#\\u"++(showFixedHex True 4 (ordFromStart c));
	charToString c = "#\\"++[c];

	escapeChar :: Char -> String;
	escapeChar '\\' = "\\\\";
	escapeChar '"' = "\\\"";
	escapeChar '\n' = "\\n";
	escapeChar '\t' = "\\t";
	escapeChar c | (ordFromStart c) >= 0x10000 = "\\U"++(showFixedHex True 6 (ordFromStart c));
	escapeChar c | (ordFromStart c) > 126 = "\\u"++(showFixedHex True 4 (ordFromStart c));
	escapeChar c | (ordFromStart c) < 32 = "\\u"++(showFixedHex True 4 (ordFromStart c));
	escapeChar c = [c];

	schemeQuote :: String -> String;
	schemeQuote s = "\"" ++ (schemeQuote' s) ++ "\"" where
		{
		schemeQuote' [] = [];
		schemeQuote' (c:cs) = (escapeChar c) ++ (schemeQuote' cs)
		};

	printString :: (Build cm r) =>
	 [r Char] -> cm String;
	printString [] = return "";
	printString (cr:cs) = do
		{
		c <- get cr;
		r <- printString cs;
		return ((escapeChar c)++r);
		};	

	class (Monad cm) =>
	 ToString cm obj where
		{
		toString :: obj -> cm String;	-- quote chars and strings
		toDisplay :: obj -> cm String;	-- don't quote chars or strings
		};

	toStringP :: (ToString cm obj,?objType :: Type obj) =>
	 (obj,()) -> cm (SList Char);
	toStringP (obj,()) = fmap MkSList (toString obj);

	toDisplayP :: (ToString cm obj,?objType :: Type obj) =>
	 (obj,()) -> cm (SList Char);
	toDisplayP (obj,()) = fmap MkSList (toDisplay obj);
	}
