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

module Org.Org.Semantic.HScheme.Procedures where
	{
	import Org.Org.Semantic.HScheme.Evaluate;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Numerics;
	import Org.Org.Semantic.HBase;


	-- 4.1.2 Literal Expressions
	quote :: (Scheme m r) =>
	 Object r m -> m (Object r m);
	quote = return;

	quoteM :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m (Object r m);
	quoteM Type (q,()) = quote q;


	-- 4.1.5 Conditionals
	ifM ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> (Object r m,(Object r m,Maybe (Object r m))) -> m (Object r m);
	ifM Type (cond,(thenClause,mElseClause)) = do
		{
		isObj <- evaluate cond;
		is <- getConvert isObj;
		if is
		 then evaluate thenClause
		 else case mElseClause of
			{
			Nothing -> return nullObject;
			Just elseClause -> evaluate elseClause;
			};
		};


	-- 6.3.1 Booleans
	notP :: (Scheme m r) =>
	 Type (r ()) -> (Bool,()) -> m Bool;
	notP Type (b,()) = return (not b);

	isBooleanP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isBooleanP Type (BooleanObject _,()) = return True;
	isBooleanP Type (_,()) = return False;


	-- 6.3.2 Pairs and Lists
	isPairP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isPairP Type (PairObject _ _,()) = return True;
	isPairP Type (_,()) = return False;

	consP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m (Object r m);
	consP Type (h,(t,())) = cons h t;

	carP :: (Scheme m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	carP Type ((h,_),()) = return h;

	cdrP :: (Scheme m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	cdrP Type ((_,t),()) = return t;

	isNilP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isNilP Type (NilObject,()) = return True;
	isNilP Type (_,()) = return False;

	listP ::  (Scheme m r) =>
	 Type (r ()) -> [Object r m] -> m [Object r m];
	listP Type list = return list;

	appendP ::  (Scheme m r) =>
	 Type (r ()) -> [[Object r m]] -> m [Object r m];
	appendP Type listlist = return (concatenateList listlist);


	-- 6.3.3 Symbols
	isSymbolP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isSymbolP Type (SymbolObject _,()) = return True;
	isSymbolP Type (_,()) = return False;

	makeSymbolP :: (Scheme m r) =>
	 Type (r ()) -> (SList Char,()) -> m Symbol;
	makeSymbolP Type (MkSList s,()) = return (MkSymbol s);


	-- 6.3.4 Characters
	charTestP :: (Scheme m r) =>
	 (Char -> Bool) -> Type (r ()) -> (Object r m,()) -> m Bool;
	charTestP f Type (CharObject c,()) = return (f c);
	charTestP f Type (_,()) = return False;

	charFuncP :: (Scheme m r) =>
	 (Char -> a) -> Type (r ()) -> (Char,()) -> m a;
	charFuncP f Type (c,()) = return (f c);


	-- 6.3.5 Strings
	isStringP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m Bool;
	isStringP Type (StringObject _,()) = return True;
	isStringP Type (_,()) = return False;

	makeRefList :: (Scheme m r) =>
	 Integer -> a -> m [r a];
	makeRefList 0 _ = return [];
	makeRefList i c = do
		{
		rest <- makeRefList (i - 1) c;
		first <- new c;
		return (first:rest);
		};

	makeStringP :: (Scheme m r) =>
	 Type (r ()) -> (Integer,Maybe Char) -> m (SRefList r Char);
	makeStringP Type (i,Nothing) = do
		{
		rs <- makeRefList i '\x0000';
		return (MkSRefList rs);
		};
	makeStringP Type (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkSRefList rs);
		};

	makeList :: (Monad m) =>
	 (a -> m b) -> [a] -> m [b];
	makeList f [] = return [];
	makeList f (a:as) = do
		{
		rest <- makeList f as;
		first <- f a;
		return (first:rest);
		};

	stringP :: (Scheme m r) =>
	 Type (r ()) -> [Char] -> m (SList Char);
	stringP Type cs = return (MkSList cs);

	stringLengthP :: (Scheme m r) =>
	 Type (r ()) -> (SRefArray r Char,()) -> m Int;
	stringLengthP Type (s,()) = return (length s);

	getArrayRef :: (Monad m) => Integer -> ArrayList a -> m a;
	getArrayRef i _ | i < 0 = fail "array out of range";
	getArrayRef i arr | i >= convertFromInt (length arr) = fail "array out of range";
	getArrayRef i arr = return (arr !! (convertToInt i));

	stringRefP :: (Scheme m r) =>
	 Type (r ()) -> (SRefArray r Char,(Integer,())) -> m Char;
	stringRefP Type (arr,(i,())) = do
		{
		r <- getArrayRef i arr;
		get r;
		};

	-- create a completely new string
	stringAppendP :: (Scheme m r) =>
	 Type (r ()) -> [SList Char] -> m (SList Char);
	stringAppendP Type = return . MkSList . concatenateList . (fmap unSList);


	-- 6.4 Control Features
	applyP :: (Scheme m r,?bindings :: Bindings r m) =>
	 Type (r ()) -> (Procedure r m,([Object r m],())) -> m (Object r m);
	applyP Type (proc,(args,())) = proc ?bindings args;
	
	valuesP :: (Scheme m r) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	valuesP Type = return . mkValuesObject;
	
	valuesToListP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m [Object r m];
	valuesToListP Type (ValuesObject list,()) = return list;
	valuesToListP Type (obj,()) = return [obj];


	-- Misc
	printList :: (Scheme m r) =>
	 ObjLocation r m -> ObjLocation r m -> m String;
	printList hl tl = do
		{
		head <- get hl;
		htext <- toString head;
		tail <- get tl;
		case tail of
			{
			NilObject -> return htext;
			(PairObject hl' tl') -> do
				{
				rtext <- printList hl' tl';
				return (htext++" "++rtext);
				};
			_ -> do
				{
				rtext <- toString tail;
				return (htext++" . "++rtext);
				};
			};
		};
	
	printValues :: (Scheme m r) =>
	 [Object r m] -> m String;
	printValues [] = return "";
	printValues [a] = toString a;
	printValues (a:as) = do
		{
		f <- toString a;
		r <- printValues as;
		return (f++" "++r);
		};
	
	printVector :: (Scheme m r) =>
	 [ObjLocation r m] -> m String;
	printVector [] = return "";
	printVector [ar] = do
		{
		a <- get ar;
		toString a;
		};
	printVector (ar:as) = do
		{
		a <- get ar;
		f <- toString a;
		r <- printVector as;
		return (f++" "++r);
		};

	printByteArrayContents :: (Scheme m r) =>
	 [r Word8] -> m String;
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

	printString :: (Scheme m r) =>
	 [r Char] -> m String;
	printString [] = return "";
	printString (cr:cs) = do
		{
		c <- get cr;
		r <- printString cs;
		return ((escapeChar c)++r);
		};	

	toString :: (Scheme m r) =>
	 Object r m -> m String;
	toString NilObject				= return "()";
	toString (BooleanObject True)	= return "#t";
	toString (BooleanObject False)	= return "#f";
	toString (SymbolObject s)		= return (show s);
	toString (NumberObject n)		= return (showNumber n);
	toString (CharObject c)			= return (charToString c);
	toString (ByteArrayObject arr)	= do
		{
		text <- printByteArrayContents (toList arr);
		return ("#x("++text++")");
		};
	toString (StringObject arr)		= do
		{
		text <- printString (toList arr);
		return ("\""++text++"\"");
		};
	toString (ValuesObject [])		= return ("#<nothing>");
	toString (ValuesObject v)		= do
		{
		text <- printValues v;
		return ("#<values: "++text++">");
		};
	toString (PairObject	hl tl)	= do
		{
		list <- printList hl tl;
		return ("("++list++")");
		};
	toString (VectorObject v)		= do
		{
		text <- printVector (toList v);
		return ("#("++text++")");
		};
	toString (InputPortObject _)	= return "#<input port>";
	toString (OutputPortObject _)	= return "#<output port>";
	toString (ProcedureObject _)	= return "#<procedure>";
	toString (MacroObject _)		= return "#<macro>";
	toString (TopLevelMacroObject _)= return "#<top-level macro>";
	toString (SyntaxObject _)		= return "#<syntax>";
	toString (BindingsObject _)		= return "#<environment>";

	toStringP :: (Scheme m r) =>
	 Type (r ()) -> (Object r m,()) -> m (SList Char);
	toStringP Type (o,()) = do
		{
		s <- toString o;
		return (MkSList s);
		};
	}
