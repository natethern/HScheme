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

module Org.Org.Semantic.HScheme.RunLib.Procedures where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;


	-- 6.3.1 Booleans
	notP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Bool,()) -> m Bool;
	notP (b,()) = return (not b);

	isBooleanP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isBooleanP (BooleanObject _,()) = return True;
	isBooleanP (_,()) = return False;


	-- 6.3.2 Pairs and Lists
	isPairP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isPairP (PairObject _ _,()) = return True;
	isPairP (_,()) = return False;

	consP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,(Object r m,())) -> m (Object r m);
	consP (h,(t,())) = cons h t;

	carP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ((Object r m,Object r m),()) -> m (Object r m);
	carP ((h,_),()) = return h;

	cdrP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ((Object r m,Object r m),()) -> m (Object r m);
	cdrP ((_,t),()) = return t;

	isNilP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isNilP (NilObject,()) = return True;
	isNilP (_,()) = return False;

	isListP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isListP (NilObject,()) = return True;
	isListP (PairObject _ _,()) = return True;
	isListP (_,()) = return False;

	listP ::  (Scheme m r,?objType :: Type (Object r m)) =>
	 [Object r m] -> m [Object r m];
	listP list = return list;

	appendP ::  (Scheme m r,?objType :: Type (Object r m)) =>
	 [[Object r m]] -> m [Object r m];
	appendP listlist = return (concatenateList listlist);


	-- 6.3.3 Symbols
	isSymbolP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isSymbolP (SymbolObject _,()) = return True;
	isSymbolP (_,()) = return False;

	makeSymbolP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SList Char,()) -> m Symbol;
	makeSymbolP (MkSList s,()) = return (MkSymbol s);


	-- 6.3.4 Characters
	charTestP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Char -> Bool) -> (Object r m,()) -> m Bool;
	charTestP f (CharObject c,()) = return (f c);
	charTestP f (_,()) = return False;

	charFuncP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Char -> a) -> (Char,()) -> m a;
	charFuncP f (c,()) = return (f c);

	integerToCharP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Integer,()) -> m Char;
	integerToCharP (i,()) = case nthFromStart i of
		{
		Just c -> return c;
		Nothing -> throwSchemeError "out-of-range-for-char" [i];
		};

	-- 6.3.5 Strings
	isStringP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isStringP (StringObject _,()) = return True;
	isStringP (_,()) = return False;

	makeRefList :: (Scheme m r,?objType :: Type (Object r m)) =>
	 Integer -> a -> m [r a];
	makeRefList 0 _ = return [];
	makeRefList i c = do
		{
		refs <- makeRefList (i - 1) c;
		ref <- new c;
		return (ref:refs);
		};

	makeStringP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Integer,Maybe Char) -> m (SRefList r Char);
	makeStringP (i,Nothing) = do
		{
		rs <- makeRefList i '\x0000';
		return (MkSRefList rs);
		};
	makeStringP (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkSRefList rs);
		};
{--
	makeList :: (Monad m) =>
	 (a -> m b) -> [a] -> m [b];
	makeList f [] = return [];
	makeList f (a:as) = do
		{
		items <- makeList f as;
		item <- f a;
		return (item:items);
		};
--}
	stringP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [Char] -> m (SList Char);
	stringP cs = return (MkSList cs);

	stringLengthP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Char,()) -> m Int;
	stringLengthP (s,()) = return (length s);

	getArrayRef :: (Scheme m r,?objType :: Type (Object r m)) =>
	 Integer -> ArrayList a -> m a;
	getArrayRef i _ | i < 0 = throwSimpleError "out-of-range";
	getArrayRef i arr | i >= convertFromInt (length arr) = throwSimpleError "out-of-range";
	getArrayRef i arr = return (arr !! (convertToInt i));

	stringRefP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Char,(Integer,())) -> m Char;
	stringRefP (arr,(i,())) = do
		{
		r <- getArrayRef i arr;
		get r;
		};

	stringCharsP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SList Char,()) -> m [Char];
	stringCharsP (MkSList cs,()) = return cs;

	-- create a completely new string
	stringAppendP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [SList Char] -> m (SList Char);
	stringAppendP = return . MkSList . concatenateList . (fmap unSList);


	-- Byte Arrays
	isByteArrayP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isByteArrayP (ByteArrayObject _,()) = return True;
	isByteArrayP (_,()) = return False;

	makeByteArrayP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Integer,Maybe Word8) -> m (SRefList r Word8);
	makeByteArrayP (i,Nothing) = do
		{
		rs <- makeRefList i 0;
		return (MkSRefList rs);
		};
	makeByteArrayP (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkSRefList rs);
		};

	byteArrayP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [Word8] -> m (SList Word8);
	byteArrayP cs = return (MkSList cs);

	byteArrayLengthP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Word8,()) -> m Int;
	byteArrayLengthP (s,()) = return (length s);

	byteArrayRefP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r Word8,(Integer,())) -> m Word8;
	byteArrayRefP (arr,(i,())) = do
		{
		r <- getArrayRef i arr;
		get r;
		};

	byteArrayBytesP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SList Word8,()) -> m [Word8];
	byteArrayBytesP (MkSList cs,()) = return cs;

	-- create a completely new byte array
	byteArrayAppendP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [SList Word8] -> m (SList Word8);
	byteArrayAppendP = return . MkSList . concatenateList . (fmap unSList);


	-- Encoding
	accreteEitherList :: [Either a (SList a)] -> [a];
	accreteEitherList [] = [];
	accreteEitherList (Left a:r) = (a:accreteEitherList r);
	accreteEitherList (Right (MkSList al):r) = al ++ (accreteEitherList r);

	encodeP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (String -> [Word8]) -> [Either Char (SList Char)] -> m (SList Word8);
	encodeP enc el = return (MkSList (enc (accreteEitherList el)));

	decodeP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ([Word8] -> ExceptionMonad m x String) ->
	 [Either Word8 (SList Word8)] -> m (Either (SList Char) Bool);
	decodeP dec el = do
		{
		result <- unExceptionMonad (dec (accreteEitherList el));
		case result of
			{
			SuccessExceptionResult text -> return (Left (MkSList text));
			_ -> return (Right False);
			};
		};

	
	-- 6.3.6 Vectors
	isVectorP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m Bool;
	isVectorP (VectorObject _,_) = return True;
	isVectorP (_,_) = return False;

	makeVector :: (Scheme m r,?objType :: Type (Object r m)) =>
	 Int -> Object r m -> m (SList (Object r m));
	makeVector i fill = return (MkSList (makeList i fill)) where
		{
		makeList 0 a = [];
		makeList i a = (a:(makeList (i - 1) a));
		};

	makeVectorP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Int,Maybe (Object r m)) -> m (SList (Object r m));
	makeVectorP (i,Just fill) = makeVector i fill;
	makeVectorP (i,Nothing) = makeVector i nullObject;

	vectorP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ([Object r m],()) -> m (SList (Object r m));
	vectorP (list,_) = return (MkSList list);

	vectorLengthP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r (Object r m),()) -> m Int;
	vectorLengthP (vec,_) = return (length vec);

	vectorRefP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SRefArray r (Object r m),(Int,())) -> m (Object r m);
	vectorRefP (arr,(i,_)) = get (fetchElement i arr);

	vectorToListP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (SList (Object r m),()) -> m [Object r m];
	vectorToListP (MkSList objs,_) = return objs;

	listToVectorP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 ([Object r m],()) -> m (SList (Object r m));
	listToVectorP (list,_) = return (MkSList list);


	-- 6.4 Control Features
	applyP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Procedure (Object r m) m,([Object r m],())) -> m (Object r m);
	applyP (proc,(args,())) = proc args;

	valuesP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 [Object r m] -> m (Object r m);
	valuesP = return . mkValuesObject;
	
	valuesToListP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m [Object r m];
	valuesToListP (ValuesObject list,()) = return list;
	valuesToListP (obj,()) = return [obj];

	lastResortThrowP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m (Object r m);
	lastResortThrowP (obj,()) = lastResortThrowObject obj;

{--
	catchM :: (Scheme m r,MonadException (Object r m) m,?objType :: Type (Object r m)) =>
	 (Procedure (Object r m) m,()) -> m (Object r m);
	catchM = catch;
--}

	-- Misc
	printList :: (Build cm r) =>
	 ObjLocation r m -> ObjLocation r m -> cm String;
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
	
	printValues :: (Build cm r) =>
	 [Object r m] -> cm String;
	printValues [] = return "";
	printValues [a] = toString a;
	printValues (a:as) = do
		{
		f <- toString a;
		r <- printValues as;
		return (f++" "++r);
		};
	
	printVector :: (Build cm r) =>
	 [ObjLocation r m] -> cm String;
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

	toString :: (Build cm r) =>
	 Object r m -> cm String;
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
	toString (EnvironmentObject _)	= return "#<environment>";

	toStringP :: (Scheme m r,?objType :: Type (Object r m)) =>
	 (Object r m,()) -> m (SList Char);
	toStringP (o,()) = do
		{
		s <- toString o;
		return (MkSList s);
		};
	}
