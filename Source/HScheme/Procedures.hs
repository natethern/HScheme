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
	 Type (r ()) -> (StringType,()) -> m Symbol;
	makeSymbolP Type (MkStringType s,()) = return (MkSymbol s);

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
	 Type (r ()) -> (Integer,Maybe Char) -> m (StringRefType r);
	makeStringP Type (i,Nothing) = do
		{
		rs <- makeRefList i '\x0000';
		return (MkStringRefType rs);
		};
	makeStringP Type (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkStringRefType rs);
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
	 Type (r ()) -> [Char] -> m (StringRefType r);
	stringP Type cs = do	
		{
		rs <- makeList new cs;
		return (MkStringRefType rs);
		};

	stringLengthP :: (Scheme m r) =>
	 Type (r ()) -> (StringRefType r,()) -> m Integer;
	stringLengthP Type (MkStringRefType rs,()) = return (length rs);

	getListRef :: (Monad m) => Integer -> [a] -> m a;
	getListRef i _ | i < 0 = fail "array out of range";
	getListRef 0 (a:_) = return a;
	getListRef _ [] = fail "array out of range";
	getListRef i (_:as) = getListRef (i - 1) as;

	stringRefP :: (Scheme m r) =>
	 Type (r ()) -> (StringRefType r,(Integer,())) -> m Char;
	stringRefP Type (MkStringRefType rs,(i,())) = do
		{
		r <- getListRef i rs;
		get r;
		};

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
	
	-- 6.5 Eval
	currentEnvironmentP ::
		(
		Scheme m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> () -> m (Bindings r m);
	currentEnvironmentP Type () = return ?bindings;
	
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
	
	hexDigit n = hd (mod n 16) where
		{
		hd i | i < 10 = succNWrap i '0';
		hd i = succNWrap (i-10) 'A';
		};

	hexCode4 i =
	 [hexDigit (div i 0x1000)] ++
	 [hexDigit (div i 0x100)] ++
	 [hexDigit (div i 0x10)] ++
	 [hexDigit i];

	charToString :: Char -> String;
	charToString c | (ordFromStart c) < 32 = "#\\u"++(hexCode4 (ordFromStart c));
	charToString c | (ordFromStart c) > 127 = "#\\u"++(hexCode4 (ordFromStart c));
	charToString c = "#\\"++[c];
	
	escapeChar :: Char -> String;
	escapeChar '\\' = "\\\\";
	escapeChar '\"' = "\\\"";
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
	toString (StringObject s)		= do
		{
		text <- printString s;
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
		text <- printVector v;
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
	 Type (r ()) -> (Object r m,()) -> m StringType;
	toStringP Type (o,()) = do
		{
		s <- toString o;
		return (MkStringType s);
		};
	}
