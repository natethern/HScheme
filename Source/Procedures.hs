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

module Procedures where
	{
	import Evaluate;
	import Conversions;
	import Object;
	import Subtype;
	import Type;

	-- 4.1.2 Literal Expressions
	quote :: (Scheme x m r) =>
	 Object r m -> m (Object r m);
	quote = return;

	quoteM :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m (Object r m);
	quoteM Type (q,()) = quote q;
	
	-- 4.1.5 Conditionals
	ifM ::
		(
		Scheme x m r,
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

	-- 6.3.2 Pairs and Lists
	consP :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m (Object r m);
	consP Type (h,(t,())) = cons h t;

	carP :: (Scheme x m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	carP Type ((h,_),()) = return h;

	cdrP :: (Scheme x m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	cdrP Type ((_,t),()) = return t;

--	makeList :: (Scheme x m r) =>
--	 [Object r m] -> m (Object r m);
--	makeList = getConvert;

	listP ::  (Scheme x m r) =>
	 Type (r ()) -> [Object r m] -> m [Object r m];
	listP Type list = return list;

	appendP ::  (Scheme x m r) =>
	 Type (r ()) -> [[Object r m]] -> m [Object r m];
	appendP Type listlist = return (concat listlist);
	
	-- 6.4 Control Features
	valuesP :: (Scheme x m r) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	valuesP Type = return . mkValuesObject;
	
	valuesToListP :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m [Object r m];
	valuesToListP Type (ValuesObject list,()) = return list;
	valuesToListP Type (obj,()) = return [obj];
	
	-- 6.5 Eval
	currentEnvironmentP ::
		(
		Scheme x m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> () -> m (Bindings r m);
	currentEnvironmentP Type () = return ?bindings;

{--	
	applyS :: Procedure m;
	applyS [] = fail "apply needs at least 1 argument";
	applyS (x:xs) = apply x (appconv xs) where
		{
		appconv [] = [];
		appconv ...
		};
--}	
	
	printList :: (Scheme x m r) =>
	 ObjLocation r m -> ObjLocation r m -> m String;
	printList hl tl = do
		{
		head <- getLocation hl;
		htext <- toString head;
		tail <- getLocation tl;
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
	
	printValues :: (Scheme x m r) =>
	 [Object r m] -> m String;
	printValues [] = return "";
	printValues [a] = toString a;
	printValues (a:as) = do
		{
		f <- toString a;
		r <- printValues as;
		return (f++" "++r);
		};
	
	printVector :: (Scheme x m r) =>
	 [ObjLocation r m] -> m String;
	printVector [] = return "";
	printVector [ar] = do
		{
		a <- getLocation ar;
		toString a;
		};
	printVector (ar:as) = do
		{
		a <- getLocation ar;
		f <- toString a;
		r <- printVector as;
		return (f++" "++r);
		};
	
	hexDigit n = hd (mod n 16) where
		{
		hd i | i < 10 = toEnum ((fromEnum '0') + i);
		hd i = toEnum ((fromEnum 'A') + i - 10);
		};

	hexCode4 i =
	 [hexDigit (div i 0x1000)] ++
	 [hexDigit (div i 0x100)] ++
	 [hexDigit (div i 0x10)] ++
	 [hexDigit i];

	charToString :: Char -> String;
	charToString c | (fromEnum c) < 32 = "#\\u"++(hexCode4 (fromEnum c));
	charToString c | (fromEnum c) > 127 = "#\\u"++(hexCode4 (fromEnum c));
	charToString c = "#\\"++[c];
	
	escapeChar :: Char -> String;
	escapeChar '\\' = "\\\\";
	escapeChar '\"' = "\\\"";
	escapeChar c = [c];
	
	printString :: (Scheme x m r) =>
	 [r Char] -> m String;
	printString [] = return "";
	printString (cr:cs) = do
		{
		c <- getLocation cr;
		r <- printString cs;
		return ((escapeChar c)++r);
		};	
	
	toString :: (Scheme x m r) =>
	 Object r m -> m String;
	toString NilObject				= return "()";
	toString (BooleanObject True)	= return "#t";
	toString (BooleanObject False)	= return "#f";
	toString (SymbolObject s)		= return (show s);
	toString (NumberObject n)		= return (show n);
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

	toStringP :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m StringType;
	toStringP Type (o,()) = do
		{
		s <- toString o;
		return (MkStringType s);
		};
	}
