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

	makeList :: (Scheme x m r) =>
	 [Object r m] -> m (Object r m);
	makeList = getConvert;

	listS ::  (Scheme x m r) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	listS Type = makeList;

	carS :: (Scheme x m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	carS Type ((h,_),()) = return h;

	cdrS :: (Scheme x m r) =>
	 Type (r ()) -> ((Object r m,Object r m),()) -> m (Object r m);
	cdrS Type ((_,t),()) = return t;

	consS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,(Object r m,())) -> m (Object r m);
	consS Type (h,(t,())) = cons h t;

	quote :: (Scheme x m r) =>
	 Object r m -> m (Object r m);
	quote = return;

	quoteS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m (Object r m);
	quoteS Type (q,()) = quote q;
{--	
	applyS :: Procedure m;
	applyS [] = fail "apply needs at least 1 argument";
	applyS (x:xs) = apply x (appconv xs) where
		{
		appconv [] = [];
		appconv ...
		};
--}	
	
	ifS ::
		(
		Scheme x m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> (Object r m,(Object r m,Maybe (Object r m))) -> m (Object r m);
	ifS Type (cond,(thenClause,mElseClause)) = do
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
	
	valuesS :: (Scheme x m r) =>
	 Type (r ()) -> [Object r m] -> m (Object r m);
	valuesS Type = return . mkValuesObject;
	
	currentEnvironmentS ::
		(
		Scheme x m r,
		?bindings :: Bindings r m
		) =>
	 Type (r ()) -> () -> m (Bindings r m);
	currentEnvironmentS Type () = return ?bindings;
	
	
	toString :: (Scheme x m r) =>
	 Object r m -> m String;
	toString NilObject				= return "()";
	toString (BooleanObject True)	= return "#t";
	toString (BooleanObject False)	= return "#f";
	toString (SymbolObject s)		= return (show s);
	toString (NumberObject n)		= return (show n);
	toString (CharObject c)			= return ("#\\"++[c]);
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
	toString (SyntaxObject _)		= return "#<syntax>";
	toString (BindingsObject _)		= return "#<environment>";

	toStringS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,()) -> m StringType;
	toStringS Type (o,()) = do
		{
		s <- toString o;
		return (MkStringType s);
		};
	}
