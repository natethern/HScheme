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
	import Object;
	import Subtype;
	import Type;
{--
	makeList :: [Object r m] -> Object r m;
	makeList [] = NilObject;
	makeList (o:os) = PairObject o (makeList os);

	bindArgList :: (Scheme x m r) => Object r m -> m (Object r m) -> Procedure m;
	bindArgList (SymbolObject f) action a = inScope (flist,makeList args) action;
	bindArgList NilObject action [] = action;
	bindArgList NilObject _ (_:_) = fail "";
	bindArgList (PairObject _ _) _ [] = fail "";
	bindArgList (PairObject (SymbolObject f1) fr) action (a1:ar) = inScope (f1,a1) (bindArgList fr ar action);
	bindArgList (PairObject _ _) _ _ = fail "";
	bindArgList _ _ _ = fail "";

	lambda :: (Scheme x m r) => Object r m -> Object r m -> m (Object r m);
	lambda flist expr = return (ProcedureObject (bindArgList flist (evaluate expr)));
--}

	isProcedure :: (Scheme x m r) =>
	 Object r m -> m Bool;
	isProcedure (ProcedureObject _) = return True;
	isProcedure _ = return False;

	isProcedureS :: (Scheme x m r) =>
	 Type (r ()) -> Object r m -> m Bool;
	isProcedureS Type = isProcedure;

	car :: (Scheme x m r) =>
	 Object r m -> m (Object r m);
	car (PairObject a _) = getLocation a;
	car _ = fail "wrong type";
	
	carS :: (Scheme x m r) =>
	 Type (r ()) -> Object r m -> m (Object r m);
	carS Type = car;
	
	cdr :: (Scheme x m r) =>
	 Object r m -> m (Object r m);
	cdr (PairObject _ a) = getLocation a;
	cdr _ = fail "wrong type";
	
	cdrS :: (Scheme x m r) =>
	 Type (r ()) -> Object r m -> m (Object r m);
	cdrS Type = cdr;
	
	cons :: (Scheme x m r) =>
	 (Object r m,Object r m) -> m (Object r m);
	cons (head,tail) = do
		{
		h <- newLocation head;
		t <- newLocation tail;
		return (PairObject h t);
		};
	
	consS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m,Object r m) -> m (Object r m);
	consS Type = cons;
	
	quote :: (Scheme x m r) =>
	 Object r m -> m (Object r m);
	quote = return;
	
	quoteS :: (Scheme x m r) =>
	 Type (r ()) -> (Object r m) -> m (Object r m);
	quoteS Type = quote;
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
		htext <- printS Type head;
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
				rtext <- printS Type tail;
				return (htext++" . "++rtext);
				};
			};
		};
	
	printValues :: (Scheme x m r) =>
	 [Object r m] -> m String;
	printValues [] = return "";
	printValues [a] = printS Type a;
	printValues (a:as) = do
		{
		f <- printS Type a;
		r <- printValues as;
		return (f++" "++r);
		};
	
	printVector :: (Scheme x m r) =>
	 [ObjLocation r m] -> m String;
	printVector [] = return "";
	printVector [ar] = do
		{
		a <- getLocation ar;
		printS Type a;
		};
	printVector (ar:as) = do
		{
		a <- getLocation ar;
		f <- printS Type a;
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
	
	
	printS :: (Scheme x m r) =>
	 Type (r ()) -> Object r m -> m String;
	printS Type NilObject				= return "()";
	printS Type (BooleanObject True)	= return "#t";
	printS Type (BooleanObject False)	= return "#f";
	printS Type (SymbolObject s)		= return s;
	printS Type (NumberObject n)		= return (show n);
	printS Type (CharObject c)			= return ("#\\"++[c]);
	printS Type (StringObject s)		= do
		{
		text <- printString s;
		return ("\""++text++"\"");
		};
	printS Type (ValuesObject [])		= return ("#<nothing>");
	printS Type (ValuesObject v)		= do
		{
		text <- printValues v;
		return ("#<values: "++text++">");
		};
	printS Type (PairObject	hl tl) = do
		{
		list <- printList hl tl;
		return ("("++list++")");
		};
	printS Type (VectorObject v)		= do
		{
		text <- printVector v;
		return ("#("++text++")");
		};
	printS Type (PortObject _)			= return "#<port>";
	printS Type (ProcedureObject _)		= return "#<procedure>";
	printS Type (MacroObject _)			= return "#<macro>";
	printS Type (SyntaxObject _)		= return "#<syntax>";
	printS Type (BindingsObject _)		= return "#<environment>";

{--		
	call-with-current-continuation :: (Scheme x m r) => 
	call-with-current-continuation foo = peirceM (
--}	
	}
