-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>

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

module Org.Org.Semantic.HScheme.Parse.ParseObject where
	{
	import Org.Org.Semantic.HScheme.Parse.Numeric;
	import Org.Org.Semantic.HScheme.Parse.WhiteSpace;
	import Org.Org.Semantic.HScheme.Parse.SchemeParser;
	import Org.Org.Semantic.HScheme.Parse.ParserError;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class (ObjectSubtype r obj Symbol,ObjectSubtype r obj obj,ObjectSubtype r obj (SList Char)) =>
	 ParseObject r obj | obj -> r where
		{
		expressionParse :: forall cm p.
			(
			?objType :: Type obj,
			SchemeParser cm obj r p
			) => p obj;
		};

	expressionOrEndParse :: (ParseObject r obj,SchemeParser cm obj r p) =>
	 p (Maybe obj);
	expressionOrEndParse = let {?objType=MkType} in 
		(expressionParse >>= (return . Just)) |||
		(optionalWhitespaceParse >> streamEndParse >> (return Nothing)) |||
		(unexpectedCharError "input");

	expressionsParse :: (ParseObject r obj,SchemeParser cm obj r p) =>
	 p [obj];
	expressionsParse = accumulateMaybeSource expressionOrEndParse;

	parseFromCharSource ::
		(
		ParseObject r obj,
		Build m r,
		ParserError m obj,
		?objType :: Type obj
		) =>
	 m (Maybe Char) -> m (Maybe obj);
	parseFromCharSource source = runParser source expressionOrEndParse;

	parseFromPort ::
		(
		ParseObject r obj,
		Build m r,
		ParserError m obj,
		?objType :: Type obj
		) =>
	 InputPort Word8 m -> m (Maybe obj);
	parseFromPort port = parseFromCharSource (parseUTF8Char (ipRead port));

	parseFromString ::
		(
		ParseObject r obj,
		Build m r,
		ParserError m obj,
		?objType :: Type obj
		) =>
	 String -> m (String,Maybe obj);
	parseFromString text = runParserString text expressionOrEndParse;
{-
	parseNumberOnlyFromString :: (Build m r,?objType :: Type obj) =>
	 String -> m (Maybe Number);
	parseNumberOnlyFromString text = runParserOnlyString text numberParse;
-}
	stringToNumberP ::
		(
		ParseObject r obj,
		Build m r,
		ParserError m obj,
		?objType :: Type obj
		) =>
	 (SList Char,Maybe Word8) -> m (Either Number Bool);
	stringToNumberP (MkSList s,mradix) = do
		{
		mn <- runParserOnlyString s (radixNumberParse (unJust 10 mradix));
		return (case mn of
			{
			Just n -> Left n;
			Nothing -> Right False;
			});
		};

	parseAllFromString ::
		(
		ParseObject r obj,
		Build cm r,
		ParserError cm obj,
		?objType :: Type obj
		) =>
	 String -> cm [obj];
	parseAllFromString text = do	
		{
		(_,objs) <- runParserString text expressionsParse;
		return objs;
		};

	parseRestOfLine ::
		(
		ObjectSubtype r obj Symbol,
		Build cm r,
		ParserError cm obj,
		?objType :: Type obj
		) =>
	 cm (Maybe Char) -> cm ();
	parseRestOfLine text = do	
		{
		runParser text restOfLineParse;
		return ();
		};

	parseUTF8Char ::
		(
		ParserError cm obj,
		?objType :: Type obj
		) =>
	 cm (Maybe Word8) -> cm (Maybe Char);
	parseUTF8Char source = exRun throwUTF8Error (parseUTF8 source);

	parseFromPortBulk ::
		(
		ParseObject r obj,
		Build cm r,
		ParserError cm obj,
		?objType :: Type obj
		) =>
	 InputPort Word8 cm ->
	 cm [obj];
	parseFromPortBulk input = do	
		{
		text <- accumulateMaybeSource (parseUTF8Char (ipRead input));
		parseAllFromString text;
		};

	readWithProcs ::
		(
		ParseObject r obj,
		Build cm r,
		ParserError cm obj,
		?objType :: Type obj
		) =>
	 (String -> cm (InputPort Word8 cm)) ->
	 String -> cm [obj];
	readWithProcs oif name = do
		{
		input <- oif name;
		objects <- parseFromPortBulk input;
		ipClose input;
		return objects;
		};

	portReadP ::
		(
		ObjectSubtype r obj VoidObjType,
		ParseObject r obj,
		Build m r,
		ParserError m obj,
		?objType :: Type obj
		) =>
	 (InputPort Word8 m,()) -> m obj;
	portReadP (port,()) = do
		{
		mobj <- parseFromPort port;
		case mobj of
			{
			Just obj -> return obj;
			Nothing -> getObject MkVoidObjType;
			};
		};

	-- conversion
	parseUTF8P ::
		(
		ApplyObject m r obj,
		ParserError m obj,
		ObjectSubtype r obj VoidObjType,
		ObjectSubtype r obj Word8,
		?objType :: Type obj
		) =>
	 (Procedure obj m,()) -> m (Either VoidObjType Char);
	parseUTF8P (source,()) = do
		{
		mc <- parseUTF8Char (do
			{
			objs <- source [];
			obj <- singleValue objs;
			eb <- fromObject obj;
			case eb of
				{
				Right b -> return (Just (b :: Word8));
				Left MkVoidObjType -> return Nothing;
				};
			});
		return (case mc of
			{
			Just c -> Right c;
			Nothing -> Left MkVoidObjType;
			});
		};
	}
