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

module Org.Org.Semantic.HScheme.Parse.SchemeParser where
	{
	import Org.Org.Semantic.HScheme.Parse.ParserError;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	class
		(
		ObjectSubtype r obj Symbol
		) =>
	 Parsable r obj | obj -> r;

	instance
		(
		ObjectSubtype r obj Symbol
		) =>
	 Parsable r obj;

	class
		(
		Build cm r,
		ObjectSubtype r obj obj,
		ParserError cm obj,
		MonadOrParser Char p,
		LiftedParser Char cm p
		) =>
	 SchemeParser cm obj r p | p -> cm, obj -> r;

	instance
		(
		Build cm r,
		ObjectSubtype r obj obj,
		ParserError cm obj,
		MonadOrParser Char p,
		LiftedParser Char cm p
		) =>
	 SchemeParser cm obj r p;

	runParser ::
		(
		ParserError cm obj,
		Build cm r,
		ObjectSubtype r obj Symbol,
		?objType :: Type obj
		) =>
	 cm (Maybe t) -> OrStreamParser cm t a -> cm a;
	runParser source parser = do
		{
		mr <- runOrStreamParser source parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwParseFailed;
			};
		};

	runParserString ::
		(
		ParserError cm obj,
		Build cm r,
		ObjectSubtype r obj Symbol,
		?objType :: Type obj
		) =>
	 [t] -> OrListParser cm t a -> cm ([t],a);
	runParserString text parser = do
		{
		mr <- doOrListParser text parser;
		case mr of
			{
			Just r -> return r;
			Nothing -> throwParseFailed;
			};
		};

	runParserOnlyString :: (Monad cm) =>
	 [t] -> OrListParser cm t a -> cm (Maybe a);
	runParserOnlyString text parser = runOrListParser text (do
		{
		a <- parser;
		streamEndParse;
		return a;
		});

	unexpectedCharError :: (SchemeParser cm obj r p,?objType :: Type obj) =>
	 String -> p a;
	unexpectedCharError context = (do
		{
		t <- tokenParse;
		parserLift (throwInappropriateCharacter context t);
		}) |||
	 (parserLift (throwInappropriateStreamEnd context));

	mOrUnexpectedCharError :: (SchemeParser cm obj r p,?objType :: Type obj) =>
	 String -> p a -> p a;
	mOrUnexpectedCharError s ma = ma ||| (unexpectedCharError s);

	prefixParse :: (MonadOrParser Char p) =>
	 p Char;
	prefixParse = do
		{
		isTokenParse '#';
		tokenParse;
		};
	}
