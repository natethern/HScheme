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

module Org.Org.Semantic.HScheme.Parse.Misc where
	{
	import Org.Org.Semantic.HScheme.Parse.ParseObject;
	import Org.Org.Semantic.HScheme.Parse.WhiteSpace;
	import Org.Org.Semantic.HScheme.Parse.SchemeParser;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;
	
	vectorContentsParse ::
		(
		ParseObject r obj,
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 p [obj];
	vectorContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			head <- expressionParse;
			tail <- vectorContentsParse;
			return (head:tail);
			}) |||
		 (return []);
		};

	byteArrayContentsParse ::
		(
		MonadOrParser Char p
		) =>
	 p [Word8];
	byteArrayContentsParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			head <- readHexFixedDigits 2;
			tail <- byteArrayContentsParse;
			return (head:tail);
			}) |||
		 (return []);
		};

	booleanHashLiteralParse ::
		(
		MonadOrParser Char p
		) =>
	 p Bool;
	booleanHashLiteralParse = (do
		{
		isTokenParse 't';
		return True;
		}) ||| (do
		{
		isTokenParse 'f';
		return False;
		});

	vectorHashLiteralParse ::
		(
		ParseObject r obj,
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p (SRefArray r obj);
	vectorHashLiteralParse = do
		{
		isTokenParse '(';
		mOrUnexpectedCharError "vector" (do
			{
			vector <- vectorContentsParse;
			optionalWhitespaceParse;
			isTokenParse ')';
			parserLift (makeSRefArray vector);
			});
		};

	byteArrayHashLiteralParse ::
		(
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p (SRefArray r Word8);
	byteArrayHashLiteralParse = do
		{
		isTokenParse 'x';
		mOrUnexpectedCharError "byte array" (do
			{
			isTokenParse '(';
			bytes <- byteArrayContentsParse;
			optionalWhitespaceParse;
			isTokenParse ')';
			parserLift (makeSRefArray bytes);
			});
		};
	}
