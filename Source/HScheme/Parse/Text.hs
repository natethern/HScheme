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

module Org.Org.Semantic.HScheme.Parse.Text where
	{
	import Org.Org.Semantic.HScheme.Parse.SchemeParser;
	import Org.Org.Semantic.HBase;

	characterConstantParse :: (MonadOrParser Char p) =>
	 p Char;
	characterConstantParse = (do
		{
		isListParse "space";
		return ' ';
		}) ||| (do
		{
		isListParse "newline";
		return '\n';
		}) ||| (do
		{
		isListParse "tab";
		return '\t';
		}) ||| (do
		{
		(isTokenParse 'u' ||| isTokenParse 'U');
		i <- readHexDigits;
		return (nthWrap i);
		}) ||| tokenParse;

	escapedCharInStringParse :: (MonadOrParser Char p) =>
	 p Char;
	escapedCharInStringParse = do
		{
		isTokenParse '\\';
		(do
			{
			isTokenParse 'n';
			return '\n';
			})
		||| (do
			{
			isTokenParse 't';
			return '\t';
			})
		||| (do
			{
			isTokenParse 'u';
			i <- readHexFixedDigits 4;
			return (nthWrap i);
			})
		||| (do
			{
			isTokenParse 'U';
			i <- readHexFixedDigits 6;
			return (nthWrap i);
			})
		||| tokenParse;
		};

	charInStringParse :: (MonadOrParser Char p) =>
	 p Char;
	charInStringParse = escapedCharInStringParse ||| (isntTokenParse '"');

	characterHashLiteralParse ::
		(
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p Char;
	characterHashLiteralParse = do
		{
		isTokenParse '\\';
		mOrUnexpectedCharError "character literal" characterConstantParse;
		};

	stringLiteralParse ::
		(
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p String;
	stringLiteralParse = do
		{
		isTokenParse '"';
		mOrUnexpectedCharError "string literal" (do
			{
			s <- mZeroOrMore charInStringParse;
			isTokenParse '"';
			return s;
			});
		};
	}
