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

module Org.Org.Semantic.HScheme.Parse.List where
	{
	import Org.Org.Semantic.HScheme.Parse.WhiteSpace;
	import Org.Org.Semantic.HScheme.Parse.SchemeParser;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;



	-- Parsers

	listContentsParse ::
		(
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p obj -> p obj;
	listContentsParse expressionParse = do
		{
		optionalWhitespaceParse;
		 (do
			{
			isTokenParse '.';
			whitespaceParse;
			mOrUnexpectedCharError "dotted pair tail"  expressionParse; 
			}) |||
		 (do
			{
			head <- expressionParse;
			tail <- listContentsParse expressionParse;
			parserLift (cons head tail);
			}) |||
		 (return nilObject);
		};
	
	listParse ::
		(
		?objType :: Type obj,
		SchemeParser cm obj r p
		) =>
	 p obj -> p obj;
	listParse expressionParse = do
		{
		isTokenParse '(';
		list <- listContentsParse expressionParse;
		optionalWhitespaceParse;
		mOrUnexpectedCharError "list end" (isTokenParse ')');
		return list;
		};
	}
