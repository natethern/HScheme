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

module Org.Org.Semantic.HScheme.Parse.WhiteSpace where
	{
	import Org.Org.Semantic.HBase.Text.Properties.Misc;
	import Org.Org.Semantic.HBase;

	restOfLineParse :: (MonadOrParser Char p) =>
	 p ();
	restOfLineParse = do
		{
		mZeroOrMore (matchTokenParse (not . isLineBreak));
		((matchTokenParse isLineBreak) >> (return ())) ||| (return ());
		};

	commentParse :: (MonadOrParser Char p) =>
	 p ();
	commentParse = do
		{
		isTokenParse ';';
		restOfLineParse;
		};

	optionalWhitespaceParse :: (MonadOrParser Char p) =>
	 p ();
	optionalWhitespaceParse = do
		{
		mZeroOrMore (((matchTokenParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};

	whitespaceParse :: (MonadOrParser Char p) =>
	 p ();
	whitespaceParse = do
		{
		mOneOrMore (((matchTokenParse isWhiteSpace) >> (return ())) ||| commentParse);
		return ();
		};
	}
