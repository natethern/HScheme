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

module Org.Org.Semantic.HScheme.Imperative.IOLoad where
	{
	import Org.Org.Semantic.HScheme.Imperative.IOSystem;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	nameInPath :: String -> String -> String;
	nameInPath path name@('/':_) = name;
	nameInPath path name = path ++ "/" ++ name;

	openFileReadWithPaths :: [String] -> String -> IO Handle;
	openFileReadWithPaths [] name = fail ("file "++(show name)++" not found in paths");
	openFileReadWithPaths (path:paths) name = catchSingle
		(openFileRead (nameInPath path name))
		(\_ -> openFileReadWithPaths paths name);

	openInputFileWithPaths ::
	 [String] -> String -> IO (InputPort Word8 IO);
	openInputFileWithPaths paths name = do
		{
		h <- openFileReadWithPaths paths name;
		return (handleInputPort h);
		};

	ioLoad ::
		(
		Scheme m r,
		BuildThrow IO (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 [String] -> 
	 String -> IO [Object r m];
	ioLoad loadpaths = readWithProcs (openInputFileWithPaths loadpaths);
	}
