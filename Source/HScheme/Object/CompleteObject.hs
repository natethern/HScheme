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

-- this arrangement to work around an instances bug in GHC

module Org.Org.Semantic.HScheme.Object.CompleteObject
	(
	module Org.Org.Semantic.HScheme.Object.CompleteObject1,
	module Org.Org.Semantic.HScheme.Object.CompleteObject
	)where
	{
	import Org.Org.Semantic.HScheme.Object.CompleteObject1;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	instance (Build cm r,MonadThrow (CompleteObject r m) cm) =>
	 ParserError cm (CompleteObject r m) where
		{
		throwParseFailed = throwSimpleError "no-parse";
		throwInappropriateCharacter context c = do
			{
			contextObj <- getObject (MkSList context);
			cObj <- getObject c;
			throwArgError "unexpected-stream-end" [contextObj,cObj];
			};
		throwInappropriateStreamEnd context = do
			{
			contextObj <- getObject (MkSList context);
			throwArgError "unexpected-stream-end" [contextObj];
			};
		throwUTF8Error err = do
			{
			errObj <- getObject (MkSList (show err));
			throwArgError "bad-utf8-parse" [errObj];
			};
		};
	}
