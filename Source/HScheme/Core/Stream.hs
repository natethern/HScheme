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

module Org.Org.Semantic.HScheme.Core.Stream where
	{
	import Org.Org.Semantic.HScheme.Core.Scheme;
	import Org.Org.Semantic.HScheme.Core.Throw;
	import Org.Org.Semantic.HScheme.Core.Object;
	import Org.Org.Semantic.HScheme.Core.Build;
	import Org.Org.Semantic.HBase;

	eofObject :: (Scheme m r) => Object r m;
	eofObject = nullObject;

	handleUTF8Error :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 UTF8Error -> cm a;
	handleUTF8Error err = throwSchemeError "bad-utf8-parse" [show err];

	parseUTF8Char :: (BuildThrow cm (Object r m) r,?objType :: Type (Object r m)) =>
	 cm (Maybe Word8) -> cm (Maybe Char);
	parseUTF8Char source = exRun handleUTF8Error (parseUTF8 source);
	}
