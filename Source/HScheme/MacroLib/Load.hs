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

module Org.Org.Semantic.HScheme.MacroLib.Load where
	{
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	loadT ::
		(
		Build cm r,
		Scheme m r,
		?load :: String -> cm [Object r m],
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m)
		) =>
	 (SList Char,()) -> cm (TopLevelObjectCommand cm r m);
	loadT (MkSList filename,()) = do
		{
		readObjects <- ?load filename;
		beginM readObjects;
		};
	}
