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

module Org.Org.Semantic.HScheme.FullStandardBindings where
	{
	import Org.Org.Semantic.HScheme.Equality;
	import Org.Org.Semantic.HScheme.Macros;
	import Org.Org.Semantic.HScheme.FullProcedures;
	import Org.Org.Semantic.HScheme.StandardBindings;
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HBase;

	fullTopLevelBindings ::
		(
		FullScheme m r,
		BuildThrow cm (Object r m) r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?objType :: Type (Object r m)
		) =>
	 Binds Symbol (TopLevelMacro cm r m) ->
	 Binds Symbol (TopLevelMacro cm r m);
	fullTopLevelBindings = concatenateList
		[
		commonTopLevelBindings,

		-- 5.2 Definitions
		addTopLevelMacroBinding	"define"			fullDefineT
		];

	fullMacroBindings ::
		(
		FullScheme m r,
		BuildThrow cm (Object r m) r,
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?objType :: Type (Object r m)
		) =>
	 Binds Symbol (Macro cm r m) ->
	 Binds Symbol (Macro cm r m);
	fullMacroBindings = concatenateList
		[
		pureMacroBindings,

		-- 4.1.6 Assignments
		addMacroBinding			"set!"				setBangM
		];

	simpleFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		?objType :: Type (Object r m)
		) =>
	 Bindings r m -> cm (Bindings r m);
	simpleFullBindings = concatenateList
		[
		commonPureBindings,

		-- 6.1 Equivalence Predicates
		addProcBinding			"eqv?"				eqvP,
		addProcBinding			"eq?"				eqP,

		-- 6.3.2 Pairs and Lists
		addProcBinding			"set-car!"			setCarP,
		addProcBinding			"set-cdr!"			setCdrP,

		-- 6.3.5 Strings
		addProcBinding			"string-set!"		stringSetP,

		-- Byte Arrays
		addProcBinding			"byte-array-set!"	byteArraySetP
		];

	monadFixFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		MonadFix m,
		?objType :: Type (Object r m)
		) =>
	 Bindings r m -> cm (Bindings r m);
	monadFixFullBindings = simpleFullBindings ++ monadFixBindings;

	-- this one is closest to R5RS
	monadContFullBindings ::
		(
		Build cm r,
		FullScheme m r,
		MonadCont m,
		?objType :: Type (Object r m)
		) =>
	 Bindings r m -> cm (Bindings r m);
	monadContFullBindings = simpleFullBindings ++ monadContBindings;
	}
