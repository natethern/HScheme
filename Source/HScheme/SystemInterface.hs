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

module Org.Org.Semantic.HScheme.SystemInterface where
	{
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.TopLevel;
	import Org.Org.Semantic.HScheme.PortProcedures;
	import Org.Org.Semantic.HScheme.Procedures;
	import Org.Org.Semantic.HScheme.SExpParser;
	import Org.Org.Semantic.HScheme.Compile;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	printResult :: (Scheme m r) =>
	 OutputPort Word8 m -> Object r m -> m ();
	printResult output obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteList output (encodeUTF8 (str ++ "\n"));
		};

	parseFromPortBulk ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 InputPort Word8 cm ->
	 cm [Object r m];
	parseFromPortBulk input = do	
		{
		text <- accumulateMaybeSource (parseUTF8Char (ipRead input));
		parseAllFromString text;
		};

	readWithProcs ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 (String -> cm (InputPort Word8 cm)) ->
	 String -> cm [Object r m];
	readWithProcs oif name = do
		{
		input <- oif name;
		objects <- parseFromPortBulk input;
		ipClose input;
		return objects;
		};

	readFiles ::
		(
		BuildThrow cm (Object r m) r,
		?objType :: Type (Object r m)
		) =>
	 (String -> cm (InputPort Word8 cm)) ->
	 [String] -> cm [Object r m];
	readFiles oif [] = return [];
	readFiles oif (name:names) = do
		{
		objs1 <- readWithProcs oif name;
		objsr <- readFiles oif names;
		return (objs1 ++ objsr);
		};

	data PureSystemInterface cm m r = MkPureSystemInterface
		{
		psiReadFile	:: String -> cm [Object r m]
		};

	data FullSystemInterface cm m r = MkFullSystemInterface
		{
		fsiPure					:: PureSystemInterface cm m r,
		fsiCurrentInputPort		:: InputPort Word8 cm,
		fsiCurrentOutputPort	:: OutputPort Word8 cm,
		fsiCurrentErrorPort		:: OutputPort Word8 cm,
		fsiOpenInputFile		:: String -> cm (InputPort Word8 cm),
		fsiOpenOutputFile		:: String -> cm (OutputPort Word8 cm)
		};

	loadT ::
		(
		Build cm r,
		Scheme m r,
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m)
		) =>
	 PureSystemInterface cm m r ->
	 (SList Char,()) -> cm (TopLevelExpression cm r m);
	loadT psi (MkSList filename,()) = do
		{
		readObjects <- psiReadFile psi filename;
		beginM readObjects;
		};

	currentInputPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (InputPort Word8 cm);
	currentInputPortP fsi () = return (fsiCurrentInputPort fsi);

	currentOutputPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (OutputPort Word8 cm);
	currentOutputPortP fsi () = return (fsiCurrentOutputPort fsi);

	currentErrorPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (OutputPort Word8 cm);
	currentErrorPortP fsi () = return (fsiCurrentErrorPort fsi);

	openInputFileP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 (SList Char,()) -> cm (InputPort Word8 cm);
	openInputFileP fsi (MkSList name,()) = fsiOpenInputFile fsi name;

	openOutputFileP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 (SList Char,()) -> cm (OutputPort Word8 cm);
	openOutputFileP fsi (MkSList name,()) = fsiOpenOutputFile fsi name;

	systemMacroBindings ::
		(
		BuildThrow cm (Object r m) r,
		Scheme m r,
		?objType :: Type (Object r m),
		?macrobindings :: Binds Symbol (Macro cm r m),
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?toplevelbindings :: Binds Symbol (TopLevelMacro cm r m)
		) =>
	 PureSystemInterface cm m r ->
	 Binds Symbol (TopLevelMacro cm r m) ->
	 Binds Symbol (TopLevelMacro cm r m);
	systemMacroBindings psi = concatenateList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT psi)
		];

	fullSystemBindings ::
		(
		Build cm r,
		Scheme m r,
		?objType :: Type (Object r m)
		) =>
	 FullSystemInterface m m r -> Bindings r m -> cm (Bindings r m);
	fullSystemBindings fsi = concatenateList
		[
		-- 6.6.1 Ports
		addProcBinding	"current-input-port"	(currentInputPortP fsi),
		addProcBinding	"current-output-port"	(currentOutputPortP fsi),
		addProcBinding	"current-error-port"	(currentErrorPortP fsi), -- nonstandard
		addProcBinding	"open-input-file"		(openInputFileP fsi),
		addProcBinding	"open-output-file"		(openOutputFileP fsi)
		];
	}
