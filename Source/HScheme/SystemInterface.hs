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

	data PureSystemInterface m r = MkPureSystemInterface
		{
		psiReadFile	:: String -> m [Object r m]
		};

	data FullSystemInterface m r = MkFullSystemInterface
		{
		fsiPure					:: PureSystemInterface m r,
		fsiCurrentInputPort		:: InputPort Word8 m,
		fsiCurrentOutputPort	:: OutputPort Word8 m,
		fsiCurrentErrorPort		:: OutputPort Word8 m,
		fsiOpenInputFile		:: String -> m (InputPort Word8 m),
		fsiOpenOutputFile		:: String -> m (OutputPort Word8 m)
		};
{--
	doEval :: (Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 Object r m ->
	 m (Bindings r m);
	doEval foo bindings obj = do
		{
		(bindings',result) <- topLevelEvaluate bindings obj;
		let {?bindings = bindings'} in foo result;
		return bindings';	
		};
--}
	printResult :: (Scheme m r) =>
	 OutputPort Word8 m -> Object r m -> m ();
	printResult output obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteList output (encodeUTF8 (str ++ "\n"));
		};
{--
	printeval :: (Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 ((?bindings :: Bindings r m) => OutputPort Word8 m) ->
	 Bindings r m ->
	 Object r m ->
	 m (Bindings r m);
	printeval output bindings obj = doEval (printResult output) bindings obj;
--}
{--
	parseEvalFromString ::
		(
		Scheme m r,?refType :: Type (r ()),
		?bindings		:: Bindings r m,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)
		) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 String -> m ();
	parseEvalFromString foo text = do	
		{
		objs <- parseAllFromString text;
		evalObjects foo objs;
		};

	parseEvalFromPortByLine :: (Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m),?refType :: Type (r ())) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 InputPort Word8 m ->
	 m (Bindings r m);
	parseEvalFromPortByLine foo bindings input = do	
		{
		mobject <- let {?bindings=bindings} in parseFromPort input;
		case mobject of
			{
			Nothing -> return bindings;
			Just obj -> do
				{
				bindings' <- doEval foo bindings obj;
				parseEvalFromPortByLine foo bindings' input;
				};
			};
		};

	parseEvalFromPortBulk :: (Scheme m r,?refType :: Type (r ()),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 InputPort Word8 m ->
	 m (Bindings r m);
	parseEvalFromPortBulk foo bindings input = do	
		{
		text <- accumulateSource (let {?bindings=bindings} in parseUTF8Char (ipRead input));
		parseEvalFromString foo bindings text;
		};

	loadBindingsWithProcs :: (Scheme m r,?refType :: Type (r ()),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 (String -> m (InputPort Word8 m)) ->
	 ((?bindings :: Bindings r m) => OutputPort Word8 m) ->
	 Bindings r m -> String -> m (Bindings r m);
	loadBindingsWithProcs oif output bindings name = do
		{
		input <- oif name;
		bindings' <- parseEvalFromPortBulk (printResult output) bindings input;
		ipClose input;
		return bindings';
		};
--}

	parseFromPortBulk :: (Scheme m r,?refType :: Type (r ()),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 InputPort Word8 m ->
	 m [Object r m];
	parseFromPortBulk input = do	
		{
		text <- accumulateMaybeSource (parseUTF8Char (ipRead input));
		parseAllFromString text;
		};

	readWithProcs :: (Scheme m r,?refType :: Type (r ()),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 (String -> m (InputPort Word8 m)) ->
--	 ((?bindings :: Bindings r m) => OutputPort Word8 m) ->
	 String -> m [Object r m];
	readWithProcs oif name = do
		{
		input <- oif name;
		objects <- parseFromPortBulk input;
		ipClose input;
		return objects;
		};

	readFiles :: (Scheme m r,?refType :: Type (r ()),
		?syntacticbindings :: Binds Symbol (Syntax r m),
		?macrobindings :: Binds Symbol (Macro r m)) =>
	 (String -> m (InputPort Word8 m)) ->
	 [String] -> m [Object r m];
	readFiles oif [] = return [];
	readFiles oif (name:names) = do
		{
		objs1 <- readWithProcs oif name;
		objsr <- readFiles oif names;
		return (objs1 ++ objsr);
		};

	loadT :: (Scheme m r) =>
	 PureSystemInterface m r ->
	 (SList Char,()) -> TopLevelAction r m;
	loadT psi (MkSList filename,()) = MkTopLevelAction (\beg objs -> do
		{
		readObjects <- psiReadFile psi filename;
		beg (readObjects ++ objs);
		});

	currentInputPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (InputPort Word8 m);
	currentInputPortP fsi () = return (fsiCurrentInputPort fsi);

	currentOutputPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (OutputPort Word8 m);
	currentOutputPortP fsi () = return (fsiCurrentOutputPort fsi);

	currentErrorPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (OutputPort Word8 m);
	currentErrorPortP fsi () = return (fsiCurrentErrorPort fsi);

	openInputFileP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 (SList Char,()) -> m (InputPort Word8 m);
	openInputFileP fsi (MkSList name,()) = fsiOpenInputFile fsi name;

	openOutputFileP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 (SList Char,()) -> m (OutputPort Word8 m);
	openOutputFileP fsi (MkSList name,()) = fsiOpenOutputFile fsi name;

	systemMacroBindings :: (Scheme m r,?refType :: Type (r ())) =>
	 PureSystemInterface m r ->
	 Binds Symbol (TopLevelMacro r m) ->
	 Binds Symbol (TopLevelMacro r m);
	systemMacroBindings psi = concatenateList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT psi)
		];

	fullSystemBindings :: (Scheme m r,?refType :: Type (r ())) =>
	 FullSystemInterface m r -> Bindings r m -> m (Bindings r m);
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
