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
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	data PureSystemInterface m r = MkPureSystemInterface
		{
		psiLoadBindings	:: Bindings r m -> String -> m (Bindings r m)
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

	doEval :: (Scheme m r) =>
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

	printResult :: (Scheme m r) =>
	 OutputPort Word8 m -> Object r m -> m ();
	printResult output obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteList output (encodeUTF8 (str ++ "\n"));
		};

	printeval :: (Scheme m r) =>
	 ((?bindings :: Bindings r m) => OutputPort Word8 m) ->
	 Bindings r m ->
	 Object r m ->
	 m (Bindings r m);
	printeval output bindings obj = doEval (printResult output) bindings obj;

	evalObjects :: (Scheme m r) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 [Object r m] ->
	 m (Bindings r m);
	evalObjects foo bindings [] = return bindings;
	evalObjects foo bindings (obj:objs) = do	
		{
		bindings' <- doEval foo bindings obj;
		evalObjects foo bindings' objs;
		};

	parseEvalFromString :: (Scheme m r) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 String ->
	 m (Bindings r m);
	parseEvalFromString foo bindings text = do	
		{
		objs <- let {?bindings=bindings} in parseAllFromString text;
		evalObjects foo bindings objs;
		};

	parseEvalFromPortByLine :: (Scheme m r,?refType :: Type (r ())) =>
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

	parseEvalFromPortBulk :: (Scheme m r) =>
	 ((?bindings :: Bindings r m) => Object r m -> m ()) ->
	 Bindings r m ->
	 InputPort Word8 m ->
	 m (Bindings r m);
	parseEvalFromPortBulk foo bindings input = do	
		{
		text <- accumulateSource (let {?bindings=bindings} in parseUTF8Char (ipRead input));
		parseEvalFromString foo bindings text;
		};

	loadBindingsWithProcs :: (Scheme m r) =>
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

	loadT :: (Scheme m r) =>
	 PureSystemInterface m r ->
	 Bindings r m -> (SList Char,()) -> m (Bindings r m,NullObjType);
	loadT psi bindings (MkSList filename,()) = do
		{
		bindings' <- psiLoadBindings psi bindings filename;
		return (bindings',MkNullObjType);
		};

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

	pureSystemBindings :: (Scheme m r) =>
	 PureSystemInterface m r -> Bindings r m -> m (Bindings r m);
	pureSystemBindings psi = concatenateList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT psi)
		];

	fullSystemBindings :: (Scheme m r) =>
	 FullSystemInterface m r -> Bindings r m -> m (Bindings r m);
	fullSystemBindings fsi = concatenateList
		[
		pureSystemBindings (fsiPure fsi),

		-- 6.6.1 Ports
		addProcBinding	"current-input-port"	(currentInputPortP fsi),
		addProcBinding	"current-output-port"	(currentOutputPortP fsi),
		addProcBinding	"current-error-port"	(currentErrorPortP fsi), -- nonstandard
		addProcBinding	"open-input-file"		(openInputFileP fsi),
		addProcBinding	"open-output-file"		(openOutputFileP fsi)
		];
	}
