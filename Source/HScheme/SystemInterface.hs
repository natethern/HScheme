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
		fsiCurrentInputPort		:: InputPort Char m,
		fsiCurrentOutputPort	:: OutputPort Char m,
		fsiCurrentErrorPort		:: OutputPort Char m,
		fsiOpenInputFile		:: String -> m (InputPort Char m),
		fsiOpenOutputFile		:: String -> m (OutputPort Char m)
		};

	doEval :: (Scheme m r) =>
	 (Object r m -> m ()) -> Bindings r m -> Object r m -> m (Bindings r m);
	doEval foo bindings obj = do
		{
		(bindings',result) <- topLevelEvaluate bindings obj;
		foo result;
		return bindings';	
		};

	printResult :: (Scheme m r) =>
	 OutputPort Char m -> Object r m -> m ();
	printResult output obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		opWriteStrLn output str;
		};

	printeval :: (Scheme m r) =>
	 OutputPort Char m -> Bindings r m -> Object r m -> m (Bindings r m);
	printeval output bindings obj = doEval (printResult output) bindings obj;

	evalObjects :: (Scheme m r) =>
	 (Object r m -> m ()) ->
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
	 (Object r m -> m ()) ->
	 Bindings r m ->
	 String ->
	 m (Bindings r m);
	parseEvalFromString foo bindings text = do	
		{
		objs <- parseAllFromString text;
		evalObjects foo bindings objs;
		};

	parseEvalFromPortByLine :: (Scheme m r) =>
	 (Object r m -> m ()) ->
	 Bindings r m ->
	 InputPort Char m ->
	 m (Bindings r m);
	parseEvalFromPortByLine foo bindings input = do	
		{
		mobject <- parseFromPort input;
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
	 (Object r m -> m ()) ->
	 Bindings r m ->
	 InputPort Char m ->
	 m (Bindings r m);
	parseEvalFromPortBulk foo bindings input = do	
		{
		text <- ipReadAll input;
		parseEvalFromString foo bindings text;
		};

	loadBindingsWithProcs :: (Scheme m r) =>
	 (String -> m (InputPort Char m)) ->
	 OutputPort Char m ->
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
	 Bindings r m -> (SList Char,()) -> m (Bindings r m,ArgNoneType);
	loadT psi bindings (MkSList filename,()) = do
		{
		bindings' <- psiLoadBindings psi bindings filename;
		return (bindings',MkArgNoneType);
		};

	currentInputPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (InputPort Char m);
	currentInputPortP fsi () = return (fsiCurrentInputPort fsi);

	currentOutputPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (OutputPort Char m);
	currentOutputPortP fsi () = return (fsiCurrentOutputPort fsi);

	currentErrorPortP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 () -> m (OutputPort Char m);
	currentErrorPortP fsi () = return (fsiCurrentErrorPort fsi);

	openInputFileP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 (SList Char,()) -> m (InputPort Char m);
	openInputFileP fsi (MkSList name,()) = fsiOpenInputFile fsi name;

	openOutputFileP :: (Scheme m r) =>
	 FullSystemInterface m r ->
	 (SList Char,()) -> m (OutputPort Char m);
	openOutputFileP fsi (MkSList name,()) = fsiOpenOutputFile fsi name;

	pureSystemBindings :: (Scheme m r) =>
	 PureSystemInterface m r -> Bindings r m -> m (Bindings r m);
	pureSystemBindings psi = chainList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT psi)
		];

	fullSystemBindings :: (Scheme m r) =>
	 FullSystemInterface m r -> Bindings r m -> m (Bindings r m);
	fullSystemBindings fsi = chainList
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
