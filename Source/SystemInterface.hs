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

module SystemInterface where
	{
	import Bindings;
	import TopLevel;
	import Procedures;
	import SExpParser;
	import Conversions;
	import Object;
	import Port;
	import Type;

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

	printeval :: (Scheme x m r) =>
	 OutputPort Char m -> Bindings r m -> Object r m -> m (Bindings r m);
	printeval output bindings obj = do
		{
		(bindings',result) <- topLevelEvaluate bindings obj;
		if (isNullObject result)
		 then (return ())
		 else do
			{
			str <- toString result;
			opWriteStrLn output str;
			};
		return bindings';	
		};

	readLoad :: (Scheme x m r) =>
	 Bindings r m ->
	 InputPort Char m ->
	 OutputPort Char m ->
	 m (Bindings r m);
	readLoad bindings input output = do	
		{
		mobject <- portRead input;
		case mobject of
			{
			Nothing -> return bindings;
			Just obj -> do
				{
				bindings' <- printeval output bindings obj;
				readLoad bindings' input output;
				};
			};
		};

	loadBindingsWithProcs :: (Scheme x m r) =>
	 (String -> m (InputPort Char m)) ->
	 OutputPort Char m ->
	 Bindings r m -> String -> m (Bindings r m);
	loadBindingsWithProcs oif output bindings name = do
		{
		input <- oif name;
		bindings' <- readLoad bindings input output;
		ipClose input;
		return bindings';
		};

	loadT :: (Scheme x m r) =>
	 PureSystemInterface m r ->
	 Bindings r m -> (StringType) -> m (Bindings r m,ArgNoneType);
	loadT psi bindings (MkStringType filename) = do
		{
		bindings' <- psiLoadBindings psi bindings filename;
		return (bindings',MkArgNoneType);
		};

	currentInputPortP :: (Scheme x m r) =>
	 FullSystemInterface m r ->
	 Type (r ()) -> () -> m (InputPort Char m);
	currentInputPortP fsi Type () = return (fsiCurrentInputPort fsi);

	currentOutputPortP :: (Scheme x m r) =>
	 FullSystemInterface m r ->
	 Type (r ()) -> () -> m (OutputPort Char m);
	currentOutputPortP fsi Type () = return (fsiCurrentOutputPort fsi);

	currentErrorPortP :: (Scheme x m r) =>
	 FullSystemInterface m r ->
	 Type (r ()) -> () -> m (OutputPort Char m);
	currentErrorPortP fsi Type () = return (fsiCurrentErrorPort fsi);

	openInputFileP :: (Scheme x m r) =>
	 FullSystemInterface m r ->
	 Type (r ()) -> (StringType,()) -> m (InputPort Char m);
	openInputFileP fsi Type (MkStringType name,()) = fsiOpenInputFile fsi name;

	openOutputFileP :: (Scheme x m r) =>
	 FullSystemInterface m r ->
	 Type (r ()) -> (StringType,()) -> m (OutputPort Char m);
	openOutputFileP fsi Type (MkStringType name,()) = fsiOpenOutputFile fsi name;

	pureSystemBindings :: (Scheme x m r) =>
	 PureSystemInterface m r -> Bindings r m -> m (Bindings r m);
	pureSystemBindings psi = chainList
		[
		-- 6.6.4 System Interface
		addTopLevelMacroBinding	"load"	(loadT psi)
		];

	fullSystemBindings :: (Scheme x m r) =>
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
