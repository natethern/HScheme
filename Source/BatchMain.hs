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

module Main where
	{
	import Org.Org.Semantic.HScheme;
	import Org.Org.Semantic.HBase;
	import System.Exit;

	type CPS r = SchemeCPS r (IO ());

	data SchemeFlavour = FullFlavour | PureFlavour | StrictPureFlavour;

	data SchemeWhichMonad = IOWhichMonad | CPSWhichMonad;

	parseArgs :: (Monad m) =>
	 [String] -> m (Maybe SchemeFlavour,Maybe SchemeWhichMonad,[String],Bool,[String]);
	parseArgs [] = return (Nothing,Nothing,[],True,[]);
	parseArgs ("-":files) = return (Nothing,Nothing,[],True,files);
	parseArgs ("--pure":args) = do
		{
		(_,m,paths,initfile,files) <- parseArgs args;
		return (Just PureFlavour,m,paths,initfile,files);
		};
	parseArgs ("--cps":args) = do
		{
		(f,_,paths,initfile,files) <- parseArgs args;
		return (f,Just CPSWhichMonad,paths,initfile,files);
		};
	parseArgs ("--plain":args) = do
		{
		(f,_,paths,initfile,files) <- parseArgs args;
		return (f,Just IOWhichMonad,paths,initfile,files);
		};
	parseArgs ("-p":args) = do
		{
		(_,m,paths,initfile,files) <- parseArgs args;
		return (Just PureFlavour,m,paths,initfile,files);
		};
	parseArgs ("--full":args) = do
		{
		(_,m,paths,initfile,files) <- parseArgs args;
		return (Just FullFlavour,m,paths,initfile,files);
		};
	parseArgs ("-f":args) = do
		{
		(_,m,paths,initfile,files) <- parseArgs args;
		return (Just FullFlavour,m,paths,initfile,files);
		};
	parseArgs (('-':('I':path@(_:_))):args) = do
		{
		(f,m,paths,initfile,files) <- parseArgs args;
		return (f,m,path:paths,initfile,files);
		};
	parseArgs ("-I":(path:args)) = do
		{
		(f,m,paths,initfile,files) <- parseArgs args;
		return (f,m,path:paths,initfile,files);
		};
	parseArgs ("--noinit":args) = do
		{
		(f,m,paths,_,files) <- parseArgs args;
		return (f,m,paths,False,files);
		};
	parseArgs ("-n":args) = do
		{
		(f,m,paths,_,files) <- parseArgs args;
		return (f,m,paths,False,files);
		};
	parseArgs (flag@('-':_):args) = fail ("unrecognised flag "++(show flag));
	parseArgs (file:args) = do
		{
		(f,m,paths,initfile,files) <- parseArgs args;
		return (f,m,paths,initfile,(file:files));
		};

	defaultFlavour :: SchemeFlavour;
	defaultFlavour = FullFlavour;

	optPrepend :: Bool -> a -> [a] -> [a];
	optPrepend True a as = (a:as);
	optPrepend _ _ as = as;

	printResult ::
		(
		Build IO r,
		?objType :: Type (Object r m),
		?stdout :: FlushSink IO Word8
		) =>
	 Object r m -> IO ();
	printResult obj = if (isNullObject obj)
	 then (return ())
	 else do
		{
		str <- toString obj;
		fsSinkList ?stdout (encodeUTF8 (str ++ "\n"));
		};

	reportError ::
		(
		Build IO r,
		?objType :: Type (Object r m),
		?stderr :: FlushSink IO Word8
		) =>
	 Object r m -> IO ();
	reportError obj = do
		{
		text <- toString obj;
		fsSinkList ?stderr (encodeUTF8 ("error: "++text++"\n"));
		fsFlush ?stderr;
		exitFailure;
		};

	main :: IO ();
	main = ioRunProgram (do
		{
		args <- ?getArgs;
		(mflavour,mwm,paths,initfile,filenames) <- parseArgs args;
		let
			{
			loadpaths = ["."] ++ paths ++ ["/usr/share/hscheme"];
			flavour = unJust defaultFlavour mflavour;
			whichmonad = unJust (case flavour of
				{
				FullFlavour -> CPSWhichMonad;
				_ -> IOWhichMonad;
				}) mwm;
			allFileNames initFile = optPrepend initfile initFile filenames
			};
		case flavour of
			{
			FullFlavour -> case whichmonad of
				{
				CPSWhichMonad ->
				 let {?objType = Type::Type (Object IORef (CPS IORef))} in
				 let {?load = ioLoad loadpaths} in
				 let {?system = ioSystem rsLift} in
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- (monadContFullBindings ++ fullSystemBindings) emptyBindings;
					runProgramBindingsWithExit printResult reportError (allFileNames "init.full.scm") bindings;
					}));
				IOWhichMonad ->
				 let {?objType = Type::Type (Object IORef IO)} in
				 let {?load = ioLoad loadpaths} in
				 let {?system = ioSystem rsLift} in
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- (monadFixFullBindings ++ fullSystemBindings) emptyBindings;
					runProgramBindings printResult reportError (allFileNames "init.full.scm") bindings;
					}));
				};
			PureFlavour -> case whichmonad of
				{
				CPSWhichMonad ->
				 let {?objType = Type::Type (Object IOConst (CPS IOConst))} in
				 let {?load = ioLoad loadpaths} in
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- monadContPureBindings emptyBindings;
					runProgramBindingsWithExit printResult reportError (allFileNames "init.pure.scm") bindings;
					}));
				IOWhichMonad ->
				 let {?objType = Type::Type (Object IOConst IO)} in
				 let {?load = ioLoad loadpaths} in
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- monadFixPureBindings emptyBindings;
					runProgramBindings printResult reportError (allFileNames "init.pure.scm") bindings;
					}));
				};
			StrictPureFlavour -> case whichmonad of
				{
				CPSWhichMonad ->
				 let {?objType = Type::Type (Object IOConst (CPS IOConst))} in
				 let {?load = ioLoad loadpaths} in
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- monadContStrictPureBindings emptyBindings;
					runProgramBindingsWithExit printResult reportError (allFileNames "init.pure.scm") bindings;
					}));
				IOWhichMonad ->
				 let {?objType = Type::Type (Object IOConst IO)} in
				 let {?load = ioLoad loadpaths} in
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ systemTopLevelBindings) (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					runProgramBindings printResult reportError (allFileNames "init.pure.scm") bindings;
					}));
				};
			};
		});


{-- for profiling
	rep :: Int -> IO () -> IO ();
	rep 0 f = return ();
	rep n f = do
		{
		f;
		rep (n-1) f;
		};

	main :: IO ();
	main = rep 100 main';
--}
	}
