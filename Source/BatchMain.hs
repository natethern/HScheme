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

	type CPS r = SchemeCPS r (IO ());

	type IdentityConst = Constant Identity;

	data SchemeStdBindings = FullStdBindings | PureStdBindings | StrictPureStdBindings;

	instance Show SchemeStdBindings where
		{
		show FullStdBindings = "full";
		show PureStdBindings = "pure";
		show StrictPureStdBindings = "strict pure";
		};

	data SchemeWhichMonad = IdentityWhichMonad | IOWhichMonad | CPSWhichMonad;

	instance Show SchemeWhichMonad where
		{
		show IdentityWhichMonad = "pure";
		show IOWhichMonad = "IO";
		show CPSWhichMonad = "CPS IO";
		};

	parseArgs :: (Monad m) =>
	 [String] -> m (Maybe SchemeStdBindings,Maybe SchemeWhichMonad,[String],Bool,[String],Bool);
	parseArgs [] = return (Nothing,Nothing,[],True,[],False);
	parseArgs ("-":files) = return (Nothing,Nothing,[],True,files,False);
	parseArgs ("--mcps":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just CPSWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--mio":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just IOWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--mpure":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just IdentityWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--bfull":args) = do
		{
		(_,m,paths,initfile,files,verbose) <- parseArgs args;
		return (Just FullStdBindings,m,paths,initfile,files,verbose);
		};
	parseArgs ("--bpure":args) = do
		{
		(_,m,paths,initfile,files,verbose) <- parseArgs args;
		return (Just PureStdBindings,m,paths,initfile,files,verbose);
		};
	parseArgs (('-':('I':path@(_:_))):args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,path:paths,initfile,files,verbose);
		};
	parseArgs ("-I":(path:args)) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,path:paths,initfile,files,verbose);
		};
	parseArgs ("--noinit":args) = do
		{
		(f,m,paths,_,files,verbose) <- parseArgs args;
		return (f,m,paths,False,files,verbose);
		};
	parseArgs ("-n":args) = do
		{
		(f,m,paths,_,files,verbose) <- parseArgs args;
		return (f,m,paths,False,files,verbose);
		};
	parseArgs ("-v":args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,paths,initfile,files,True);
		};
	parseArgs (flag@('-':_):args) = fail ("unrecognised flag "++(show flag));
	parseArgs (file:args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,paths,initfile,(file:files),verbose);
		};

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

	main :: IO ();
	main = ioRunProgram (do
		{
		args <- ?getArgs;
		(mflavour,mwm,paths,initfile,filenames,verbose) <- parseArgs args;
		let
			{
			loadpaths = ["."] ++ paths ++ ["/usr/share/hscheme"];
			whichmonad = unJust CPSWhichMonad mwm;
			flavour = unJust (case whichmonad of
				{
				IdentityWhichMonad -> PureStdBindings;
				_ -> FullStdBindings;
				}) mflavour;
			allFileNames initFile = optPrepend initfile initFile filenames
			};
		if verbose then do
			{
			fsSinkList ?stderr (encodeUTF8 "HScheme 0.1\n");
			fsSinkList ?stderr (encodeUTF8 "Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>\n");
			fsSinkList ?stderr (encodeUTF8 ("monad: " ++ (show whichmonad) ++ "\n"));
			fsSinkList ?stderr (encodeUTF8 ("bindings: " ++ (show flavour) ++ "\n\n"));
			}
		 else return ();
		case whichmonad of
			{
			CPSWhichMonad ->
			 let {?objType = MkType::Type (Object IORef (CPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = ioRead loadpaths} in
			 case flavour of
				{
				FullStdBindings ->
				 let {?system = ioSystem lift} in
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContFullBindings ++ monadFixBindings ++ fullSystemBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.full.scm");
					runObjectsWithExit printResult objects bindings;
					}));
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContPureBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjectsWithExit printResult objects bindings;
					}));
				StrictPureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContStrictPureBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjectsWithExit printResult objects bindings;
					}));
				};
			IOWhichMonad ->
			 let {?objType = MkType::Type (Object IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = ioRead loadpaths} in
			 case flavour of
				{
				FullStdBindings ->
				 let {?system = ioSystem id} in
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadFixFullBindings ++ fullSystemBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.full.scm");
					runObjects printResult objects bindings;
					}));
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixPureBindings emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjects printResult objects bindings;
					}));
				StrictPureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjects printResult objects bindings;
					}));
				};
			IdentityWhichMonad ->
			 let {?objType = MkType::Type (Object IdentityConst Identity)} in
			 let {?binder = recursiveBinder} in
			 let {?read = ioRead loadpaths} in
			 case flavour of
				{
				FullStdBindings -> fail "can't use pure monad with full bindings";
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixPureBindings emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjects printResult objects bindings;
					}));
				StrictPureStdBindings -> 
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjects printResult objects bindings;
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
