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

	mutualBind ::
		(
		RunnableScheme IO m r,
		?objType :: Type (Object r m),
		?stderr :: FlushSink IO Word8,
		?stdout :: FlushSink IO Word8,
		?stdin :: PeekSource IO (Maybe Word8)
		) =>
	 [String] ->
	 ((
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro IO r m),
	 	?macrobindings :: Binds Symbol (Macro IO r m),
	 	?syntacticbindings :: Binds Symbol (Syntax IO r m)
	 	) => Binds Symbol (Macro IO r m) -> Binds Symbol (Macro IO r m)) ->
	 ((
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro IO r m),
	 	?macrobindings :: Binds Symbol (Macro IO r m),
	 	?syntacticbindings :: Binds Symbol (Syntax IO r m)
	 	) => Binds Symbol (TopLevelMacro IO r m) -> Binds Symbol (TopLevelMacro IO r m)) ->
	 ((?macrobindings :: Binds Symbol (Macro IO r m),
	 	?syntacticbindings :: Binds Symbol (Syntax IO r m),
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro IO r m),
	 	?system :: FullSystemInterface IO m r
	 	) => a) ->
	 a;
	mutualBind loadpaths macroBinds tlBinds a = 
	 let
	 	{
	 	?syntacticbindings = emptyBindings;
	 	?system = ioFullSystemInterface id loadpaths;
	 	} in
	 let
		{
		mb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 macroBinds emptyBindings;
		tlb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 systemMacroBindings (fsiPure ?system) (tlBinds emptyBindings);
		} in
	 let
	 	{
	 	?macrobindings = mb;
	 	?toplevelbindings = tlb;
	 	} in a;

	optPrepend :: Bool -> a -> [a] -> [a];
	optPrepend True a as = (a:as);
	optPrepend _ _ as = as;

	instance
		(
		MonadGettableReference m r,
		MonadCreatable m r,
		MonadException (Object r m) m
		) =>
	 RunnableScheme m m r where
		{
		rsRun = id;
		rsLift = id;
		};

	instance
		(
		MonadGettableReference m r,
		MonadCreatable m r,
		MonadException (Object r (SchemeCPS r (m ()))) m
		) =>
	 RunnableScheme m (SchemeCPS r (m ())) r where
		{
		rsRun ma = runExceptionContinuationPass
		 (\err -> case err of
			{
			StringError s -> fail s;
			ExceptionError ex -> fail (show ex);
			ObjError obj -> throw obj;
			}) return ma;
		rsLift = lift;
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
				CPSWhichMonad -> let {?objType = Type::Type (Object IORef (CPS IORef))} in
				 (mutualBind loadpaths fullMacroBindings fullTopLevelBindings
				  (do
					{
					bindings <- (monadContFullBindings ++ fullSystemBindings (ioFullSystemInterface rsLift loadpaths)) emptyBindings;
					runProgramBindingsWithExit exitFailure (allFileNames "init.full.scm") bindings;
					}));
				IOWhichMonad -> let {?objType = Type::Type (Object IORef IO)} in
				 (mutualBind loadpaths fullMacroBindings fullTopLevelBindings
				  (do
					{
					bindings <- (monadFixFullBindings ++ fullSystemBindings (ioFullSystemInterface rsLift loadpaths)) emptyBindings;
					runProgramBindings exitFailure (allFileNames "init.full.scm") bindings;
					}));
				};
			PureFlavour -> case whichmonad of
				{
				CPSWhichMonad -> let {?objType = Type::Type (Object IOConst (CPS IOConst))} in
				 (mutualBind loadpaths pureMacroBindings pureTopLevelBindings
				  (do
					{
					bindings <- monadContPureBindings emptyBindings;
					runProgramBindingsWithExit exitFailure (allFileNames "init.pure.scm") bindings;
					}));
				IOWhichMonad -> let {?objType = Type::Type (Object IOConst IO)} in
				 (mutualBind loadpaths pureMacroBindings pureTopLevelBindings
				  (do
					{
					bindings <- monadFixPureBindings emptyBindings;
					runProgramBindings exitFailure (allFileNames "init.pure.scm") bindings;
					}));
				};
			StrictPureFlavour -> case whichmonad of
				{
				CPSWhichMonad -> let {?objType = Type::Type (Object IOConst (CPS IOConst))} in
				 (mutualBind loadpaths pureMacroBindings pureTopLevelBindings
				  (do
					{
					bindings <- monadContStrictPureBindings emptyBindings;
					runProgramBindingsWithExit exitFailure (allFileNames "init.pure.scm") bindings;
					}));
				IOWhichMonad -> let {?objType = Type::Type (Object IOConst IO)} in
				 (mutualBind loadpaths pureMacroBindings pureTopLevelBindings
				  (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					runProgramBindings exitFailure (allFileNames "init.pure.scm") bindings;
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
