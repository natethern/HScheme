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

	cpsRun :: (?objType :: Type (Object r m)) =>
	 CPS r () -> IO ();
	cpsRun ma = runExceptionContinuationPass
	 (\_ -> fail "error in catch code!") return ma;

	defaultFlavour :: SchemeFlavour;
	defaultFlavour = FullFlavour;

	withRefType ::
	 Type (r ()) -> ((?objType :: Type (Object r m)) => a) -> a;
	withRefType _ a = let {?objType = Type} in a;

	boundRun ::
		(
		MonadBottom m,
		MonadException (Object r m) m,
		MonadGettableReference m r,
		MonadCreatable m r,
		?objType :: Type (Object r m),
		?stderr :: FlushSink IO Word8,
		?stdout :: FlushSink IO Word8,
		?stdin :: PeekSource IO (Maybe Word8)
		) =>
	 ((?macrobindings :: Binds Symbol (Macro m r m),
	 	?syntacticbindings :: Binds Symbol (Syntax m r m),
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
	 	?system :: FullSystemInterface m m r
	 	) => Bindings r m -> m result) ->
	 [String] ->
	 (forall a. IO a -> m a) ->
	 	((
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
	 	?macrobindings :: Binds Symbol (Macro m r m),
	 	?syntacticbindings :: Binds Symbol (Syntax m r m)
	 	) => Binds Symbol (Macro m r m) -> Binds Symbol (Macro m r m)) ->
	 	((
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro m r m),
	 	?macrobindings :: Binds Symbol (Macro m r m),
	 	?syntacticbindings :: Binds Symbol (Syntax m r m)
	 	) => Binds Symbol (TopLevelMacro m r m) -> Binds Symbol (TopLevelMacro m r m)) ->
	 ((?system :: FullSystemInterface m m r) => Bindings r m -> m (Bindings r m)) ->
	 m result;
	boundRun interacter loadpaths lifter macroBinds tlBinds symbolBinds = 
	 let {?syntacticbindings = emptyBindings} in
	 let
		{
		mb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 macroBinds emptyBindings;
		system = let {?macrobindings = mb} in
		 ioFullSystemInterface lifter loadpaths;
		tlb = let {?macrobindings = mb;?toplevelbindings = tlb} in
		 systemMacroBindings (fsiPure system) (tlBinds emptyBindings);
		} in
	 let
	 	{
	 	?macrobindings = mb;
	 	?system = system;
	 	?toplevelbindings = tlb;
	 	} in
	 do
		{
		bindings <- symbolBinds emptyBindings;
		interacter bindings;
		};

	optPrepend :: Bool -> a -> [a] -> [a];
	optPrepend True a as = (a:as);
	optPrepend _ _ as = as;

	main :: IO ();
--	main = putStrLn (show test1);
--	main1 :: IO ();
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
			};
		case flavour of
			{
			FullFlavour -> case whichmonad of
				{
				CPSWhichMonad -> withRefType ioRefType (cpsRun (boundRun (runProgramWithExit (lift exitFailure) (optPrepend initfile "init.full.scm" filenames)) loadpaths lift
				 fullMacroBindings fullTopLevelBindings (monadContFullBindings ++ fullSystemBindings ?system)));
				IOWhichMonad -> withRefType ioRefType (boundRun (runProgram exitFailure (optPrepend initfile "init.full.scm" filenames)) loadpaths id
				 fullMacroBindings fullTopLevelBindings (monadFixFullBindings ++ fullSystemBindings ?system));
				};
			PureFlavour -> case whichmonad of
				{
				CPSWhichMonad -> withRefType ioConstType (cpsRun (boundRun (runProgramWithExit (lift exitFailure) (optPrepend initfile "init.pure.scm" filenames)) loadpaths lift
				 pureMacroBindings pureTopLevelBindings monadContPureBindings));
				IOWhichMonad -> withRefType ioConstType (boundRun (runProgram exitFailure (optPrepend initfile "init.pure.scm" filenames)) loadpaths id
				 pureMacroBindings pureTopLevelBindings monadFixPureBindings);
				};
			StrictPureFlavour -> case whichmonad of
				{
				CPSWhichMonad -> withRefType ioConstType (cpsRun (boundRun (runProgramWithExit (lift exitFailure) (optPrepend initfile "init.pure.scm" filenames)) loadpaths lift
				 macroBindings pureTopLevelBindings monadContStrictPureBindings));
				IOWhichMonad -> withRefType ioConstType (boundRun (runProgram exitFailure (optPrepend initfile "init.pure.scm" filenames)) loadpaths id
				 macroBindings pureTopLevelBindings monadFixStrictPureBindings);
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
