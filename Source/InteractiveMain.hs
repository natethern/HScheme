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

	data SchemeFlavour = FullFlavour | PureFlavour | StrictPureFlavour;

	data SchemeWhichMonad = IOWhichMonad | CPSWhichMonad;

	parseArgs :: (Monad m) =>
	 [String] -> m (Maybe SchemeFlavour,Maybe SchemeWhichMonad,[String],[String]);
	parseArgs [] = return (Nothing,Nothing,[],[]);
	parseArgs ("-":files) = return (Nothing,Nothing,[],files);
	parseArgs ("--pure":args) = do
		{
		(_,m,paths,files) <- parseArgs args;
		return (Just PureFlavour,m,paths,files);
		};
	parseArgs ("--cps":args) = do
		{
		(f,_,paths,files) <- parseArgs args;
		return (f,Just CPSWhichMonad,paths,files);
		};
	parseArgs ("--plain":args) = do
		{
		(f,_,paths,files) <- parseArgs args;
		return (f,Just IOWhichMonad,paths,files);
		};
	parseArgs ("-p":args) = do
		{
		(_,m,paths,files) <- parseArgs args;
		return (Just PureFlavour,m,paths,files);
		};
	parseArgs ("--full":args) = do
		{
		(_,m,paths,files) <- parseArgs args;
		return (Just FullFlavour,m,paths,files);
		};
	parseArgs ("-f":args) = do
		{
		(_,m,paths,files) <- parseArgs args;
		return (Just FullFlavour,m,paths,files);
		};
	parseArgs (('-':('I':path@(_:_))):args) = do
		{
		(f,m,paths,files) <- parseArgs args;
		return (f,m,path:paths,files);
		};
	parseArgs ("-I":(path:args)) = do
		{
		(f,m,paths,files) <- parseArgs args;
		return (f,m,path:paths,files);
		};
	parseArgs (flag@('-':_):args) = fail ("unrecognised flag "++(show flag));
	parseArgs (file:args) = do
		{
		(f,m,paths,files) <- parseArgs args;
		return (f,m,paths,(file:files));
		};

	cpsInteract :: (?refType :: Type (r ())) =>
	 CPS r () -> IO ();
	cpsInteract ma = runContinuationPass
	 (\_ -> fail "error in catch code!") return ma;

	defaultFlavour :: SchemeFlavour;
	defaultFlavour = FullFlavour;

	withRefType ::
	 t -> ((?refType :: t) => a) -> a;
	withRefType t a = let {?refType = t} in a;

	boundInteraction ::
		(
		MonadBottom m,
		MonadException (Object r m) m,
		MonadGettableReference m r,
		MonadCreatable m r,
		?refType :: Type (r ()),
		?stderr :: FlushSink IO Word8,
		?stdout :: FlushSink IO Word8,
		?stdin :: PeekSource IO Word8
		) =>
	 ((?macrobindings :: Binds Symbol (Macro r m),
	 	?syntacticbindings :: Binds Symbol (Syntax r m),
	 	?toplevelbindings :: Binds Symbol (TopLevelMacro r m),
	 	?system :: FullSystemInterface m r
	 	) => Bindings r m -> String -> m result) ->
	 [String] ->
	 (forall a. IO a -> m a) ->
	 	((?macrobindings :: Binds Symbol (Macro r m),
	 	?syntacticbindings :: Binds Symbol (Syntax r m)
	 	) => Binds Symbol (Macro r m) -> Binds Symbol (Macro r m)) ->
	 ((?system :: FullSystemInterface m r) => Bindings r m -> m (Bindings r m)) ->
	 String ->
	 m result;
	boundInteraction interacter loadpaths lifter macroBinds symbolBinds filename = 
	 let {?syntacticbindings = emptyBindings} in
	 let {?macrobindings = let
			{
			mb = let {?macrobindings = mb} in macroBinds emptyBindings;
			} in mb} in
	 let {?system = ioFullSystemInterface lifter loadpaths} in
	 let {?toplevelbindings = systemMacroBindings (fsiPure ?system) emptyBindings} in
	 do
		{
		bindings <- symbolBinds emptyBindings;
		interacter bindings filename;
		};

	main :: IO ();
	main = ioRunProgram (do
		{
		args <- ?getArgs;
		(mflavour,mwm,paths,files) <- parseArgs args;
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
			FullFlavour -> withRefType ioRefType (case whichmonad of
				{
				CPSWhichMonad -> cpsInteract (boundInteraction interactWithExit loadpaths lift
				 fullMacroBindings (monadContFullBindings ++ fullSystemBindings ?system) "init.full.scm");
				IOWhichMonad -> boundInteraction interact loadpaths id
				 fullMacroBindings (monadFixFullBindings ++ fullSystemBindings ?system) "init.full.scm";
				});
			PureFlavour -> withRefType ioConstType (case whichmonad of
				{
				CPSWhichMonad -> cpsInteract (boundInteraction interactWithExit loadpaths lift
				 macroBindings monadContPureBindings "init.pure.scm");
				IOWhichMonad -> boundInteraction interact loadpaths id
				 macroBindings monadFixPureBindings "init.pure.scm";
				});
			StrictPureFlavour -> withRefType ioConstType (case whichmonad of
				{
				CPSWhichMonad -> cpsInteract (boundInteraction interactWithExit loadpaths lift
				 macroBindings monadContStrictPureBindings "init.pure.scm");
				IOWhichMonad -> boundInteraction interact loadpaths id
				 macroBindings monadFixStrictPureBindings "init.pure.scm";
				});
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
