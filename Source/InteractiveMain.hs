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
				CPSWhichMonad -> cpsInteract (do
					{
					let {fsi=ioFullSystemInterface lift loadpaths;};
					bindings <- (monadContFullBindings ++ fullSystemBindings fsi) emptyBindings;
					interactWithExit fsi bindings "init.full.scm";
					});
				IOWhichMonad -> do
					{
					let {fsi=ioFullSystemInterface id loadpaths;};
					bindings <- (monadFixFullBindings ++ fullSystemBindings fsi) emptyBindings;
					interact fsi bindings "init.full.scm";
					};
				});
			PureFlavour -> withRefType ioConstType (case whichmonad of
				{
				CPSWhichMonad -> cpsInteract (do
					{
					let {fsi=ioFullSystemInterface lift loadpaths;};
					bindings <- (monadContPureBindings ++ pureSystemBindings (fsiPure fsi)) emptyBindings;
					interactWithExit fsi bindings "init.pure.scm";
					});
				IOWhichMonad -> do
					{
					let {fsi=ioFullSystemInterface id loadpaths;};
					bindings <- (monadFixPureBindings ++ pureSystemBindings (fsiPure fsi)) emptyBindings;
					interact fsi bindings "init.pure.scm";
					};
				});
			StrictPureFlavour -> withRefType ioConstType (case whichmonad of
				{
				CPSWhichMonad -> cpsInteract (do
					{
					let {fsi=ioFullSystemInterface lift loadpaths;};
					bindings <- monadContStrictPureBindings emptyBindings;
					interactWithExit fsi bindings "init.pure.scm";
					});
				IOWhichMonad -> do
					{
					let {fsi=ioFullSystemInterface id loadpaths;};
					bindings <- monadFixStrictPureBindings emptyBindings;
					interact fsi bindings "init.pure.scm";
					};
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
