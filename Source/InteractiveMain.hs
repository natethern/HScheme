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
	import System.Environment;

	type CPS r = SchemeCPS r (IO ());

	instance Show (SchemeCPSError IORef (IO ())) where
		{
		show (StringError s) = s;
		show (ExceptionError ex) = show ex;
		show (ObjError ex) = "Scheme object error";
		};

	instance (MonadGettableReference IO r,MonadCreatable IO r) =>
	 MonadThrow (Object r IO) IO where
		{
		throw obj = do
			{
			text <- toString obj;
			fail text;
			};
		};

	instance (MonadGettableReference IO r,MonadCreatable IO r) =>
	 MonadException (Object r IO) IO where
		{
		catch foo cc = catchSingle foo (\ex -> do
			{
			obj <- getConvert (MkSymbol "failure",MkSList (show ex));
			cc obj;
			});
		};

	type Interact m r = FullSystemInterface m r -> m ();

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

	cpsInteract :: 
		(
		MonadGettableReference IO r,
		MonadCreatable IO r,
		?refType :: Type (r ())
		) =>
	 [String] -> (Interact (CPS r) r) -> IO ();
	cpsInteract loadpaths interact = runContinuationPass
	 (\_ -> fail "error in catch code!")
	 return
	 (interact (ioFullSystemInterface lift loadpaths));

	defaultFlavour :: SchemeFlavour;
	defaultFlavour = FullFlavour;

	main :: IO ();
	main = do
		{
		args <- getArgs;
		(mflavour,mwm,paths,files) <- parseArgs args;
		let {loadpaths = ["."] ++ paths ++ ["/usr/share/hscheme"]};
		let {flavour = unJust defaultFlavour mflavour};
		let {whichmonad = unJust (case flavour of
			{
			FullFlavour -> CPSWhichMonad;
			_ -> IOWhichMonad;
			}) mwm};
		case flavour of
			{
			FullFlavour -> let {?refType = Type} in cpsInteract loadpaths (fullInteract :: Interact (CPS IORef) IORef);
			PureFlavour -> let {?refType = Type} in (pureInteract :: Interact IO (Constant IO)) (ioFullSystemInterface id loadpaths);
			StrictPureFlavour -> let {?refType = Type} in (strictPureInteract :: Interact IO (Constant IO)) (ioFullSystemInterface id loadpaths);
			};
		};


{-- for profiling
	rep :: Integer -> IO () -> IO ();
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
