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
	import Arguments;
	import Org.Org.Semantic.HScheme;
	import Org.Org.Semantic.HBase;

{-
	cpsInteract :: (?refType :: Type (r ())) =>
	 CPS r () -> IO ();
	cpsInteract ma = runContinuationPass
	 (\_ -> fail "error in catch code!") return ma;

	withRefType ::
	 t -> ((?refType :: t) => a) -> a;
	withRefType t a = let {?refType = t} in a;
-}
	type CPS r = SchemeCPS r (IO ());
	type GCPS r = SchemeGCPS r (IO ());

	type IdentityConst = Constant Identity;

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
{-
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
	 ((?macrobindings :: SymbolBindings (Macro r m),
	 	?syntacticbindings :: SymbolBindings (Syntax r m),
	 	?toplevelbindings :: SymbolBindings (TopLevelMacro r m),
	 	?system :: FullSystemInterface m r
	 	) => Bindings r m -> String -> m result) ->
	 [String] ->
	 (forall a. IO a -> m a) ->
	 	((?macrobindings :: SymbolBindings (Macro r m),
	 	?syntacticbindings :: SymbolBindings (Syntax r m)
	 	) => SymbolBindings (Macro r m) -> SymbolBindings (Macro r m)) ->
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
-}
	main :: IO ();
	main = ioRunProgram (do
		{
		args <- ?getArgs;
		(mflavour,mwm,paths,initfile,filenames,verbose) <- parseArgs args;
		let
			{
			loadpaths = ["."] ++ paths ++ ["/usr/share/hscheme"];
			whichmonad = unJust GCPSWhichMonad mwm;
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
{-
			GCPSWhichMonad ->
			 let {?objType = MkType::Type (Object IORef (GCPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = ioRead loadpaths} in
			 case flavour of
				{
				FullStdBindings ->
				 let {?system = ioSystem (lift . lift)} in
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContFullBindings ++ monadGuardBindings ++ monadFixBindings ++ fullSystemBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.full.scm");
					interactWithExit bindings "";
					}));
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContPureBindings ++ monadGuardBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					interactWithExit bindings "";
					}));
				StrictPureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContStrictPureBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					interactWithExit bindings "";
					}));
				};
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
					interactWithExit bindings "";
--					runObjectsWithExit printResult objects bindings;
					}));
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContPureBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					interactWithExit bindings "";
					}));
				StrictPureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContStrictPureBindings ++ monadFixBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					runObjectsWithExit printResult objects bindings;
					interactWithExit bindings "";
					}));
				};
-}
			IOWhichMonad ->
			 let {?objType = MkType::Type (Object IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = ioRead loadpaths} in
			 let {?system = ioSystem id} in
			 case flavour of
				{
				FullStdBindings ->
				 (mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadFixFullBindings ++ monadGuardBindings ++ fullSystemBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.full.scm");
					interact bindings "";
					}));
				PureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadFixPureBindings ++ monadGuardBindings) emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					interact bindings "";
					}));
				StrictPureStdBindings ->
				 (mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					objects <- readFiles (allFileNames "init.pure.scm");
					interact bindings "";
					}));
				};
			IdentityWhichMonad -> fail "can't use pure monad for interaction";
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
