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
			GCPSWhichMonad ->
			 let {?objType = MkType::Type (Object IORef (GCPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = lift . lift . (ioRead loadpaths)} in
			 let {?system = ioSystem (lift . lift)} in
			 case flavour of
				{
				FullStdBindings ->
				 mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContFullBindings ++ monadGuardBindings ++ monadFixBindings ++ fullSystemBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.full.scm"));
						interactWithExit bindings commands;
						});
					});
				PureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContPureBindings ++ monadGuardBindings ++ monadFixBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
						interactWithExit bindings commands;
						});
					});
				StrictPureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContStrictPureBindings ++ monadFixBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
						interactWithExit bindings commands;
						});
					});
				};
			CPSWhichMonad ->
			 let {?objType = MkType::Type (Object IORef (CPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = lift . (ioRead loadpaths)} in
			 let {?system = ioSystem lift} in
			 case flavour of
				{
				FullStdBindings ->
				 mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContFullBindings ++ monadFixBindings ++ fullSystemBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.full.scm"));
						interactWithExit bindings commands;
						});
					});
				PureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContPureBindings ++ monadFixBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
						interactWithExit bindings commands;
						});
					});
				StrictPureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadContStrictPureBindings ++ monadFixBindings) emptyBindings;
					rsRun (do
						{
						commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
						interactWithExit bindings commands;
						});
					});
				};
			IOWhichMonad ->
			 let {?objType = MkType::Type (Object IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = ioRead loadpaths} in
			 let {?system = ioSystem id} in
			 case flavour of
				{
				FullStdBindings ->
				 mutualBind fullMacroBindings (fullTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadFixFullBindings ++ monadGuardBindings ++ fullSystemBindings) emptyBindings;
					commands <- fExtract (fmap readLoad (allFileNames "init.full.scm"));
					interact bindings commands;
					});
				PureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- (monadFixPureBindings ++ monadGuardBindings) emptyBindings;
					commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
					interact bindings commands;
					});
				StrictPureStdBindings ->
				 mutualBind pureMacroBindings (pureTopLevelBindings ++ (loadTopLevelBindings readLoad)) (do
					{
					bindings <- monadFixStrictPureBindings emptyBindings;
					commands <- fExtract (fmap readLoad (allFileNames "init.pure.scm"));
					interact bindings commands;
					});
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
