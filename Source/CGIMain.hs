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
	import Org.Org.Semantic.HBase.Encoding.URI;
	import Org.Org.Semantic.HBase.Protocol.CGI;
	import Org.Org.Semantic.HBase;

	type CPS r = SchemeCPS (IO ()) (CompleteObject r);
	type GCPS r = SchemeGCPS (IO ()) (CompleteObject r);

	type IdentityConst = Constant Identity;

	getStdBindings :: SchemeWhichMonad -> QueryParameters -> SchemeStdBindings;
	getStdBindings whichmonad params = case findQueryParameter (encodeLatin1 "bindings") params of
		{
		Just s | s == (encodeLatin1 "full") -> FullStdBindings;
		Just s | s == (encodeLatin1 "pure") -> PureStdBindings;
		Just s | s == (encodeLatin1 "strict") -> StrictPureStdBindings;
		_ -> defaultStdBindings whichmonad;
		};

	getWhichMonad :: QueryParameters -> SchemeWhichMonad;
	getWhichMonad params = case findQueryParameter (encodeLatin1 "monad") params of
		{
		Just s | s == (encodeLatin1 "gcps") -> GCPSWhichMonad;
		Just s | s == (encodeLatin1 "cps") -> CPSWhichMonad;
		Just s | s == (encodeLatin1 "io") -> IOWhichMonad;
		Just s | s == (encodeLatin1 "pure") -> IdentityWhichMonad;
		_ -> defaultWhichMonad;
		};

	main :: IO ();
	main = cgiRunProgram printError (do
		{
		putStrLn "Content-Type: text/plain\n";
		params <- cgiGetQueryParameters;
		source <- case findQueryParameter (encodeLatin1 "input") params of
			{
			Just sourceBytes -> return (decodeLatin1 sourceBytes);
			Nothing -> fail "no input";
			};
		let
			{
			verbose = case findQueryParameter (encodeLatin1 "verbose") params of
				{
				Just [0x30] -> False;	-- "0"
				Just _ -> True;
				Nothing -> False;
				};
			whichmonad = getWhichMonad params;
			stdbindings = getStdBindings whichmonad params;
			};
		if verbose
		 then verbosity ?stdout whichmonad stdbindings
		 else return ();
		case whichmonad of
			{
			GCPSWhichMonad -> 
			 let {?objType = MkType::Type (CompleteObject IORef (GCPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = matchSecureRead (ioRead ["."]) ["init.pure.scm","init.full.scm"]} in
			 case stdbindings of
				{
				FullStdBindings -> runSchemeProgram
				 printResult
				 fullMacroBindings
				 (fullTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings,
					monadGuardBindings,
					evalBindings (lift . lift),
					eqBindings,
					setBindings,
					portBindings,
					systemBindings (lift . lift)
					])
				 "init.full.scm" source;
				PureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings,
					monadGuardBindings,
					evalBindings (lift . lift),
					portBindings
					])
				 "init.pure.scm" source;
				StrictPureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings
					])
				 "init.pure.scm" source;
				};
			CPSWhichMonad -> 
			 let {?objType = MkType::Type (CompleteObject IORef (CPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = matchSecureRead (ioRead ["."]) ["init.pure.scm","init.full.scm"]} in
			 case stdbindings of
				{
				FullStdBindings -> runSchemeProgram
				 printResult
				 fullMacroBindings
				 (fullTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings,
					evalBindings lift,
					eqBindings,
					setBindings,
					portBindings,
					systemBindings lift
					])
				 "init.full.scm" source;
				PureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings,
					evalBindings lift,
					portBindings
					])
				 "init.pure.scm" source;
				StrictPureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					monadContBindings
					])
				 "init.pure.scm" source;
				};
			IOWhichMonad -> 
			 let {?objType = MkType::Type (CompleteObject IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = matchSecureRead (ioRead ["."]) ["init.pure.scm","init.full.scm"]} in
			 case stdbindings of
				{
				FullStdBindings -> runSchemeProgram
				 printResult
				 fullMacroBindings
				 (fullTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					evalBindings id,
					eqBindings,
					setBindings,
					portBindings,
					systemBindings id
					])
				 "init.full.scm" source;
				PureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					evalBindings id,
					portBindings
					])
				 "init.pure.scm" source;
				StrictPureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings
					])
				 "init.pure.scm" source;
				};
			IdentityWhichMonad -> 
			 let {?objType = MkType::Type (CompleteObject IdentityConst Identity)} in
			 let {?binder = recursiveBinder} in
			 let {?read = matchSecureRead (ioRead ["."]) ["init.pure.scm","init.full.scm"]} in
			 case stdbindings of
				{
				FullStdBindings -> fail "can't use pure monad with full bindings";
				PureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings,
					portBindings
					])
				 "init.pure.scm" source;
				StrictPureStdBindings -> runSchemeProgram
				 printResult
				 pureMacroBindings
				 (pureTopLevelBindings ++ (loadTopLevelBindings readLoad))
				 (concatenateList
					[
					baseBindings,
					monadFixBindings
					])
				 "init.pure.scm" source;
				};
			};
		});
	}
