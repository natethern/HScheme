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
	import Org.Org.Semantic.HBase.Encoding.URI;
	import Org.Org.Semantic.HBase.Protocol.CGI;
	import Org.Org.Semantic.HBase;

	type CPS r = SchemeCPS r (IO ());

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

	printError ::
		(
		Show a,
		?stderr :: FlushSink IO Word8
		) =>
	 a -> IO ();
	printError a = do
		{
		fsSinkList ?stderr (encodeUTF8 ("Error: "++(show a)++"\n"));
		fsFlush ?stderr;
		};

	isFull :: QueryParameters -> Bool;
	isFull params = case findQueryParameter (encodeLatin1 "flavour") params of
		{
		Just [0x70,0x75,0x72,0x65] -> False;
		_ -> True;
		};

	runProg ::
		(
		RunnableScheme IO m r,
		?objType :: Type (Object r m),
		?read :: String -> IO [Object r m],
		?binder :: TopLevelBinder r m,
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 MacroBindings IO r m ->
	 ((?syntacticbindings :: Bindings Symbol (Syntax r (Object r m))) => TopLevelBindings IO r m) ->
	 LocationBindings IO r m ->
	 String ->
	 String ->
	 IO ();
	runProg macroBindings tlBindings runBindings initfilename source =
	 mutualBind macroBindings tlBindings (do
		{
		bindings <- runBindings emptyBindings;
		initObjects <- readFiles [initfilename];
		progObjects <- parseAllFromString source;
		runObjects printResult (initObjects ++ progObjects) bindings;
		});

	main :: IO ();
	main = ioRunProgram (catchSingle (do
		{
		putStrLn "Content-Type: text/plain\n";
		params <- cgiGetQueryParameters;
		source <- case findQueryParameter (encodeLatin1 "input") params of
			{
			Just sourceBytes -> return (decodeLatin1 sourceBytes);
			Nothing -> fail "no input";
			};
		case findQueryParameter (encodeLatin1 "monad") params of
			{
			Just [0x63,0x6F,0x6E,0x74] -> 
			 let {?objType = MkType::Type (Object IORef (CPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = ioRead ["."]} in
			 if (isFull params)
			 then runProg
			  fullMacroBindings
			  (fullTopLevelBindings ++ (loadTopLevelBindings (matchSecureLoad readLoad ["init.pure.scm","init.full.scm"])))
			  monadContFullBindings
			  "init.full.scm" source
			 else runProg
			  pureMacroBindings
			  (pureTopLevelBindings ++ (loadTopLevelBindings (matchSecureLoad readLoad ["init.pure.scm","init.full.scm"])))
			  monadContPureBindings
			  "init.pure.scm" source;
			_ -> 
			 let {?objType = MkType::Type (Object IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = ioRead ["."]} in
			 if (isFull params)
			 then runProg
			  fullMacroBindings
			  (fullTopLevelBindings ++ (loadTopLevelBindings (matchSecureLoad readLoad ["init.pure.scm","init.full.scm"])))
			  monadFixFullBindings
			  "init.full.scm" source
			 else runProg
			  pureMacroBindings
			  (pureTopLevelBindings ++ (loadTopLevelBindings (matchSecureLoad readLoad ["init.pure.scm","init.full.scm"])))
			  monadFixPureBindings
			  "init.pure.scm" source;
			};
		})
		printError);
	}
