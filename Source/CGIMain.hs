-- This is written in Haskell.
{--
SoupServer -- RDF-based information system
Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>

This file is part of SoupServer.

SoupServer is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

SoupServer is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SoupServer; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--}

module Main where
	{
	import Org.Org.Semantic.HScheme;
	import Org.Org.Semantic.HBase.Encoding.URI;
	import Org.Org.Semantic.HBase.Protocol.CGI;
	import Org.Org.Semantic.HBase;
	import System.Exit;

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

	reportError ::
		(
		Build IO r,
		?objType :: Type (Object r m),
		?stderr :: FlushSink IO Word8
		) =>
	 Object r m -> IO ();
	reportError obj = do
		{
		text <- toString obj;
		fsSinkList ?stderr (encodeUTF8 ("error: "++text++"\n"));
		fsFlush ?stderr;
		exitFailure;
		};

	printError :: (Show a) =>
	 a -> IO ();
	printError a = putStrLn ("Error: "++ (show a)++"");

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
	 ((?toplevelbindings :: Bindings Symbol (TopLevelMacro IO r m)) => Bindings Symbol (Macro IO r m) -> Bindings Symbol (Macro IO r m)) ->
	 ((?toplevelbindings :: Bindings Symbol (TopLevelMacro IO r m)) => Bindings Symbol (TopLevelMacro IO r m) -> Bindings Symbol (TopLevelMacro IO r m)) ->
	 (Bindings Symbol (ObjLocation r m) -> IO (Bindings Symbol (ObjLocation r m))) ->
	 String ->
	 String ->
	 IO ();
	runProg macroBindings tlBindings runBindings initfilename source =
	 mutualBind macroBindings tlBindings (do
		{
		bindings <- runBindings emptyBindings;
		initObjects <- readFiles [initfilename];
		progObjects <- parseAllFromString source;
		runObjects printResult reportError (initObjects ++ progObjects) bindings;
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
			 let {?objType = Type::Type (Object IORef (CPS IORef))} in
			 let {?binder = setBinder} in
			 let {?read = ioRead ["."]} in
			 if (isFull params)
			 then runProg
			  fullMacroBindings fullTopLevelBindings monadContFullBindings
			  "init.full.scm" source
			 else runProg
			  pureMacroBindings pureTopLevelBindings monadContPureBindings
			  "init.pure.scm" source;
			_ -> 
			 let {?objType = Type::Type (Object IORef IO)} in
			 let {?binder = setBinder} in
			 let {?read = ioRead ["."]} in
			 if (isFull params)
			 then runProg
			  fullMacroBindings fullTopLevelBindings monadFixFullBindings
			  "init.full.scm" source
			 else runProg
			  pureMacroBindings pureTopLevelBindings monadFixPureBindings
			  "init.pure.scm" source;
			};
		})
		printError);
	}
