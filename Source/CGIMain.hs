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

	fixPureInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	fixPureInterpret source =
	 let {?objType = Type::Type (Object IORef IO)} in
	 let {?binder = setBinder} in
	 let {?read = ioRead ["."]} in
	 mutualBind pureMacroBindings pureTopLevelBindings (do
		{
		bindings <- monadFixPureBindings emptyBindings;
		initObjects <- readFiles ["init.pure.scm"];
		progObjects <- parseAllFromString source;
		runObjects printResult reportError (initObjects ++ progObjects) bindings;
		});

	fixFullInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	fixFullInterpret source =
	 let {?objType = Type::Type (Object IORef IO)} in
	 let {?binder = setBinder} in
	 let {?read = ioRead ["."]} in
	 mutualBind fullMacroBindings fullTopLevelBindings (do
		{
		bindings <- monadFixFullBindings emptyBindings;
		initObjects <- readFiles ["init.full.scm"];
		progObjects <- parseAllFromString source;
		runObjects printResult reportError (initObjects ++ progObjects) bindings;
		});

	contPureInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	contPureInterpret source =
	 let {?objType = Type::Type (Object IORef (CPS IORef))} in
	 let {?binder = setBinder} in
	 let {?read = ioRead ["."]} in
	 mutualBind pureMacroBindings pureTopLevelBindings (do
		{
		bindings <- monadContPureBindings emptyBindings;
		initObjects <- readFiles ["init.pure.scm"];
		progObjects <- parseAllFromString source;
		runObjects printResult reportError (initObjects ++ progObjects) bindings;
		});

	contFullInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	contFullInterpret source =
	 let {?objType = Type::Type (Object IORef (CPS IORef))} in
	 let {?binder = setBinder} in
	 let {?read = ioRead ["."]} in
	 mutualBind fullMacroBindings fullTopLevelBindings (do
		{
		bindings <- monadContFullBindings emptyBindings;
		initObjects <- readFiles ["init.full.scm"];
		progObjects <- parseAllFromString source;
		runObjects printResult reportError (initObjects ++ progObjects) bindings;
		});

	isFull :: QueryParameters -> Bool;
	isFull params = case findQueryParameter (encodeLatin1 "flavour") params of
		{
		Just [0x70,0x75,0x72,0x65] -> False;
		_ -> True;
		};

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
			Just [0x63,0x6F,0x6E,0x74] -> if (isFull params)
			 then contFullInterpret source
			 else contPureInterpret source;
			_ -> if (isFull params)
			 then fixFullInterpret source
			 else fixPureInterpret source;
			};
		})
		printError);
	}
