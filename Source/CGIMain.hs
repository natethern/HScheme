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

	type CPS r = SchemeCPS r (IO ());

	printError :: (Show a) =>
	 a -> IO ();
	printError a = putStrLn ("Error: "++ (show a)++"");

	fixPureInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	fixPureInterpret source = let {?refType = ioConstType} in do
		{
		bindings <- concatenateList
			[
			monadFixPureBindings,
			pureSystemBindings (ioPureSystemInterface id ["."])
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface id ["."]) bindings "init.pure.scm";
		parseEvalFromString (printResult stdOutputPort) bindings' source;
		return ();
		};

	contPureInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> CPS IOConst ();
	contPureInterpret source = let {?refType = ioConstType} in do
		{
		bindings <- concatenateList
			[
			monadContPureBindings,
			pureSystemBindings (ioPureSystemInterface (lift . lift) ["."])
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface (lift . lift) ["."]) bindings "init.pure.scm";
		parseEvalFromString (printResult (remonadOutputPort (lift . lift) stdOutputPort)) bindings' source;
		return ();
		};

	fixFullInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> IO ();
	fixFullInterpret source = let {?refType = ioRefType} in do
		{
		bindings <- concatenateList
			[
			monadFixFullBindings,
			pureSystemBindings (ioQuietPureSystemInterface id ["."])
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface id ["."]) bindings "init.full.scm";
		parseEvalFromString (printResult stdOutputPort) bindings' source;
		return ();
		};

	contFullInterpret ::
		(
		?stdout :: FlushSink IO Word8,
		?stderr :: FlushSink IO Word8
		) =>
	 String -> CPS IORef ();
	contFullInterpret source = let {?refType = ioRefType} in do
		{
		bindings <- concatenateList
			[
			monadContFullBindings,
			pureSystemBindings (ioQuietPureSystemInterface (lift . lift) ["."])
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface (lift . lift) ["."]) bindings "init.full.scm";
		parseEvalFromString (printResult (remonadOutputPort (lift . lift) stdOutputPort)) bindings' source;
		return ();
		};

	isFull :: QueryParameters -> Bool;
	isFull params = case findQueryParameter (encodeLatin1 "flavour") params of
		{
		Just [0x70,0x75,0x72,0x65] -> False;
		_ -> True;
		};

	runCPS ::
	 CPS r () -> IO ();
	runCPS cps = runContinuationPass (fail . show) return cps;

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
			 then runCPS (contFullInterpret source)
			 else runCPS (contPureInterpret source);
			_ -> if (isFull params)
			 then fixFullInterpret source
			 else fixPureInterpret source;
			};
		})
		printError);
	}
