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
	import Org.Org.Semantic.HBase;
	import System.Environment;
	import Numeric;

	type M = ShowExceptionMonad IO;

	type ConstantRef = Constant M;

	type CPS r = SchemeCPS r (M ());

	instance Show (SchemeCPSError r (M ())) where
		{
		show (StringError s) = s;
		show (ExceptionError ex) = show ex;
		show (ObjError ex) = "Scheme object error";
		};

	printError :: (Show a) =>
	 a -> IO ();
	printError a = putStrLn ("Error: "++ (show a)++"");

	fixPureInterpret ::
	 String -> IO ();
	fixPureInterpret source = do
		{
		(bindings :: Bindings (Constant IO) m) <- chainList
			[
			monadFixPureBindings,
			pureSystemBindings (ioPureSystemInterface id)
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface id) bindings "Prelude.pure.scm";
		parseEvalFromString (printResult stdOutputPort) bindings' source;
		return ();
		};

	contPureInterpret ::
	 String -> CPS ConstantRef ();
	contPureInterpret source = do
		{
		(bindings :: Bindings ConstantRef (CPS ConstantRef)) <- chainList
			[
			monadContPureBindings,
			pureSystemBindings (ioPureSystemInterface (lift . lift))
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface (lift . lift)) bindings "Prelude.pure.scm";
		parseEvalFromString (printResult (remonadOutputPort (lift . lift) stdOutputPort)) bindings' source;
		return ();
		};

	fixFullInterpret ::
	 String -> IO ();
	fixFullInterpret source = do
		{
		(bindings :: Bindings IORef m) <- chainList
			[
			monadFixFullBindings,
			pureSystemBindings (ioQuietPureSystemInterface id)
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface id) bindings "Prelude.full.scm";
		parseEvalFromString (printResult stdOutputPort) bindings' source;
		return ();
		};

	contFullInterpret ::
	 String -> CPS IORef ();
	contFullInterpret source = do
		{
		(bindings :: Bindings IORef (CPS IORef)) <- chainList
			[
			monadContFullBindings,
			pureSystemBindings (ioQuietPureSystemInterface (lift . lift))
			] emptyBindings;
		bindings' <- psiLoadBindings (ioQuietPureSystemInterface (lift . lift)) bindings "Prelude.full.scm";
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
	 CPS r () -> M ();
	runCPS cps = runContinuationPass (\s -> throwX (MkShowException (show s))) return cps;

	main :: IO ();
	main = catch (do
		{
		putStrLn "Content-Type: text/plain\n";
		xio <- unExceptionMonad (do
			{
			text <- lift (getHandleContents stdin);
			params <- mapShowException (readParseX text);
			source <- case findQueryParameter (encodeLatin1 "input") params of
				{
				Just sourceBytes -> return (decodeLatin1 sourceBytes);
				Nothing -> throwX (MkShowException "no input");
				};
			case findQueryParameter (encodeLatin1 "monad") params of
				{
				Just [0x63,0x6F,0x6E,0x74] -> if (isFull params)
				 then runCPS (contFullInterpret source)
				 else runCPS (contPureInterpret source);
				_ -> if (isFull params)
				 then lift (fixFullInterpret source)
				 else lift (fixPureInterpret source);
				};
			});
		case xio of
			{
			ExceptionExceptionResult s -> printError s;
			_ -> return ();
			};
		}) (\(err :: IOException) -> printError err);
	}
