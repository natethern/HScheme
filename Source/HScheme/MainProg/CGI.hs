-- This is written in Haskell.
{--
HScheme -- a Scheme interpreter written in Haskell
Copyright (C) 2003 Ashley Yakeley <ashley@semantic.org>

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

module Org.Org.Semantic.HScheme.MainProg.CGI where
	{
	import Org.Org.Semantic.HScheme.MainProg.Batch;
	import Org.Org.Semantic.HScheme.Bind;
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.MacroLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase.Encoding.URI;
	import Org.Org.Semantic.HBase;

	paramsBindings :: (Build cm r,Scheme m r) =>
	 ([Word8] -> Bool) -> QueryParameters -> LocationBindings cm r m;
	paramsBindings match (MkQueryParameters list) = concatenateList (fmap paramBinding list) where
		{
		paramBinding :: (Build cm r,Scheme m r) =>
		 ([Word8],[Word8]) -> LocationBindings cm r m;
		paramBinding (name,value) | match name = \b -> do
			{
			valueObj <- getObject (MkSList value);
			addLocationBinding (MkSymbol (decodeLatin1 name)) valueObj b;
			};
		paramBinding _ = nothing;
		};

	runSchemeProgram ::
		(
		RunnableScheme IO m r,
		?objType :: Type (Object r m),
		?read :: String -> IO [Object r m],
		?binder :: TopLevelBinder r m,
		?stdout :: FlushSink IO Word8
		) =>
	 MacroBindings IO r m ->
	 ((?syntacticbindings :: Bindings Symbol (Syntax r (Object r m))) => TopLevelBindings IO r m) ->
	 ((?macrobindings :: Symbol -> Maybe (Macro IO r m),?toplevelbindings :: Symbol -> Maybe (TopLevelMacro IO r m)) => LocationBindings IO r m) ->
	 String ->
	 String ->
	 IO ();
	runSchemeProgram macroBindings tlBindings runBindings initfilename source =
	 mutualBind macroBindings tlBindings (do
		{
		bindings <- runBindings emptyBindings;
		initObjects <- readFiles [initfilename];
		progObjects <- parseAllFromString source;
		runObjects printResult (initObjects ++ progObjects) bindings;
		}) where
		{
		printResult ::
			(
			Build IO r,
			?objType :: Type (Object r m),
			?stdout :: FlushSink IO Word8
			) =>
		Object r m -> IO ();
		printResult obj = do
			{
			str <- toString obj;
			fsSinkList ?stdout (encodeUTF8 (str ++ "\n"));
			};
		};

	cgiRunProgram ::
	 ((?stdin :: PeekSource IO (Maybe Word8),?stdout :: FlushSink IO Word8,?getEnvVar :: String -> IO (Maybe String)) => IO ()) -> IO ();
	cgiRunProgram prog = ioRunProgram (catchBottom (catchSingle prog printError) printError) where
		{
		printError ::
			(
			Show a,
			?stdout :: FlushSink IO Word8
			) =>
		a -> IO ();
		printError a = do
			{
			fsSinkList ?stdout (encodeUTF8 ("Error: "++(show a)++"\n"));
			fsFlush ?stdout;
			};
		};
	}
