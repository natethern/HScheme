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
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.MacroLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase.Encoding.URI;
	import Org.Org.Semantic.HBase;

	paramsBindings :: (ObjectSubtype r obj (SList Word8),Build cm r) =>
	 ([Word8] -> Bool) -> QueryParameters -> RefBindings cm r obj;
	paramsBindings match (MkQueryParameters list) = concatenateList (fmap paramBinding list) where
		{
		paramBinding :: (ObjectSubtype r obj (SList Word8),Build cm r) =>
		 ([Word8],[Word8]) -> RefBindings cm r obj;
		paramBinding (name,value) | match name = \b -> do
			{
			valueObj <- getObject (MkSList value);
			addLocationBinding (MkSymbol (decodeLatin1 name)) valueObj b;
			};
		paramBinding _ = nothing;
		};

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

	runSchemeProgram ::
		(
		ParseObject r obj,
		ParserError IO obj,
		InterpretObject m r obj,
		AssembleError IO obj,
		RunnableScheme IO m r obj,
		?objType :: Type obj,
		?read :: String -> IO [obj],
		?binder :: TopLevelBinder r obj m
		) =>
	 (obj -> IO ()) ->
	 MacroBindings IO r obj m ->
	 ((?syntacticbindings :: SymbolBindings (Syntax r obj m)) => TopLevelBindings IO r obj m) ->
	 ((?macrobindings :: Symbol -> Maybe (Macro IO r obj m),?toplevelbindings :: Symbol -> Maybe (TopLevelMacro IO r obj m)) => RefBindings IO r obj) ->
	 String ->
	 String ->
	 IO ();
	runSchemeProgram outproc mcBindings tlBindings runBindings initfilename source =
	 mutualBind mcBindings tlBindings (do
		{
		bindings <- runBindings emptyBindings;
		initCommand <- readLoad initfilename;
		progObjects <- parseAllFromString source;
		runObjects outproc initCommand progObjects bindings;
		});

	cgiRunProgram ::
	 (forall a. (Show a,?stdout :: FlushSink IO Word8,?stderr :: FlushSink IO Word8) => a -> IO ()) ->
	 ((?stdin :: PeekSource IO (Maybe Word8),?stdout :: FlushSink IO Word8,?getEnvVar :: String -> IO (Maybe String)) => IO ()) -> IO ();
	cgiRunProgram errproc prog = ioRunProgram (catchBottom (catchSingle prog errproc) errproc);
	}
