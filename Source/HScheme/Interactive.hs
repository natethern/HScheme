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

module Org.Org.Semantic.HScheme.Interactive where
	{
	import Org.Org.Semantic.HScheme.SystemInterface;
	import Org.Org.Semantic.HScheme.FMapBindings;
	import Org.Org.Semantic.HScheme.FullStandardBindings;
	import Org.Org.Semantic.HScheme.SExpParser;
	import Org.Org.Semantic.HScheme.StandardBindings;
	import Org.Org.Semantic.HScheme.Bindings;
	import Org.Org.Semantic.HScheme.PortProcedures;
	import Org.Org.Semantic.HScheme.Procedures;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.InputPortParser;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	reportError :: (Scheme m r,?refType :: Type (r ())) =>
	 OutputPort Word8 m -> Object r m -> m ();
	reportError errPort error = do
		{
		text <- toString error;
		opWriteList errPort (encodeUTF8 ("error: "++text++"\n"));
		opFlush errPort;
		};

	interactiveLoop ::
		(
		Scheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?refType :: Type (r ())
		) =>
	 FullSystemInterface m r -> Bindings r m -> m ();
	interactiveLoop fsi bindings = do
		{
		let {input = trapEOT (parseUTF8Char (ipRead (fsiCurrentInputPort fsi)));};
		mbindings' <- catch (catchBottom (do
			{
			opWriteList (fsiCurrentOutputPort fsi) (encodeUTF8 "hscheme> ");
			opFlush (fsiCurrentOutputPort fsi);
			mobject <- parseFromCharSource input;
			case mobject of
				{
				Nothing -> return Nothing;
				Just obj -> do
					{
					bindings' <- printeval (fsiCurrentOutputPort fsi) bindings obj;
					return (Just bindings');
					};
				};
			}) (\ex -> do
				{
				runParser input restOfLineParse;
				errObj <- getConvert (MkSymbol "failure",MkSList (show ex));
				reportError (fsiCurrentErrorPort fsi) errObj;
				return (Just bindings);
				})
			)
			(\error -> do
			{
			runParser input restOfLineParse;
			reportError (fsiCurrentErrorPort fsi) error;
			return (Just bindings);
			});

		case mbindings' of
			{
			Just bindings' -> interactiveLoop fsi bindings';
			Nothing -> return ();
			};
		};

	interact ::
		(
		Scheme m r,
		MonadBottom m,
		MonadException (Object r m) m,
		?refType :: Type (r ())
		) =>
	 FullSystemInterface m r ->
	 Bindings r m ->
	 String ->
	 m ();
	interact fsi bindings filename = catch (do
		{
		bindings' <- psiLoadBindings (fsiPure fsi) bindings filename;
		interactiveLoop fsi bindings';
		})
		(\error -> do
		{
		reportError (fsiCurrentErrorPort fsi) error;
		});

	interactWithExit ::
		(
		Scheme m r,
		MonadCont m,
		MonadBottom m,
		MonadException (Object r m) m,
		?refType :: Type (r ())
		) =>
	 FullSystemInterface m r ->
	 Bindings r m ->
	 String ->
	 m ();
	interactWithExit fsi rootBindings filename = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;
		bindings' <- catch (psiLoadBindings (fsiPure fsi) bindings filename)
			(\error -> do
			{
			reportError (fsiCurrentErrorPort fsi) error;
			exitFunc ();
			});
		interactiveLoop fsi bindings';
		});

	strictPureInteract ::
		(
		Scheme m r,
		MonadFix m,
		MonadBottom m,
		MonadException (Object r m) m
		) =>
	 FullSystemInterface m r -> m ();
	strictPureInteract fsi = do
		{
		bindings <- chainList
			[
			monadFixStrictPureBindings
			] emptyBindings;
		let {?refType = Type} in
		 interact fsi bindings "init.pure.scm";
		};

	pureInteract ::
		(
		Scheme m r,
		MonadFix m,
		MonadBottom m,
		MonadException (Object r m) m
		) =>
	 FullSystemInterface m r -> m ();
	pureInteract fsi = do
		{
		bindings <- chainList
			[
			monadFixPureBindings,
			pureSystemBindings (fsiPure fsi)
			] emptyBindings;
		let {?refType = Type} in
		 interact fsi bindings "init.pure.scm";
		};

	fullInteract ::
		(
		FullScheme m r,
		MonadCont m,
		MonadBottom m,
		MonadException (Object r m) m
		) =>
	 FullSystemInterface m r -> m ();
	fullInteract fsi = do
		{
		bindings <- chainList
			[
			monadContFullBindings,
			fullSystemBindings fsi
			] emptyBindings;
		let {?refType = Type} in
		 interactWithExit fsi bindings "init.full.scm";
		};
	}
