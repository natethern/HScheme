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
	import Org.Org.Semantic.HScheme.Procedures;
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.InputPortParser;
	import Org.Org.Semantic.HScheme.Port;
	import Org.Org.Semantic.HBase;

	reportError :: (Scheme m r,Show x) =>
	 Type (r ()) -> OutputPort Char m -> x -> m ();
	reportError t errPort error = do
		{
		opWriteStrLn errPort ("error: "++(show error));
		opFlush errPort;
		};

	interactiveLoop ::
		(
		Scheme m r,
		MonadBottom m,
		MonadSingleException x m,
		Show x
		) =>
	 Type (r ()) -> FullSystemInterface m r -> Bindings r m -> m ();
	interactiveLoop t fsi bindings = do
		{
		let {input = trapEOT (fsiCurrentInputPort fsi);};
		mbindings' <- catchSingle (catchBottom (do
			{
			opWriteList (fsiCurrentOutputPort fsi) "hscheme> ";
			opFlush (fsiCurrentOutputPort fsi);
			mobject <- portRead input;
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
				reportError t (fsiCurrentErrorPort fsi) ex;
				return (Just bindings);
				})
			)
			(\error -> do
			{
			runParser input restOfLineParse;
			reportError t (fsiCurrentErrorPort fsi) error;
			return (Just bindings);
			});

		case mbindings' of
			{
			Just bindings' -> interactiveLoop t fsi bindings';
			Nothing -> return ();
			};
		};

	interact ::
		(
		Scheme m r,
		MonadCont m,
		MonadBottom m,
		MonadSingleException x m,
		Show x
		) =>
	 Type (r ()) ->
	 FullSystemInterface m r ->
	 Bindings r m ->
	 String ->
	 m ();
	interact t fsi rootBindings filename = callCC (\exitFunc -> do
		{
		bindings <- chainList
			[
			addProcBinding "exit" (exitFuncProc exitFunc)
			] rootBindings;
		bindings' <- catchSingle (psiLoadBindings (fsiPure fsi) bindings filename)
			(\error -> do
			{
			reportError t (fsiCurrentErrorPort fsi) error;
			exitFunc ();
			});
		interactiveLoop t fsi bindings';
		});

	safePureInteract ::
		(
		Scheme m r,
		MonadBottom m,
		MonadSingleException x m,
		Show x
		) =>
	 FullSystemInterface m r -> m ();
	safePureInteract fsi = do
		{
		bindings <- chainList
			[
			stdBindings
			] emptyBindings;
		catchSingle (do
			{
			bindings' <- psiLoadBindings (fsiPure fsi) bindings "Prelude.pure.scm";
			interactiveLoop t fsi bindings';
			})
			(\error -> do
			{
			reportError t (fsiCurrentErrorPort fsi) error;
			});
		} where
		{
		foo :: FullSystemInterface m r -> Type (r ());
		foo _ =Type;
		t = foo fsi
		};

	pureInteract ::
		(
		Scheme m r,
		MonadCont m,
		MonadBottom m,
		MonadSingleException x m,
		Show x
		) =>
	 FullSystemInterface m r -> m ();
	pureInteract fsi = do
		{
		bindings <- chainList
			[
			monadicStdBindings,
			pureSystemBindings (fsiPure fsi)
			] emptyBindings;
		interact Type fsi bindings  "Prelude.pure.scm";
		};

	fullInteract ::
		(
		FullScheme m r,
		MonadCont m,
		MonadBottom m,
		MonadSingleException x m,
		Show x
		) =>
	 FullSystemInterface m r -> m ();
	fullInteract fsi = do
		{
		bindings <- chainList
			[
			fullStdBindings,
			fullSystemBindings fsi
			] emptyBindings;
		interact Type fsi bindings "Prelude.full.scm";
		};
	}
