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

module Org.Org.Semantic.HScheme.Compile where
	{
	import Org.Org.Semantic.HScheme.Conversions;
	import Org.Org.Semantic.HScheme.Object;
	import Org.Org.Semantic.HScheme.SymbolExpression;
	import Org.Org.Semantic.HScheme.FunctorLambda;
	import Org.Org.Semantic.HBase;

	type SchemeExpression r m = SymbolExpression Symbol (m (ObjLocation r m));

	type Macro cm r m = [Object r m] -> cm (SchemeExpression r m (m (Object r m)));
	type Syntax cm r m = [Object r m] -> cm (Object r m);

	-- as fProductList
	execList :: (Monad m) =>
	 [m a] -> m [a];
	execList [] = return [];
	execList (ma:mas) = do
		{
		a <- ma;
		as <- execList mas;
		return (a:as);
		};

	doApply :: (Scheme m r) =>
	 m (Object r m) ->
	 m [Object r m] ->
	 m (Object r m);
	doApply mf margs = do
		{
		f <- mf;
		case f of
			{
			ProcedureObject proc -> do
				{
				args <- margs;
				proc args;
				};
			_ -> fail "bad-apply-form";
			};
		};

	makeApply :: (Scheme m r) =>
	 SchemeExpression r m (m (Object r m)) ->
	 [SchemeExpression r m (m (Object r m))] ->
	 SchemeExpression r m (m (Object r m));
	makeApply f args = fApply (fmap doApply f) (fmap execList (fExtract args));

	compileApply ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> [Object r m] -> cm (SchemeExpression r m (m (Object r m)));
	compileApply f arglist = do
		{
		fe <- compileExpression f;
		ae <- sinkList compileExpression arglist;
		return (makeApply fe ae);
		};

	compileExpression ::
		(
		Build cm r,
		Scheme m r,
		?syntacticbindings :: Binds Symbol (Syntax cm r m),
		?macrobindings :: Binds Symbol (Macro cm r m)
		) =>
	 Object r m -> cm (SchemeExpression r m (m (Object r m)));
	compileExpression (SymbolObject sym) = return (fmap (\mloc -> do
		{
		loc <- mloc;
		get loc;
		}) (fSymbol sym));
	compileExpression (PairObject head tail) = do
		{
		h <- get head;
		t <- get tail;
		marglist <- getMaybeConvert t;
		case marglist of
			{
			Nothing -> fail "not an argument list";
			Just arglist -> case h of
				{
				SymbolObject sym -> case getBinding ?syntacticbindings sym of
					{
					Just syntax -> do
						{
						obj <- syntax arglist;
						compileExpression obj;
						};
					Nothing -> case getBinding ?macrobindings sym of
						{
						Just macro -> macro arglist;
						Nothing -> compileApply h arglist;
						};
					};
				_ -> compileApply h arglist;
				};
			};
		};
	compileExpression a = case a of
		{
		BooleanObject _ -> return (return' (return a));
		NumberObject _ -> return (return' (return a));
		CharObject _ -> return (return' (return a));
		StringObject _ -> return (return' (return a));
		ByteArrayObject _ -> return (return' (return a));
		_ -> fail "can't evaluate form" -- throwArgError "cant-evaluate-form" [a];
		};
	}
