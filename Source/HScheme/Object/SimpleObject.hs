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

module Org.Org.Semantic.HScheme.Object.SimpleObject(SimpleObject) where
	{
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	data SimpleObject r m =
	 NilObject												|
	 PairObject			(r (SimpleObject r m)) (r (SimpleObject r m))	|
	 SymbolObject		Symbol								|
	 ProcedureObject	(Procedure (SimpleObject r m) m)			|
	 NumberObject		Number								|
	 StringObject		(SRefArray r Char)					;

	instance ListObject ref (Object ref m) where
		{
		objectCell NilObject = Just Nothing;
		objectCell (PairObject hr tr) = Just (Just (hr,tr));
		objectCell _ = Nothing;
		pairObject = PairObject;
		nilObject = NilObject;
		};

	
	-- Object

	instance ObjectSubtype r (Object r m) (Object r m) where
		{
		getObject = return;
		resultFromObject = return . return;
		};

	
	-- Bool

	instance MaybeA Bool (Object r m) where
		{
		maybeConvert = Just . convert;
		};

	instance IsA Bool (Object r m) where
		{
		convert NilObject = False;
		convert (NumberObject n) = not (isZero n);
		
		-- everything apart from () and 0 is True
		convert _ = True;
		};

	instance ObjectSubtype r (Object r m) Bool where
		{
		getObject = return . BooleanObject;
		resultFromObject obj = return (return (convert obj));
		};

	
	-- Char

	instance MaybeA Char (Object r m) where
		{
		maybeConvert (CharObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Char where
		{
		getObject = return . CharObject;
		resultFromObject = getMaybeToMatch CharTypeExpected (return . maybeConvert);
		};

	
	-- Number

	instance MaybeA Number (Object r m) where
		{
		maybeConvert (NumberObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Number where
		{
		getObject = return . NumberObject;
		resultFromObject = getMaybeToMatch NumberTypeExpected (return . maybeConvert);
		};


	-- EIReal

	instance ObjectSubtype r (Object r m) EIReal where
		{
		getObject i = getObject (convert i :: Number);
		resultFromObject obj = do
			{
			mn <- resultFromObject obj;
			return (do
				{
				(n :: Number) <- mn;
				maybeToMatch RealTypeExpected obj (maybeApproximate n);
				});
			};
		};


	-- Rational

	instance ObjectSubtype r (Object r m) Rational where
		{
		getObject i = getObject (convert i :: EIReal);
		resultFromObject obj = do
			{
			mn <- resultFromObject obj;
			return (do
				{
				(n :: EIReal) <- mn;
				maybeToMatch RationalTypeExpected obj (maybeApproximate n);
				});
			};
		};


	-- Integer

	instance ObjectSubtype r (Object r m) Integer where
		{
		getObject i = getObject (convert i :: EIReal);
		resultFromObject obj = do
			{
			mn <- resultFromObject obj;
			return (do
				{
				(n :: EIReal) <- mn;
				maybeToMatch IntegerTypeExpected obj (maybeApproximate n);
				});
			};
		};


	-- Int

	instance ObjectSubtype r (Object r m) Int where
		{
		getObject i = getObject (convert i :: Integer);
		resultFromObject obj = do
			{
			mn <- resultFromObject obj;
			return (do
				{
				(n :: Integer) <- mn;
				maybeToMatch IntTypeExpected obj (maybeConvert n);
				});
			};
		};


	-- Word8

	instance ObjectSubtype r (Object r m) Word8 where
		{
		getObject i = getObject (convert i :: Integer);
		resultFromObject obj = do
			{
			mn <- resultFromObject obj;
			return (do
				{
				(n :: Integer) <- mn;
				maybeToMatch Word8TypeExpected obj (maybeConvert n);
				});
			};
		};

	
	-- Symbol

	instance MaybeA Symbol (Object r m) where
		{
		maybeConvert (SymbolObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) Symbol where
		{
		getObject = return . SymbolObject;
		resultFromObject = getMaybeToMatch SymbolTypeExpected (return . maybeConvert);
		};


	-- SRefArray r Char

	instance MaybeA (SRefArray r Char) (Object r m) where
		{
		maybeConvert (StringObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r Char) where
		{
		getObject = return . StringObject;
		resultFromObject = getMaybeToMatch StringTypeExpected (return . maybeConvert);
		};


	-- SRefList r t

	instance (MaybeA (SRefArray r t) (Object r m)) =>
	 MaybeA (SRefList r t) (Object r m) where
		{
		maybeConvert obj = do
			{
			(arr :: SRefArray r t) <- maybeConvert obj;
			return (MkSRefList (toList arr));
			};
		};

	instance (MaybeA (SRefArray r t) (Object r m),ObjectSubtype r (Object r m) (SRefArray r t)) =>
	 ObjectSubtype r (Object r m) (SRefList r t) where
		{
		getObject (MkSRefList rs) = getObject (fromList rs :: SRefArray r t);
		resultFromObject obj = do
			{
			marr <- resultFromObject obj;
			return (do
				{
				(arr :: SRefArray r t) <- marr;
				return (MkSRefList (toList arr));
				});
			};
		};


	-- SList t

	getSRefArrayList :: (Build cm r) =>
	 SRefArray r a -> cm [a];
	getSRefArrayList rarray = readRList (toList rarray) where
		{
		readRList [] = return [];
		readRList (r:rs) = do
			{
			c <- get r;
			cs <- readRList rs;
			return (c:cs);
			};
		};

	makeSRefArray :: (Build cm r) =>
	 [a] -> cm (SRefArray r a);
	makeSRefArray list = do
		{
		rlist <- getRList list;
		return (fromList rlist);
		} where
		{
		getRList [] = return [];
		getRList (c:cs) = do
			{
			r <- new c;
			rs <- getRList cs;
			return (r:rs);
			};
		};

	instance (ObjectSubtype r (Object r m) (SRefArray r t)) =>
	 ObjectSubtype r (Object r m) (SList t) where
		{
		getObject (MkSList list) = do
			{
			(arr :: SRefArray r t) <- makeSRefArray list;
			getObject arr;
			};

		resultFromObject obj = do
			{
			mrarray <- resultFromObject obj;
			case mrarray of
				{
				SuccessResult (rarray :: SRefArray r t) -> do
					{
					slist <- getSRefArrayList rarray;
					return (return (MkSList slist));
					};
				ExceptionResult ex -> return (ExceptionResult ex);
				};
			};
		};

	
	-- Procedure

	instance MaybeA (Procedure (Object r m) m) (Object r m) where
		{
		maybeConvert (ProcedureObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (Procedure (Object r m) m) where
		{
		getObject = return . ProcedureObject;
		resultFromObject = getMaybeToMatch ProcedureTypeExpected (return . maybeConvert);
		};

	
	-- Mismatch

	instance (Build cm r) =>
	 MonadIsA cm (Object r m) (Mismatch (Object r m)) where
		{
		getConvert (MkMismatch exp obj) = do
			{
			expected <- getObject (MkSList (show exp));
			makeList [expected,obj];
			};
		};

	instance
		(
		Build cm r,
		ObjectSubtype r obj obj,
		ObjectSubtype r obj Symbol,
		ObjectSubtype r obj Int,
		MonadIsA cm obj (Mismatch obj)
		) =>
	 MonadIsA cm obj (ArgumentMismatch obj) where
		{
		getConvert (UnionArgMismatch am1 am2) = do
			{
			am1Obj <- getConvert am1;
			am2Obj <- getConvert am2;
			makeList [am1Obj,am2Obj];
			};
		getConvert (TooFewArguments pos) = getObject (MkSymbol "too-few-arguments",(pos,()));
		getConvert (TooManyArguments pos list) = getObject (MkSymbol "too-few-arguments",(pos,(list,())));
		getConvert (WrongArgumentType pos mm) = do
			{
			(mObj :: obj) <- getConvert mm;
			getObject (MkSymbol "wrong-argument-type",(pos,(mObj,())))
			};
		};

	
	-- InterpretObject

	instance (Build m r,MonadThrow (Object r m) m) =>
	 RunError (Object r m) m where
		{
		throwWrongContextError		= throwArgError "single-value-expected";
		throwTooFewArgumentsError	= throwSimpleError "too-few-args";
		throwTooManyArgumentsError	= throwArgError "too-many-args";
		throwBadCombinationError t (MkMismatch exp obj) =  do
			{
			expobj <- getObject (MkSList (show exp));
			throwArgError "bad-combination" [t,expobj,obj];
			};
		throwBadApplyFormError obj = throwArgError "bad-apply-form" [obj];
		throwMismatchError = throwMismatchObjError;
		throwArgumentListMismatchError am = do
			{
			amObj <- getConvert am;
			throwArgError "arg-list-mismatch" [amObj];
			};
		};
	}
