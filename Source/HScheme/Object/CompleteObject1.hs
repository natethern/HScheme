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

module Org.Org.Semantic.HScheme.Object.CompleteObject1 where
	{
	import Org.Org.Semantic.HScheme.RunLib;
	import Org.Org.Semantic.HScheme.Parse;
	import Org.Org.Semantic.HScheme.MacroLib;
	import Org.Org.Semantic.HScheme.Interpret;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	data Object r m =
	 NilObject												|
	 PairObject			(r (Object r m)) (r (Object r m))	|
	 SymbolObject		Symbol								|
	 ProcedureObject	(Procedure (Object r m) m)			|
	 BooleanObject		Bool								|
	 NumberObject		Number								|
	 CharObject			Char								|
	 ByteArrayObject	(SRefArray r Word8)					|
	 StringObject		(SRefArray r Char)					|
	 VoidObject												|
	 VectorObject		(SRefArray r (Object r m))			|
	 InputPortObject 	(InputPort Word8 m)					|
	 OutputPortObject 	(OutputPort Word8 m)				|
	 EnvironmentObject	(Environment r (Object r m))		;

	instance ListObject ref (Object ref m) where
		{
		objectCell NilObject = Just Nothing;
		objectCell (PairObject hr tr) = Just (Just (hr,tr));
		objectCell _ = Nothing;
		pairObject = PairObject;
		nilObject = NilObject;
		};

	instance HasBooleanType (Object r m) where
		{
		isBooleanType (BooleanObject _) = True;
		isBooleanType _ = False;
		};
	
	-- VoidObjType

	instance MaybeA VoidObjType (Object r m) where
		{
		maybeConvert VoidObject = Just MkVoidObjType;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) VoidObjType where
		{
		getObject MkVoidObjType = return VoidObject;
		resultFromObject = getMaybeToMatch EOFTypeExpected (return . maybeConvert);
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
		convert (BooleanObject a) = a;
		
		-- everything apart from #f is True
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

	
	-- Environment

	instance MaybeA (Environment r (Object r m)) (Object r m) where
		{
		maybeConvert (EnvironmentObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (Environment r (Object r m)) where
		{
		getObject = return . EnvironmentObject;
		resultFromObject = getMaybeToMatch EnvironmentTypeExpected (return . maybeConvert);
		};

	
	-- InputPort

	instance MaybeA (InputPort Word8 m) (Object r m) where
		{
		maybeConvert (InputPortObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (InputPort Word8 m) where
		{
		getObject = return . InputPortObject;
		resultFromObject = getMaybeToMatch InputPortTypeExpected (return . maybeConvert);
		};

	
	-- OutputPort

	instance MaybeA (OutputPort Word8 m) (Object r m) where
		{
		maybeConvert (OutputPortObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (OutputPort Word8 m) where
		{
		getObject = return . OutputPortObject;
		resultFromObject = getMaybeToMatch OutputPortTypeExpected (return . maybeConvert);
		};


	-- SRefArray r Word8

	instance MaybeA (SRefArray r Word8) (Object r m) where
		{
		maybeConvert (ByteArrayObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r Word8) where
		{
		getObject = return . ByteArrayObject;
		resultFromObject = getMaybeToMatch ByteArrayTypeExpected (return . maybeConvert);
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


	-- SRefArray r (Object r m)

	instance MaybeA (SRefArray r (Object r m)) (Object r m) where
		{
		maybeConvert (VectorObject a) = Just a;
		maybeConvert _ = Nothing;
		};

	instance ObjectSubtype r (Object r m) (SRefArray r (Object r m)) where
		{
		getObject = return . VectorObject;
		resultFromObject = getMaybeToMatch VectorTypeExpected (return . maybeConvert);
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


	-- MapObjects

	instance MapObjects r (Object r m) where
		{
		internalMap map (VectorObject arr) = do
			{
			list' <- for (\loc -> do
				{
				obj <- get loc;
				obj' <- internalMap map obj;
				new obj';
				}) (toList arr);
			return (VectorObject (fromList list'));
			};
		internalMap map (PairObject hloc tloc) = do
			{
			head <- get hloc;
			head' <- internalMap map head;
			tail <- get tloc;
			tail' <- internalMap map tail;
			cons head' tail';
			};
		internalMap map obj = map obj;
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


	-- Equivalence

	instance
--	 (Eq (r (Object r m)),Eq (r Word8),Eq (r Char)) =>
	 (Eq1 r) =>
	 Eq (Object r m) where
		{
		(NumberObject a) == (NumberObject b)	= a == b;
		(PairObject ah at) == (PairObject bh bt)	= (eq1 ah bh) && (eq1 at bt);
		(ByteArrayObject a) == (ByteArrayObject b)	= sameList eq1 (toList a) (toList b);
		(StringObject a) == (StringObject b)	= sameList eq1 (toList a) (toList b);
		(VectorObject a) == (VectorObject b)	= sameList eq1 (toList a) (toList b);
		(BooleanObject a) == (BooleanObject b)	= a == b;
		(SymbolObject a) == (SymbolObject b)	= a == b;
		(CharObject a) == (CharObject b)	= a == b;
		NilObject == NilObject		= True;
		VoidObject == VoidObject	= True;
		_ == _	= False;
		};

	instance
--	 (Eq (r (Object r m)),Eq (r Word8),Eq (r Char)) =>
	 (Eq1 r) =>
	 Eqv (Object r m) where
		{
		eqv (NumberObject a) (NumberObject b) = eqvNumber a b;
		eqv a b = a == b;
		};

	instance (Build cm r,
--		Eq (r (Object r m)),Eq (r Word8),Eq (r Char)
		Eq1 r
		) =>
	 Equal cm (Object r m) where
		{
		equal (NumberObject a) (NumberObject b) = return (equalNumber a b);
		equal (PairObject ah at) (PairObject bh bt) = do
			{
			ahc <- get ah;
			bhc <- get bh;
			s <- equal ahc bhc;
			if s then do
				{
				atc <- get at;
				btc <- get bt;
				equal atc btc;
				} else (return False);
			};
		equal (VectorObject a) (VectorObject b) = sameSRefArray equal a b;
		equal (ByteArrayObject a) (ByteArrayObject b) = sameSRefArray (\a' b' -> return (a' == b')) a b;
		equal (StringObject a) (StringObject b) = sameSRefArray (\a' b' -> return (a' == b')) a b;
		equal a b = return (a == b);
		};


	-- Read & Print

	instance ParseObject r (Object r m) where
		{
		expressionParse = do
			{
			optionalWhitespaceParse;
			(fmap NumberObject numberParse)	|||
			(fmap SymbolObject symbolParse)	|||
			hashLiteralParse		|||
			(quotedParse expressionParse)			|||
			(stringLiteralParse >>= (parserLift . getObject . MkSList))	|||
			(listParse expressionParse)				|||
			((streamEndParse ||| (unexpectedCharError "input")) >> mzero);
			} where
			{
			hashLiteralParse ::
				(
-- MonadThrow (Object r m) cm,
				SchemeParser cm (Object r m) r p,
				?objType :: Type (Object r m)
				) => 
			p (Object r m);
			hashLiteralParse = do
				{
				isTokenParse '#';
				mOrUnexpectedCharError "# literal"
					(
					(fmap BooleanObject booleanHashLiteralParse) |||
					(fmap CharObject characterHashLiteralParse) |||
					(fmap VectorObject vectorHashLiteralParse) |||
					(fmap ByteArrayObject byteArrayHashLiteralParse)
					);
				};
			};
		};

	instance (Build cm r) =>
	 ToString cm (Object r m) where
		{
		toString NilObject				= return "()";
		toString (BooleanObject True)	= return "#t";
		toString (BooleanObject False)	= return "#f";
		toString (SymbolObject s)		= return (show s);
		toString (NumberObject n)		= return (showNumber n);
		toString (CharObject c)			= return (charToString c);
		toString (ByteArrayObject arr)	= do
			{
			text <- printByteArrayContents (toList arr);
			return ("#x("++text++")");
			};
		toString (StringObject arr)		= do
			{
			text <- printString (toList arr);
			return ("\""++text++"\"");
			};
		toString (PairObject	hl tl)	= do
			{
			list <- printList hl tl;
			return ("("++list++")");
			};
		toString (VectorObject v)		= do
			{
			text <- printVector (toList v);
			return ("#("++text++")");
			};
		toString VoidObject				= return "#<void>";
		toString (InputPortObject _)	= return "#<input port>";
		toString (OutputPortObject _)	= return "#<output port>";
		toString (ProcedureObject _)	= return "#<procedure>";
		toString (EnvironmentObject _)	= return "#<environment>";
		};


	-- Errors

	instance (Build cm r,MonadThrow (Object r m) cm) =>
	 ProcedureError cm (Object r m) where
		{
		throwMismatchError = throwMismatchObjError;
		throwArgumentListMismatchError am = do
			{
			amObj <- getConvert am;
			throwArgError "arg-list-mismatch" [amObj];
			};
		};

	instance (Build cm r,MonadThrow (Object r m) cm) =>
	 PatternError cm (Object r m) where
		{
		throwBadPatternError patternObj = throwArgError "bad-pattern" [patternObj];
		throwPatternNotMatchedError = throwArgError "no-pattern-match";
		};

	instance (Build cm r,MonadThrow (Object r m) cm) =>
	 AssembleError cm (Object r m) where
		{
		throwBadCombinationError t (MkMismatch exp obj) =  do
			{
			expobj <- getObject (MkSList (show exp));
			throwArgError "bad-combination" [t,expobj,obj];
			};
		};

	instance (Build m r,MonadThrow (Object r m) m) =>
	 RunError m (Object r m) where
		{
		throwWrongContextError		= throwArgError "single-value-expected";
		throwTooFewArgumentsError	= throwSimpleError "too-few-args";
		throwTooManyArgumentsError	= throwArgError "too-many-args";
		throwBadApplyFormError obj = throwArgError "bad-apply-form" [obj];
		throwArrayRangeError (a,b) i = throwSchemeError "out-of-array-range" ((a,(b,())),(i,()));
		};

	instance (Build cm r,MonadThrow (Object r m) cm) =>
	 SyntaxError cm (Object r m) where
		{
		throwUndefinedSyntaxError sym = throwArgError "undefined-syntax" [SymbolObject sym];
		throwUnknownSyntaxConstructorError sym = throwArgError "undefined-syntax-maker" [SymbolObject sym];
		};
	}
