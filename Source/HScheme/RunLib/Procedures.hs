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

module Org.Org.Semantic.HScheme.RunLib.Procedures where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase.Time;
	import Org.Org.Semantic.HBase;


	-- 6.3.1 Booleans
	notP :: (Monad m) =>
	 (Bool,()) -> m Bool;
	notP (b,()) = return (not b);

	isBooleanP :: (HasBooleanType obj,Monad m,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isBooleanP (obj,()) = return (isBooleanType obj);


	-- 6.3.3 Symbols
	isSymbolP :: (Build m r,ObjectSubtype r obj Symbol,?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isSymbolP (obj,()) = getObjectIs (MkType :: Type Symbol) obj;

	makeSymbolP :: (Monad m) =>
	 (SList Char,()) -> m Symbol;
	makeSymbolP (MkSList s,()) = return (MkSymbol s);


	-- 6.3.4 Characters
	charTestP :: (Build m r,ObjectSubtype r obj Char,?objType :: Type obj) =>
	 (Char -> Bool) -> (obj,()) -> m Bool;
	charTestP f (obj,()) = testObject (return . f) obj;

	charFuncP :: (Monad m) =>
	 (Char -> a) -> (Char,()) -> m a;
	charFuncP f (c,()) = return (f c);

	integerToCharP ::
		(
		Build m r,
		RunError m obj,
--		ObjectSubtype r obj Symbol,
		ObjectSubtype r obj Integer,
		?objType :: Type obj
		) =>
	 (Integer,()) -> m Char;
	integerToCharP (i,()) = case nthFromStart i of
		{
		Just c -> return c;
		Nothing -> do
			{
			iobj <- getObject i;
			throwMismatchError (MkMismatch (RangeExpected 0 0x10FFFF) iobj);
			};
		};

	-- 6.3.5 Strings
	isStringP :: (Build m r,ObjectSubtype r obj (SList Char),?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isStringP (obj,()) = getObjectIs (MkType :: Type (SList Char)) obj;

	makeRefList :: (Build m r,?objType :: Type obj) =>
	 Integer -> a -> m [r a];
	makeRefList 0 _ = return [];
	makeRefList i c = do
		{
		refs <- makeRefList (i - 1) c;
		ref <- new c;
		return (ref:refs);
		};

	makeStringP :: (ListObject r obj,Build m r,?objType :: Type obj) =>
	 (Integer,Maybe Char) -> m (SRefList r Char);
	makeStringP (i,Nothing) = do
		{
		rs <- makeRefList i '\x0000';
		return (MkSRefList rs);
		};
	makeStringP (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkSRefList rs);
		};

	stringP :: (Monad m) =>
	 [Char] -> m (SList Char);
	stringP cs = return (MkSList cs);

	stringLengthP :: (ListObject r obj,Monad m,?objType :: Type obj) =>
	 (SRefArray r Char,()) -> m Int;
	stringLengthP (s,()) = return (length s);

	getArrayRef ::
		(
		RunError m obj,
		?objType :: Type obj
		) =>
	 Integer -> ArrayList a -> m a;
	getArrayRef i arr | i < 0 = throwArrayRangeError (0,(convertFromInt (length arr)) - 1) i;
	getArrayRef i arr | i >= len = throwArrayRangeError (0,len - 1) i where
		{
		len = convertFromInt (length arr);
		};
	getArrayRef i arr = return (arr !! (convertToInt i));

	stringRefP ::
		(
		ListObject r obj,
		Build m r,
		RunError m obj,
		?objType :: Type obj
		) =>
	 (SRefArray r Char,(Integer,())) -> m Char;
	stringRefP (arr,(i,())) = do
		{
		r <- getArrayRef i arr;
		get r;
		};

	stringCharsP :: (Monad m) =>
	 (SList Char,()) -> m [Char];
	stringCharsP (MkSList cs,()) = return cs;

	-- create a completely new string
	stringAppendP :: (Monad m) =>
	 [SList Char] -> m (SList Char);
	stringAppendP = return . MkSList . concatenateList . (fmap unSList);


	-- Byte Arrays
	isByteArrayP :: (Build m r,ObjectSubtype r obj (SList Word8),?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isByteArrayP (obj,()) = getObjectIs (MkType :: Type (SList Word8)) obj;

	makeByteArrayP :: (ListObject r obj,Build m r,?objType :: Type obj) =>
	 (Integer,Maybe Word8) -> m (SRefList r Word8);
	makeByteArrayP (i,Nothing) = do
		{
		rs <- makeRefList i 0;
		return (MkSRefList rs);
		};
	makeByteArrayP (i,Just c) = do
		{
		rs <- makeRefList i c;
		return (MkSRefList rs);
		};

	byteArrayP :: (Monad m) =>
	 [Word8] -> m (SList Word8);
	byteArrayP cs = return (MkSList cs);

	byteArrayLengthP :: (ListObject r obj,Monad m,?objType :: Type obj) =>
	 (SRefArray r Word8,()) -> m Int;
	byteArrayLengthP (s,()) = return (length s);

	byteArrayRefP ::
		(
		ListObject r obj,
		Build m r,
		RunError m obj,
		?objType :: Type obj
		) =>
	 (SRefArray r Word8,(Integer,())) -> m Word8;
	byteArrayRefP (arr,(i,())) = do
		{
		r <- getArrayRef i arr;
		get r;
		};

	byteArrayBytesP :: (Monad m) =>
	 (SList Word8,()) -> m [Word8];
	byteArrayBytesP (MkSList cs,()) = return cs;

	-- create a completely new byte array
	byteArrayAppendP :: (Monad m) =>
	 [SList Word8] -> m (SList Word8);
	byteArrayAppendP = return . MkSList . concatenateList . (fmap unSList);


	-- Encoding
	accreteEitherList :: [Either a (SList a)] -> [a];
	accreteEitherList [] = [];
	accreteEitherList (Left a:r) = (a:accreteEitherList r);
	accreteEitherList (Right (MkSList al):r) = al ++ (accreteEitherList r);

	encodeP :: (Monad m) =>
	 (String -> [Word8]) -> [Either Char (SList Char)] -> m (SList Word8);
	encodeP enc el = return (MkSList (enc (accreteEitherList el)));

	decodeP :: (Monad m) =>
	 ([Word8] -> ExceptionMonad m x String) ->
	 [Either Word8 (SList Word8)] -> m (Either (SList Char) Bool);
	decodeP dec el = do
		{
		result <- unExceptionMonad (dec (accreteEitherList el));
		case result of
			{
			SuccessResult text -> return (Left (MkSList text));
			_ -> return (Right False);
			};
		};

	
	-- 6.3.6 Vectors
	isVectorP :: (Build m r,ObjectSubtype r obj (SList obj),?objType :: Type obj) =>
	 (obj,()) -> m Bool;
	isVectorP (obj,()) = getObjectIs (t obj) obj where
		{
		t :: obj -> Type (SList obj);
		t _ = MkType;
		};

	makeVector :: Int -> obj -> SList obj;
	makeVector i fill = MkSList (makeObjList i fill) where
		{
		makeObjList 0 a = [];
		makeObjList n a = (a:(makeObjList (n - 1) a));
		};

	makeVectorP :: (Monad m,ListObject r obj,?objType :: Type obj) =>
	 (Int,Maybe obj) -> m (SList obj);
	makeVectorP (i,Just fill) = return (makeVector i fill);
	makeVectorP (i,Nothing) = return (makeVector i nilObject);

	vectorP :: (Monad m,?objType :: Type obj) =>
	 [obj] -> m (SList obj);
	vectorP list = return (MkSList list);

	vectorLengthP :: (ListObject r obj,Monad m,?objType :: Type obj) =>
	 (SRefArray r obj,()) -> m Int;
	vectorLengthP (vec,_) = return (length vec);

	vectorRefP :: (Build m r,ListObject r obj,?objType :: Type obj) =>
	 (SRefArray r obj,(Int,())) -> m obj;
	vectorRefP (arr,(i,_)) = get (fetchElement i arr);

	vectorToListP :: (Monad m,?objType :: Type obj) =>
	 (SList obj,()) -> m [obj];
	vectorToListP (MkSList objs,_) = return objs;

	listToVectorP :: (Monad m,?objType :: Type obj) =>
	 ([obj],()) -> m (SList obj);
	listToVectorP (list,_) = return (MkSList list);

	-- Misc
	currentTimeP :: (Monad m) =>
	 (forall a. IO a -> m a) ->
	 () -> m EIReal;
	currentTimeP lifter () = lifter (fmap (\(MkUTCTime d s) -> (InexactReal (approximate (convert (86400 * d) + s)))) getCurrentUTCTime);
	}
