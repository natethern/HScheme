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

module Org.Org.Semantic.HScheme.Parse.Numeric where
	{
	import Org.Org.Semantic.HScheme.Parse.SchemeParser;
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase.Text.Properties.Misc;
	import Org.Org.Semantic.HBase;

	setMParse ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 (Maybe a) -> a -> p (Maybe a);
	setMParse Nothing a = return (Just a);
	setMParse (Just _) _ = unexpectedCharError "number prefix";

	setRadix ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 (Maybe Word8,Maybe Bool) -> Word8 -> p (Maybe Word8,Maybe Bool);
	setRadix (mr,me) r = do
		{
		mr' <- setMParse mr r;
		return (mr',me);
		};

	setExactness ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 (Maybe Word8,Maybe Bool) -> Bool -> p (Maybe Word8,Maybe Bool);
	setExactness (mr,me) e = do
		{
		me' <- setMParse me e;
		return (mr,me');
		};

	preficesParse ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 (Maybe Word8,Maybe Bool) -> p (Maybe Word8,Maybe Bool);
	preficesParse mm = (do
		{
		c <- prefixParse;
		mm' <- case c of
			{
			'b' -> setRadix mm 2;
			'o' -> setRadix mm 8;
			'd' -> setRadix mm 10;
			'x' -> setRadix mm 16;
			'i' -> setExactness mm False;
			'e' -> setExactness mm True;
			_ -> mzero;
			};
		preficesParse mm';
		}) ||| return mm;

	digitParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p Word8;
	digitParse = let {?getDecimalDigit=Just getDecimalDigit;} in readDigit;

	digitsPointParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p ([Word8],Maybe [Word8]);
	digitsPointParse = (do
		{
		isTokenParse '.';
		ds <- mZeroOrMore digitParse;
		return ([],Just ds);
		}) ||| (do
		{
		i <- digitParse;
		(is,mds) <- digitsPointParse;
		return (i:is,mds);
		}) ||| (return ([],Nothing));

	hashesPointParse :: (MonadOrParser Char p) =>
	 p ([Word8],Bool);
	hashesPointParse = (do
		{
		isTokenParse '.';
		mZeroOrMore (isTokenParse '#');
		return ([],False);
		}) ||| (do
		{
		isTokenParse '#';
		(is,mds) <- hashesPointParse;
		return (0:is,False);
		}) ||| (return ([],True));

	radixPower :: (?radix :: Word8) =>
	 Integer -> Rational;
	radixPower n = (convert (convert ?radix :: Integer) :: Rational) ^^ n;

	convertRational :: (?exactness :: Maybe Bool) =>
	 Bool -> NaNExtended Rational -> EIReal;
	convertRational exactnessGuess r = if unJust exactnessGuess ?exactness
	 then convert (fmap Finite r)
	 else InexactReal (approximate (fmap Finite r) :: Double);

	uintegerParse :: (MonadOrParser Char p,?radix :: Word8) =>
	 p (Integer,Bool);
	uintegerParse = do
		{
		ds <- mOneOrMore digitParse;
		hs <- mZeroOrMore (do
			{
			isTokenParse '#';
			return 0;
			});
		return (addUpDigits 0 (ds ++ hs),case hs of
			{
			[] -> True;
			(_:_) -> False;
			});
		};

	exponentMarker :: Char -> Bool;
	exponentMarker 'e' = True;
	exponentMarker 's' = True;
	exponentMarker 'f' = True;
	exponentMarker 'd' = True;
	exponentMarker 'l' = True;
	exponentMarker 'E' = True;
	exponentMarker 'S' = True;
	exponentMarker 'F' = True;
	exponentMarker 'D' = True;
	exponentMarker 'L' = True;
	exponentMarker _ = False;

	signParse :: (MonadOrParser Char p) =>
	 p Bool;
	signParse = (do
		{
		isTokenParse '+';
		return False;
		}) ||| (do
		{
		isTokenParse '-';
		return True;
		}) ||| (return False);

	decimalParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	decimalParse = do
		{
		(i,md) <- digitsPointParse;
		(pre,post,e) <- case md of
			{
			Just [] -> do
				{
				case i of
					{
					[] -> mOneOrMore (isTokenParse '#'); -- got nothing but a ".", so must have #s to be a number
					_ -> mZeroOrMore (isTokenParse '#');
					};
				return (i,[],False);
				};
			Just d -> do
				{
				mZeroOrMore (isTokenParse '#');
				return (i,d,False);
				};
			Nothing -> case i of
				{
				[] -> mzero;	-- because digitsPointParse didn't match anything at all
				_ -> do
					{
					(is,e) <- hashesPointParse;
					return (i++is,[],e);
					};
				};
			};
		mexp <- mOptional (let {?radix=10} in do
			{
			matchTokenParse exponentMarker;
			neg <- signParse;
			ds <- mOneOrMore digitParse;
			return (let {d = addUpDigits 0 ds} in if neg then negate d else d);
			});
		return (let
			{
			fullNum :: Integer;
			fullNum = addUpDigits 0 (pre ++ post);

			offset :: Integer;
			offset = (unJust 0 mexp) - (convert (length post));

			rat :: Rational;
			rat = multiply (radixPower offset) fullNum;
			} in convertRational (e && not (isJust mexp)) (Number rat));
		};

	urealParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	urealParse = (do
		{
		(i1,e1) <- uintegerParse;
		isTokenParse '/';
		(i2,e2) <- uintegerParse;
		return (convertRational (e1 && e2) (divide i2 i1));
		}) ||| decimalParse;

	realParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	realParse = do
		{
		neg <- signParse;
		n <- urealParse;
		return (if neg then negate n else n);
		};

	imaginaryParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p EIReal;
	imaginaryParse = do
		{
		neg <- (isTokenParse '+' >> return False) ||| (isTokenParse '-' >> return True);
		mn <- mOptional urealParse;
		isTokenParse 'i';
		return (let {n = unJust 1 mn} in if neg then negate n else n);
		};

	complexParse :: (MonadOrParser Char p,?radix :: Word8,?exactness :: Maybe Bool) =>
	 p Number;
	complexParse = (do
		{
		n <- imaginaryParse;
		return (rectComplex zero n);
		}) ||| (do
		{
		n1 <- realParse;
		(do
			{
			isTokenParse '@';
			n2 <- realParse;
			return (polarComplex n1 n2);
			}) |||
		(do
			{
			n2 <- imaginaryParse;
			return (rectComplex n1 n2);
			}) |||
		(return (rectComplex n1 zero));
		});

	radixNumberParse ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 Word8 -> p Number;
	radixNumberParse radix = do
		{
		(mr,me) <- preficesParse (Nothing,Nothing);
		let
			{
			?radix = unJust radix mr;
			?exactness = me;
			} in
		 complexParse;
		};

	numberParse ::
		(
		SchemeParser cm obj r p,
		?objType :: Type obj
		) =>
	 p Number;
	numberParse = radixNumberParse 10;
	}
