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

module Numerics
	(
	Number,
	realPartN,imaginaryPartN,conjugateN,
	toInexactN,
	eqvNumber,eqNumber,equalNumber
	) where
	{
	import Complex;
	import Ratio;
	import Numeric;
	
	data Number = ExactNum !Rational !Rational | InexactNum !(Complex Double);
	
	toCD :: Rational -> Rational -> Complex Double;
	toCD r i = (fromRat r) :+ (fromRat i);
	
	toInexact :: Number -> Complex Double;
	toInexact (InexactNum a) = a;
	toInexact (ExactNum r i) = toCD r i;
	
	toInexactN :: Number -> Number;
	toInexactN = InexactNum . toInexact;
	
	isExact :: Number -> Bool;
	isExact (ExactNum _ _) = True;
	isExact (InexactNum _) = False;
	
	eqvNumber :: Number -> Number -> Bool;
	eqvNumber (ExactNum ar ai) (ExactNum br bi) = ar == br && ai == bi;
	eqvNumber (InexactNum (ar :+ ai)) (InexactNum (br :+ bi)) = ar == br && ai == bi;
	eqvNumber _ _ = False;
	
	eqNumber :: Number -> Number -> Bool;
	eqNumber = eqvNumber;
	
	equalNumber :: Number -> Number -> Bool;
	equalNumber (ExactNum ar ai) (ExactNum br bi) = ar == br && ai == bi;
	equalNumber a b = (toInexact a) == (toInexact b);
	
	instance Eq Number where
		{
		(==) = equalNumber;
		};
	
	showRat :: Rational -> String;
	showRat r = (show n) ++ (if d==1 then "" else "/" ++ (show d)) where
		{
		n = numerator r;
		d = denominator r;
		};
	
	instance Show Number where
		{
		show (ExactNum r 0) = showRat r;
		show (ExactNum 0 i) = (showRat i) ++ "i";
		show (ExactNum r i) = (showRat r) ++ "+" ++ (showRat i) ++ "i";
		show (InexactNum (r :+ 0)) = show r;
		show (InexactNum (0 :+ i)) = (show i) ++ "i";
		show (InexactNum (r :+ i)) = (show r) ++ "+" ++ (show i) ++ "i";
		};
	
	conjugateN :: Number -> Number;
	conjugateN (ExactNum r i) = ExactNum r (-i);
	conjugateN (InexactNum a) = InexactNum (conjugate a);

	realPartN :: Number -> Number;
	realPartN (ExactNum r i) = ExactNum r 0;
	realPartN (InexactNum (r:+i)) = InexactNum (r:+0);

	imaginaryPartN :: Number -> Number;
	imaginaryPartN (ExactNum r i) = ExactNum i 0;
	imaginaryPartN (InexactNum (r:+i)) = InexactNum (i:+0);
	
	instance Num Number where
		{
		fromInteger n = ExactNum (fromInteger n) 0;
		
		(ExactNum ar ai) + (ExactNum br bi) = ExactNum (ar + br) (ai + bi);
		a + b = InexactNum ((toInexact a) + (toInexact b));

		(ExactNum ar ai) - (ExactNum br bi) = ExactNum (ar - br) (ai - bi);
		a - b = InexactNum ((toInexact a) - (toInexact b));

		(ExactNum ar ai) * (ExactNum br bi) = ExactNum
		 (ar*br - ai*bi)
		 (ai*br + ar*bi);
		(ExactNum 0 0) * b = ExactNum 0 0;
		a * (ExactNum 0 0) = ExactNum 0 0;
		a * b = InexactNum ((toInexact a) * (toInexact b));
		
		abs x = sqrt (x * (conjugateN x));
		
		signum (ExactNum 0 0) = ExactNum 0 0;
		signum (InexactNum 0) = InexactNum 0;
		signum x = x / (abs x);
		};
	
	instance Fractional Number where
		{
		fromRational n =  ExactNum (fromRational n) 0;

		(ExactNum ar ai) / (ExactNum br bi) = ExactNum
		 ((ar*br + ai*bi)/d)
		 ((ai*br - ar*bi)/d) where
			{
			d = br*br + bi*bi;
			};
		(ExactNum 0 0) / b | (b /= 0) = ExactNum 0 0;
		a / b = InexactNum ((toInexact a) / (toInexact b));
		};
	
	n1 :: (Complex Double -> Complex Double) -> (Number -> Number);
	n1 op a = InexactNum (op (toInexact a));
	
	instance Floating Number where
		{
		pi = InexactNum pi;
		log = n1 log;
		exp = n1 exp;
		sin = n1 sin;
		cos = n1 cos;
		tan = n1 tan;
		asin = n1 asin;
		acos = n1 acos;
		atan = n1 atan;
		sinh = n1 sinh;
		cosh = n1 cosh;
		tanh = n1 tanh;
		asinh = n1 asinh;
		acosh = n1 acosh;
		atanh = n1 atanh;
		};	
	}
