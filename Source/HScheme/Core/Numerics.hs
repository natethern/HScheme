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

module Org.Org.Semantic.HScheme.Core.Numerics where
	{
	import Org.Org.Semantic.HBase;
	
	type Number = Complex EIReal;
	
	equalNumber :: Number -> Number -> Bool;
	equalNumber = (==);
	
	eqvNumber :: Number -> Number -> Bool;
	eqvNumber = (==);
	
	eqNumber :: Number -> Number -> Bool;
	eqNumber = (==);
	
	isExactN :: Number -> Bool;
	isExactN (ExactReal _ :+ ExactReal _) = True;
	isExactN _ = False;

	toInexactN :: Number -> Number;
	toInexactN n = (convert (convert n :: Complex Double));

	toExactN :: Number -> Number;
	toExactN n = case maybeConvert n of
		{
		Just (cir :: Complex (InfExtended Rational)) -> convert cir;
		Nothing -> nan;
		};

	isIntegerN :: Number -> Bool;
	isIntegerN n = isJust (do
		{
		(eir :: EIReal) <- maybeConvert n;
		(i :: Integer) <- maybeConvert eir;
		return ();
		});

	showRational :: Rational -> String;
	showRational r | (denominator r ==1) = show (numerator r);
	showRational r = (show (numerator r)) ++"%"++ (show (denominator r));

	showInfRational :: InfExtended Rational -> String;
	showInfRational Infinity = "infinity";
	showInfRational (Finite r) = showRational r;

	showEIReal :: EIReal -> String;
	showEIReal (ExactReal ir) = showInfRational ir;
	showEIReal (InexactReal d) = show d;

	showNumber :: Number -> String;
	showNumber (r :+ i) | isZero i = showEIReal r;
	showNumber (r :+ i) | isZero r = (showEIReal i) ++ "i";
	showNumber (r :+ i) = (showEIReal r) ++ "+" ++ (showEIReal i) ++ "i";
	}
