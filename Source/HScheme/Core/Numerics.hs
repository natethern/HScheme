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
	toInexactN = fmap (InexactReal . toInexact);

	toExactN :: Number -> Number;
	toExactN = fmap (convert . toExact);

	isIntegerN :: Number -> Bool;
	isIntegerN n = isJust (do
		{
		(eir :: EIReal) <- maybeConvert n;
		(i :: Integer) <- maybeConvert eir;
		return ();
		});

	showRational :: Rational -> String;
	showRational r | (denominator r ==1) = show (numerator r);
	showRational r = (show (numerator r)) ++"/"++ (show (denominator r));

	showInfRational :: InfExtended Rational -> String;
	showInfRational Infinity = "infinity";
	showInfRational (Finite r) = showRational r;

	showEIReal :: EIReal -> String;
	showEIReal (ExactReal ir) = showInfRational ir;
	showEIReal (InexactReal d) = show d;

	showImagPart :: EIReal -> String;
	showImagPart (ExactReal 0) = "";
	showImagPart (ExactReal 1) = "+i";
	showImagPart (ExactReal (-1)) = "-i";
	showImagPart x | isNegative x = "-" ++ (showEIReal (negate x)) ++ "i";
	showImagPart x = "+" ++ (showEIReal x) ++ "i";

	showNumber :: Number -> String;
	showNumber ((ExactReal 0) :+ (ExactReal 0)) = "0";
	showNumber ((ExactReal 0) :+ i) = showImagPart i;
	showNumber (r :+ i) = (showEIReal r) ++ (showImagPart i);
	}
