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

module UnicodeDefs where
	{
	data GCMajorClass =
	 ClLetter | ClMark | ClNumber | ClSeparator | ClPunctuation | ClSymbol | ClOther
	 deriving Eq;
	 

	data GeneralCategory =
	 GcLu | GcLl | GcLt | GcLm | GcLo |
	 GcMn | GcMc | GcMe |
	 GcNd | GcNl | GcNo |
	 GcZs | GcZl | GcZp |
	 GcPc | GcPd | GcPs | GcPe | GcPi | GcPf | GcPo |
	 GcSm | GcSc | GcSk | GcSo |
	 GcCc | GcCf | GcCs | GcCo | GcCn
	 deriving Eq;
	 
	gcMajorClass :: GeneralCategory -> GCMajorClass;
	gcMajorClass GcLu = ClLetter		;
	gcMajorClass GcLl = ClLetter		;
	gcMajorClass GcLt = ClLetter		;
	gcMajorClass GcLm = ClLetter		;
	gcMajorClass GcLo = ClLetter		;
	gcMajorClass GcMn = ClMark			;
	gcMajorClass GcMc = ClMark			;
	gcMajorClass GcMe = ClMark			;
	gcMajorClass GcNd = ClNumber		;
	gcMajorClass GcNl = ClNumber		;
	gcMajorClass GcNo = ClNumber		;
	gcMajorClass GcZs = ClSeparator		;
	gcMajorClass GcZl = ClSeparator		;
	gcMajorClass GcZp = ClSeparator		;
	gcMajorClass GcPc = ClPunctuation	;
	gcMajorClass GcPd = ClPunctuation	;
	gcMajorClass GcPs = ClPunctuation	;
	gcMajorClass GcPe = ClPunctuation	;
	gcMajorClass GcPi = ClPunctuation	;
	gcMajorClass GcPf = ClPunctuation	;
	gcMajorClass GcPo = ClPunctuation	;
	gcMajorClass GcSm = ClSymbol		;
	gcMajorClass GcSc = ClSymbol		;
	gcMajorClass GcSk = ClSymbol		;
	gcMajorClass GcSo = ClSymbol		;
	gcMajorClass GcCc = ClOther			;
	gcMajorClass GcCf = ClOther			;
	gcMajorClass GcCs = ClOther			;
	gcMajorClass GcCo = ClOther			;
	gcMajorClass GcCn = ClOther			;
	}
