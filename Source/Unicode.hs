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

module Unicode
	(
	module UnicodeDefs,
	getGeneralCategory,getDecimalDigit,getNumber,
	toUpperCase,toLowerCase,toTitleCase,
	isWhitespace,isBidiControl,isJoinControl,
	isDash,isHyphen,isQuotationMark,isTerminalPunctuation,
	isOtherMath,isHexDigit,isOtherAlphabetic,isIdeographic,
	isDiacritic,isExtender,isOtherLowercase,isOtherUppercase,
	isNoncharacter	
	) where
	{
	import UnicodeData;
	import UnicodeDefs;

	instance Show GCMajorClass where
		{
		show ClLetter		= "Letter";
		show ClMark			= "Mark";
		show ClNumber		= "Number";
		show ClSeparator	= "Separator";
		show ClPunctuation	= "Punctuation";
		show ClSymbol		= "Symbol";
		show ClOther		= "Other";
		};

	instance Show GeneralCategory where
		{
		show GcLu = "Lu";
		show GcLl = "Ll";
		show GcLt = "Lt";
		show GcLm = "Lm";
		show GcLo = "Lo";

		show GcMn = "Mn";
		show GcMc = "Mc";
		show GcMe = "Me";

		show GcNd = "Nd";
		show GcNl = "Nl";
		show GcNo = "No";

		show GcZs = "Zs";
		show GcZl = "Zl";
		show GcZp = "Zp";

		show GcPc = "Pc";
		show GcPd = "Pd";
		show GcPs = "Ps";
		show GcPe = "Pe";
		show GcPi = "Pi";
		show GcPf = "Pf";
		show GcPo = "Po";

		show GcSm = "Sm";
		show GcSc = "Sc";
		show GcSk = "Sk";
		show GcSo = "So";

		show GcCc = "Cc";
		show GcCf = "Cf";
		show GcCs = "Cs";
		show GcCo = "Co";
		show GcCn = "Cn";
		};
	
	getGeneralCategory :: Char -> GeneralCategory;
	getGeneralCategory = codeGenCat . fromEnum;
	
	getDecimalDigit :: Char -> Maybe Integer;
	getDecimalDigit = codeDecDigit . fromEnum;
	
	getNumber :: Char -> Maybe Rational;
	getNumber = codeNumber . fromEnum;
	
	toUpperCase :: Char -> Char;
	toUpperCase = toEnum . codeUpperCase . fromEnum;
	
	toLowerCase :: Char -> Char;
	toLowerCase = toEnum . codeLowerCase . fromEnum;
	
	toTitleCase :: Char -> Char;
	toTitleCase = toEnum . codeTitleCase . fromEnum;
	
	isWhitespace :: Char -> Bool;
	isWhitespace = codeIs_White_space . fromEnum;
	
	isBidiControl :: Char -> Bool;
	isBidiControl = codeIs_Bidi_Control . fromEnum;
	
	isJoinControl :: Char -> Bool;
	isJoinControl = codeIs_Join_Control . fromEnum;
	
	isDash :: Char -> Bool;
	isDash = codeIs_Dash . fromEnum;
	
	isHyphen :: Char -> Bool;
	isHyphen = codeIs_Hyphen . fromEnum;
	
	isQuotationMark :: Char -> Bool;
	isQuotationMark = codeIs_Quotation_Mark . fromEnum;
	
	isTerminalPunctuation :: Char -> Bool;
	isTerminalPunctuation = codeIs_Terminal_Punctuation . fromEnum;
	
	isOtherMath :: Char -> Bool;
	isOtherMath = codeIs_Other_Math . fromEnum;
	
	isHexDigit :: Char -> Bool;
	isHexDigit = codeIs_Hex_Digit . fromEnum;
	
	isOtherAlphabetic :: Char -> Bool;
	isOtherAlphabetic = codeIs_Other_Alphabetic . fromEnum;
	
	isIdeographic :: Char -> Bool;
	isIdeographic = codeIs_Ideographic . fromEnum;
	
	isDiacritic :: Char -> Bool;
	isDiacritic = codeIs_Diacritic . fromEnum;
	
	isExtender :: Char -> Bool;
	isExtender = codeIs_Extender . fromEnum;
	
	isOtherLowercase :: Char -> Bool;
	isOtherLowercase = codeIs_Other_Lowercase . fromEnum;
	
	isOtherUppercase :: Char -> Bool;
	isOtherUppercase = codeIs_Other_Uppercase . fromEnum;
	
	isNoncharacter :: Char -> Bool;
	isNoncharacter = codeIs_Noncharacter_Code_Point . fromEnum;
	}
