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

module UnicodeData where
	{
	import UnicodeDefs;
	import Array;

	sparseArray :: (Ix i) => (i,i) -> a -> [(i,a)] -> Array i a;
	sparseArray	bounds other values =
	 accumArray (\_ a -> a) other bounds values;


	-- Data

	codeDecDigit :: Int -> Maybe Integer;
define(`uchar',`ifelse(`$7',`',`',`	codeDecDigit 0x$1 = Just $7;
')')dnl
include(DATAFILE)dnl
	codeDecDigit _ = Nothing;	

	codeNumber :: Int -> Maybe Rational;
define(`uchar',`ifelse(`$9',`',`',`	codeNumber 0x$1 = Just ($9);
')')dnl
include(DATAFILE)dnl
	codeNumber _ = Nothing;	

	codeGenCatList :: [(Int,GeneralCategory)];
	codeGenCatList =
		[
define(`uchar',`		(0x$1,Gc$3),
')dnl
include(DATAFILE)dnl
		(0x110000,GcCn)
		];

	codeGenCatArray :: Array Int GeneralCategory;
	codeGenCatArray = sparseArray (0,0x110000) GcCn codeGenCatList;

	codeGenCat :: Int -> GeneralCategory;
	codeGenCat i = if (inRange (0,0x110000) i)
	 then codeGenCatArray ! i
	 else GcCn;
	
	codeUpperCase :: Int -> Int;
define(`uchar',`ifelse(`$13',`',`',`	codeUpperCase 0x$1 = 0x$13;
')')dnl
include(DATAFILE)dnl
	codeUpperCase i = i;
	
	codeLowerCase :: Int -> Int;
define(`uchar',`ifelse(`$14',`',`',`	codeLowerCase 0x$1 = 0x$14;
')')dnl
include(DATAFILE)dnl
	codeLowerCase i = i;
	
	codeTitleCase :: Int -> Int;
define(`uchar',`ifelse(`$15',`',`',`	codeTitleCase 0x$1 = 0x$15;
')')dnl
include(DATAFILE)dnl
	codeTitleCase i = i;


	-- Properties
define(`propbegin',`ifelse($1,`',`',`
	codeIs_`'$1 :: Int -> Bool;
')')dnl
define(`propend',`ifelse($1,`',`',`	codeIs_`'curpropname _ = False;
')')dnl
define(`curpropname',`')dnl
define(`newprop',`ifelse($1,curpropname,`',`propend(curpropname)`'propbegin($1)')`'define(`curpropname',$1)')dnl
define(`charprop',`newprop($1)ifelse(`$3',`',`	codeIs_$1 0x$2 = True;',
`	codeIs_$1 i | (i >= 0x$2) && (i <= 0x$3) = True;')')dnl
include(PROPFILE)dnl
newprop(`')dnl
	}
