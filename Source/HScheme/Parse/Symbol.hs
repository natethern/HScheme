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

module Org.Org.Semantic.HScheme.Parse.Symbol (symbolParse) where
	{
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HBase.Text.Properties.GeneralCategory;
	import Org.Org.Semantic.HBase.Text.Properties.Case;
	import Org.Org.Semantic.HBase;

{--
ASCII: 2^7 = 128
 1 space		 		Zs
52 letters				Lu,Ll
10 digits				Nd
33 controls				Cc
 6 left/right	()[]{}	Ps,Pe
 1 currency		$		Sc
 6 maths		+<=>|~	Sm
 1 dash			-		Pd
 1 connector	_		Pc
 2 accents		^`		Sk
 6 stops		,:;.!?	Po
 2 quotes		'"		Po
 7 misc			#%&*/@\	Po

R5RS:
initial = letter or !$%&*/:<=>?^_~
subs = initial or digit or +-.@
non-subs = control or space or left/right or '"`,;#\|

forbidden = Cc except tab cr lf
reserved = []{} (Ps,Pe), | (Sm)
whitespace = Zs, tab cr lf (Cc)
initial = Lu, Ll, Pc, Sc, :!?%&*/ (Po), <=>~ (Sm), ^ (Sk)
swing = Nd, Pd, + (Sm), .@ (Po),
others = () (Ps,Pe), '",;#\ (Po), ` (Sk)

HScheme:
forbidden = C* except tab cr lf
reserved = (Ps,Pe) except () , Pi, Pf, |
whitespace = Z*, tab cr lf
initial = L*, M*, S* except +`| , Pc, Pd except - , Po except .@'",;#\
swing = N*, -+.@
others = () (Ps,Pe), '",;#\ (Po), ` (Sk)

--}

	allowed :: Char -> Bool;
	allowed '\t' = True;
	allowed '\r' = True;
	allowed '\n' = True;
	allowed i = gcMajorClass (getGeneralCategory i) /= ClOther;
	
	allowedIdentifier1 :: Char -> Bool;

	allowedIdentifier1 '+' = False;
	allowedIdentifier1 '`' = False;
	allowedIdentifier1 '|' = False;

	allowedIdentifier1 '-' = False;

	allowedIdentifier1 '.' = False;
	allowedIdentifier1 '@' = False;
	allowedIdentifier1 '\'' = False;
	allowedIdentifier1 '"' = False;
	allowedIdentifier1 ',' = False;
	allowedIdentifier1 ';' = False;
	allowedIdentifier1 '#' = False;
	allowedIdentifier1 '\\' = False;
	
	allowedIdentifier1 i = case getGeneralCategory i of
		{
		GcPc -> True;
		GcPd -> True;
		GcPo -> True;
		gc -> case gcMajorClass gc of
			{
			ClLetter -> True;
			ClMark -> True;
			ClSymbol -> True;
			_ -> False;
			};
		};

	
	allowedIdentifierR :: Char -> Bool;

	allowedIdentifierR '-' = True;
	allowedIdentifierR '+' = True;
	allowedIdentifierR '.' = True;
	allowedIdentifierR '@' = True;
	
	allowedIdentifierR i = case gcMajorClass (getGeneralCategory i) of
		{
		ClNumber -> True;
		_ -> allowedIdentifier1 i;
		};


	symbolParse :: (MonadOrParser Char p) =>
	 p Symbol;
	symbolParse = fmap MkSymbol (
	 (fmap (\c -> [c]) (isTokenParse '+'))	|||
	 (fmap (\c -> [c]) (isTokenParse '-'))	|||
	 (do
		{
		n <- matchTokenParse allowedIdentifier1;
		ns <- mZeroOrMore ((matchTokenParse allowedIdentifierR) >>= (return . toLowerCase));
		return (toLowerCase n:ns);
		})
	 );
	}
