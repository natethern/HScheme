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

module Org.Org.Semantic.HScheme.Core.Mismatch where
	{
	import Org.Org.Semantic.HScheme.Core.Symbol;
	import Org.Org.Semantic.HBase;

	data Expected =
	 EitherExpected Expected Expected	|
	 EOFTypeExpected			|
	 LiteralExpected Symbol		|
	 NullExpected				|
	 PairTypeExpected			|
	 VectorTypeExpected			|
	 VectorLengthExpected Int	|
	 CharTypeExpected			|
	 NumberTypeExpected			|
	 RealTypeExpected			|
	 RationalTypeExpected		|
	 IntegerTypeExpected		|
	 IntTypeExpected			|
	 Word8TypeExpected			|
	 SymbolTypeExpected			|
	 EnvironmentTypeExpected	|
	 InputPortTypeExpected		|
	 OutputPortTypeExpected		|
	 ByteArrayTypeExpected		|
	 StringTypeExpected			|
	 ProcedureTypeExpected		|
	 RangeExpected Integer Integer	;

	listExpected :: Expected;
	listExpected = EitherExpected NullExpected PairTypeExpected;

	instance Show Expected where
		{
		show (EitherExpected a b)		= (show a) ++ " or " ++ (show b);
		show (LiteralExpected sym)		= "'" ++ show sym;
		show EOFTypeExpected			= "#<eof>";
		show NullExpected				= "()";
		show PairTypeExpected			= "pair";
		show VectorTypeExpected			= "vector";
		show (VectorLengthExpected i)	= "vector length " ++ (show i);
		show CharTypeExpected			= "char";
		show NumberTypeExpected			= "number";
		show RealTypeExpected			= "real number";
		show RationalTypeExpected		= "rational number";
		show IntegerTypeExpected		= "integer";
		show IntTypeExpected			= "int";
		show Word8TypeExpected			= "byte";
		show SymbolTypeExpected			= "symbol";
		show EnvironmentTypeExpected	= "environment";
		show InputPortTypeExpected		= "input port";
		show OutputPortTypeExpected		= "output port";
		show ByteArrayTypeExpected		= "byte-array";
		show StringTypeExpected			= "string";
		show ProcedureTypeExpected		= "procedure";
		show (RangeExpected a b)		= "integer in range [" ++ (show a) ++ "," ++ (show b) ++ "]";
		};

	data Mismatch obj = MkMismatch Expected obj;

	returnThrowMismatch ::
		(
		Monad m,
		MonadSingleThrow (Mismatch obj) n
		) =>
	 Expected -> obj -> m (n a);
	returnThrowMismatch exp obj = return (throwSingle (MkMismatch exp obj));

	type MatchMonad obj = Result (Mismatch obj);

	data ArgumentMismatch obj =
	 UnionArgMismatch (ArgumentMismatch obj) (ArgumentMismatch obj)	|
	 TooFewArguments Int					|
	 TooManyArguments Int [obj]				|
	 WrongArgumentType Int (Mismatch obj)	;
	}
