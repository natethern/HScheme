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

module Org.Org.Semantic.HScheme.RunLib.SystemPorts where
	{
	import Org.Org.Semantic.HScheme.Core;
	import Org.Org.Semantic.HBase;

	currentInputPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (InputPort Word8 cm);
	currentInputPortP fsi () = return (fsiCurrentInputPort fsi);

	currentOutputPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (OutputPort Word8 cm);
	currentOutputPortP fsi () = return (fsiCurrentOutputPort fsi);

	currentErrorPortP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 () -> cm (OutputPort Word8 cm);
	currentErrorPortP fsi () = return (fsiCurrentErrorPort fsi);

	openInputFileP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 (SList Char,()) -> cm (InputPort Word8 cm);
	openInputFileP fsi (MkSList name,()) = fsiOpenInputFile fsi name;

	openOutputFileP :: (Build cm r) =>
	 FullSystemInterface cm m r ->
	 (SList Char,()) -> cm (OutputPort Word8 cm);
	openOutputFileP fsi (MkSList name,()) = fsiOpenOutputFile fsi name;
	}
