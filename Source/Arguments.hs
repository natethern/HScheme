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

module Arguments where
	{
	import Org.Org.Semantic.HBase;

	data SchemeStdBindings = FullStdBindings | PureStdBindings | StrictPureStdBindings;

	instance Show SchemeStdBindings where
		{
		show FullStdBindings = "full";
		show PureStdBindings = "pure";
		show StrictPureStdBindings = "strict pure";
		};

	data SchemeWhichMonad = IdentityWhichMonad | IOWhichMonad | CPSWhichMonad | GCPSWhichMonad;

	instance Show SchemeWhichMonad where
		{
		show IdentityWhichMonad = "pure";
		show IOWhichMonad = "IO";
		show CPSWhichMonad = "CPS IO";
		show GCPSWhichMonad = "GCPS IO";
		};

	defaultWhichMonad :: SchemeWhichMonad;
	defaultWhichMonad = GCPSWhichMonad;

	defaultStdBindings :: SchemeWhichMonad -> SchemeStdBindings;
	defaultStdBindings IdentityWhichMonad = StrictPureStdBindings;
	defaultStdBindings _ = FullStdBindings;

	parseArgs :: (Monad m) =>
	 [String] -> m (Maybe SchemeStdBindings,Maybe SchemeWhichMonad,[String],Bool,[String],Bool);
	parseArgs [] = return (Nothing,Nothing,[],True,[],False);
	parseArgs ("-":files) = return (Nothing,Nothing,[],True,files,False);
	parseArgs ("--mgcps":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just GCPSWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--mcps":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just CPSWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--mio":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just IOWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--mpure":args) = do
		{
		(f,_,paths,initfile,files,verbose) <- parseArgs args;
		return (f,Just IdentityWhichMonad,paths,initfile,files,verbose);
		};
	parseArgs ("--bfull":args) = do
		{
		(_,m,paths,initfile,files,verbose) <- parseArgs args;
		return (Just FullStdBindings,m,paths,initfile,files,verbose);
		};
	parseArgs ("--bpure":args) = do
		{
		(_,m,paths,initfile,files,verbose) <- parseArgs args;
		return (Just PureStdBindings,m,paths,initfile,files,verbose);
		};
	parseArgs (('-':('I':path@(_:_))):args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,path:paths,initfile,files,verbose);
		};
	parseArgs ("-I":(path:args)) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,path:paths,initfile,files,verbose);
		};
	parseArgs ("--noinit":args) = do
		{
		(f,m,paths,_,files,verbose) <- parseArgs args;
		return (f,m,paths,False,files,verbose);
		};
	parseArgs ("-n":args) = do
		{
		(f,m,paths,_,files,verbose) <- parseArgs args;
		return (f,m,paths,False,files,verbose);
		};
	parseArgs ("-v":args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,paths,initfile,files,True);
		};
	parseArgs (flag@('-':_):args) = fail ("unrecognised flag "++(show flag));
	parseArgs (file:args) = do
		{
		(f,m,paths,initfile,files,verbose) <- parseArgs args;
		return (f,m,paths,initfile,(file:files),verbose);
		};
	}
