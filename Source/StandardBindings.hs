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

module StandardBindings where
	{
	import Port;
	import Lambda;
	import Equality;
	import NumericProcedures;
	import Procedures;
	import Evaluate;
	import Bindings;
	import Object;
	import Numerics;

	stdBindings :: (Scheme x m r) => Bindings r m -> m (Bindings r m);
	stdBindings = chainList
		[
		addProcBinding	"car"	carS,
		addProcBinding	"cdr"	cdrS,
		addProcBinding	"cons"	consS,
		addProcBinding	"list"	listS,
		addProcBinding	"to-string"	toStringS,
		addMacroBinding	"if"	ifS,
		addMacroBinding	"quote"	quoteS,
		addProcBinding	"values"	valuesS,
		addProcBinding	"current-environment"	currentEnvironmentS,
		addProcBinding	"eval"	evaluateS,
		addProcBinding	"equal?"	equalS,
		addProcBinding	"procedure?"	isProcedureS,
		addMacroBinding	"let"	letS,
		addMacroBinding	"let*"	letStarS,
		addMacroBinding	"lambda"	lambdaS,
		addProcBinding	"+"	(foldingLS (+) 0),
		addProcBinding	"-"	subtractS,
		addProcBinding	"*"	(foldingLS (*) 1),
		addProcBinding	"/"	divideS
		];

	monadicStdBindings :: (Scheme x m r) => Bindings r m -> m (Bindings r m);
	monadicStdBindings = chainList
		[
		stdBindings,
		addMacroBinding	"begin"	beginS,
		addProcBinding	"call-with-current-continuation"	callCCS,
		addProcBinding	"input-port?"	isInputPortS,
		addProcBinding	"output-port?"	isOutputPortS,
		addProcBinding	"port-write-char"	portWriteCharS,
		addProcBinding	"close-input-port"	inputPortCloseS,
		addProcBinding	"close-output-port"	outputPortCloseS
		];
	}
