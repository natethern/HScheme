# HScheme -- a Scheme interpreter written in Haskell
# Copyright (C) 2002 Ashley Yakeley <ashley@semantic.org>
#
# This file is part of HScheme.
#
# HScheme is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# HScheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HScheme; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

GHC = ghc  -package lang  $*
#  -fno-monomorphism-restriction

HC				= ghc
HCI				= ghci
HCPACKAGES		= -package data
HCIMPORTDIRS	= -i. -i/usr/share/haskell-import
#HCOPTS			= -pgmc$(CC) -pgma$(CC) -fglasgow-exts -fallow-undecidable-instances -fno-implicit-prelude
HCOPTS			= -pgmc$(CC) -pgma$(CC) -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -fno-implicit-prelude
HCFLAGS			= $(HCOPTS) $(HCPACKAGES) $(HCIMPORTDIRS)
HROOT			= Org/Org/Semantic
HIER			= $(HROOT)/HScheme
LDH				= $(HC) -pgml$(CC) $(HCPACKAGES)
LDFLAGS			=

HC_SRCS			= \
	$(HIER)/Numerics.hs	\
	$(HIER)/Port.hs	\
	$(HIER)/InputPortParser.hs	\
	\
	$(HIER)/Object.hs	\
	$(HIER)/Conversions.hs	\
	$(HIER)/Evaluate.hs	\
	$(HIER)/TopLevel.hs	\
	$(HIER)/SystemInterface.hs	\
	$(HIER)/Procedures.hs	\
	$(HIER)/Lambda.hs	\
	$(HIER)/PortProcedures.hs	\
	$(HIER)/Equality.hs	\
	$(HIER)/SExpChars.hs	\
	$(HIER)/SExpParser.hs	\
	$(HIER)/Syntax.hs	\
	\
	$(HIER)/Bindings.hs	\
	$(HIER)/NumericProcedures.hs	\
	$(HIER)/StandardBindings.hs	\
	$(HIER)/FMapBindings.hs	\
	$(HIER)/FullProcedures.hs	\
	$(HIER)/FullStandardBindings.hs	\
	$(HIER)/IOBindings.hs	\
	\
	$(HIER)/SchemeCPS.hs	\
	$(HIER)/Interactive.hs	\
	InteractiveMain.hs

OBJS = $(patsubst %.hs,%.o,$(HC_SRCS))

COMMAND	=hscheme.cmd

default: build

clean:
	rm -f $(COMMAND) Makefile.bak
	rm -f *.hi *.o $(HIER)/*.hi $(HIER)/*.o

build: $(COMMAND)

$(COMMAND): $(OBJS) /usr/lib/libHBase.a
	$(LDH) $(filter %.o,$^) /usr/lib/libHBase.a $(LDFLAGS) -o $@

test: $(COMMAND)
	./$<

# Standard suffix rules

.SUFFIXES: .o .hs .hi .lhs .hc .s

%.int:
	$(HCI) $(HCOPTS) -package lang -i../../JVM-Bridge/Current/source/Haskell $(HCIMPORTDIRS) $*

%.hi: %.o
	@:

%.o: %.hs
	$(HC) $(HCFLAGS) -c $< -o $@

%.o: %.lhs
	$(HC) $(HCFLAGS) -c $< -o $@

depend: $(HC_SRCS)
	$(HC) -M $(HCFLAGS) $^

$(HROOT):
	mkdir -p Org/Org/
	cd Org/Org/; ln -s ../../ Semantic

# DO NOT DELETE: Beginning of Haskell dependencies
Org/Org/Semantic/HScheme/Numerics.o : Org/Org/Semantic/HScheme/Numerics.hs
Org/Org/Semantic/HScheme/Numerics.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Port.o : Org/Org/Semantic/HScheme/Port.hs
Org/Org/Semantic/HScheme/Port.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/InputPortParser.o : Org/Org/Semantic/HScheme/InputPortParser.hs
Org/Org/Semantic/HScheme/InputPortParser.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/InputPortParser.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Object.o : Org/Org/Semantic/HScheme/Object.hs
Org/Org/Semantic/HScheme/Object.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Object.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Object.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Conversions.hs
Org/Org/Semantic/HScheme/Conversions.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Evaluate.o : Org/Org/Semantic/HScheme/Evaluate.hs
Org/Org/Semantic/HScheme/Evaluate.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Evaluate.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/TopLevel.hs
Org/Org/Semantic/HScheme/TopLevel.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/SystemInterface.hs
Org/Org/Semantic/HScheme/SystemInterface.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/SExpParser.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Procedures.hs
Org/Org/Semantic/HScheme/Procedures.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Lambda.hs
Org/Org/Semantic/HScheme/Lambda.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/PortProcedures.hs
Org/Org/Semantic/HScheme/PortProcedures.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Equality.hs
Org/Org/Semantic/HScheme/Equality.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SExpChars.o : Org/Org/Semantic/HScheme/SExpChars.hs
Org/Org/Semantic/HScheme/SExpChars.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/SExpParser.hs
Org/Org/Semantic/HScheme/SExpParser.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/SExpChars.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/InputPortParser.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Syntax.hs
Org/Org/Semantic/HScheme/Syntax.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Bindings.o : Org/Org/Semantic/HScheme/Bindings.hs
Org/Org/Semantic/HScheme/Bindings.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Bindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Bindings.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/NumericProcedures.hs
Org/Org/Semantic/HScheme/NumericProcedures.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/StandardBindings.hs
Org/Org/Semantic/HScheme/StandardBindings.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/NumericProcedures.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Equality.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Lambda.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Syntax.hi
Org/Org/Semantic/HScheme/FMapBindings.o : Org/Org/Semantic/HScheme/FMapBindings.hs
Org/Org/Semantic/HScheme/FMapBindings.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/FMapBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/FullProcedures.hs
Org/Org/Semantic/HScheme/FullProcedures.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/FullStandardBindings.hs
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/StandardBindings.hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/FullProcedures.hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Equality.hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/IOBindings.hs
Org/Org/Semantic/HScheme/IOBindings.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/SystemInterface.hi
Org/Org/Semantic/HScheme/SchemeCPS.o : Org/Org/Semantic/HScheme/SchemeCPS.hs
Org/Org/Semantic/HScheme/SchemeCPS.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/SchemeCPS.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SchemeCPS.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Interactive.hs
Org/Org/Semantic/HScheme/Interactive.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/InputPortParser.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/StandardBindings.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/SExpParser.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/FullStandardBindings.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/FMapBindings.hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/SystemInterface.hi
InteractiveMain.o : InteractiveMain.hs
InteractiveMain.o : /usr/share/haskell-import/Org/Org/Semantic/HBase.hi
InteractiveMain.o : Org/Org/Semantic/HScheme/SchemeCPS.hi
InteractiveMain.o : Org/Org/Semantic/HScheme/Interactive.hi
InteractiveMain.o : Org/Org/Semantic/HScheme/SystemInterface.hi
InteractiveMain.o : Org/Org/Semantic/HScheme/IOBindings.hi
# DO NOT DELETE: End of Haskell dependencies
