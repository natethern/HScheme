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
HCIMPORTDIRS	= -i. -iCommon
HCOPTS			= -pgmc$(CC) -pgma$(CC) -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances
HCFLAGS			= $(HCOPTS) $(HCPACKAGES) $(HCIMPORTDIRS)
LDH				= $(HC) -pgml$(CC) $(HCPACKAGES)
LDFLAGS			=

HC_SRCS			= \
	Common/Type.hs	\
	Common/Subtype.hs	\
	Common/LiftedMonad.hs	\
	Common/Ref.hs	\
	Common/MoreRef.hs	\
	ContinuationPassing.hs	\
	UnicodeDefs.hs	\
	UnicodeData.hs	\
	Unicode.hs	\
	Numerics.hs	\
	Port.hs	\
	InputPortParser.hs	\
	\
	Object.hs	\
	Conversions.hs	\
	Evaluate.hs	\
	TopLevel.hs	\
	SystemInterface.hs	\
	Procedures.hs	\
	Lambda.hs	\
	PortProcedures.hs	\
	Equality.hs	\
	SExpChars.hs	\
	SExpParser.hs	\
	Syntax.hs	\
	\
	Bindings.hs	\
	NumericProcedures.hs	\
	StandardBindings.hs	\
	FMapBindings.hs	\
	FullProcedures.hs	\
	FullStandardBindings.hs	\
	IOBindings.hs	\
	\
	SchemeCPS.hs	\
	Pure.hs	\
	Full.hs	\
	Interactive.hs	\
	InteractiveMain.hs

OBJS = $(patsubst %.hs,%.o,$(HC_SRCS))


default: build

clean:
	rm -f hscheme UnicodeData.hs UnicodeData.m4 UnicodeData-3.1.0.txt UnicodePropList.m4 PropList-3.1.0.txt Makefile.bak
	rm -f *.hi *.o Common/*.hi Common/*.o

build: hscheme

hscheme: $(OBJS)
	$(LDH) $(filter %.o,$^) $(LDFLAGS) -o $@

test: hscheme
	./$<

# Standard suffix rules

.SUFFIXES: .o .hs .hi .lhs .hc .s

UnicodeData.hs: UnicodeData.hs.m4 UnicodeData.m4 UnicodePropList.m4
	m4 -DDATAFILE="UnicodeData.m4" -DPROPFILE="UnicodePropList.m4" $< > $@

UnicodeData.m4: UnicodeData-3.1.0.txt
	sed -e 's/;/'\'',`/g; s/^/uchar(`/; s/$$/'\'')dnl/' $< > $@

UnicodeData-3.1.0.txt:
	rm -f $@
	wget http://www.unicode.org/Public/3.1-Update/$@

UnicodePropList.m4: PropList-3.1.0.txt
	sed -e 's/#.*$$//;s/^ *\([.0-9A-F]*\) *; *\([0-9A-Za-z_]*\)/charprop(\2,\1)/;s/\.\./,/;/^$$/ d' $<	> $@

PropList-3.1.0.txt:
	rm -f $@
	wget http://www.unicode.org/Public/3.1-Update/$@

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
# DO NOT DELETE: Beginning of Haskell dependencies
Common/Type.o : Common/Type.hs
Common/Subtype.o : Common/Subtype.hs
Common/Subtype.o : Common/Type.hi
Common/LiftedMonad.o : Common/LiftedMonad.hs
Common/Ref.o : Common/Ref.hs
Common/MoreRef.o : Common/MoreRef.hs
Common/MoreRef.o : Common/LiftedMonad.hi
Common/MoreRef.o : Common/Ref.hi
ContinuationPassing.o : ContinuationPassing.hs
ContinuationPassing.o : Common/LiftedMonad.hi
UnicodeDefs.o : UnicodeDefs.hs
UnicodeData.o : UnicodeData.hs
UnicodeData.o : UnicodeDefs.hi
Unicode.o : Unicode.hs
Unicode.o : UnicodeDefs.hi
Unicode.o : UnicodeData.hi
Numerics.o : Numerics.hs
Port.o : Port.hs
InputPortParser.o : InputPortParser.hs
InputPortParser.o : Common/LiftedMonad.hi
InputPortParser.o : Port.hi
Object.o : Object.hs
Object.o : Common/Subtype.hi
Object.o : Numerics.hi
Object.o : Port.hi
Conversions.o : Conversions.hs
Conversions.o : Common/Type.hi
Conversions.o : Common/Subtype.hi
Conversions.o : Numerics.hi
Conversions.o : Port.hi
Conversions.o : Object.hi
Evaluate.o : Evaluate.hs
Evaluate.o : Common/Type.hi
Evaluate.o : Object.hi
TopLevel.o : TopLevel.hs
TopLevel.o : Common/Type.hi
TopLevel.o : Object.hi
TopLevel.o : Conversions.hi
TopLevel.o : Evaluate.hi
SystemInterface.o : SystemInterface.hs
SystemInterface.o : Common/Type.hi
SystemInterface.o : Port.hi
SystemInterface.o : Object.hi
SystemInterface.o : Conversions.hi
SystemInterface.o : SExpParser.hi
SystemInterface.o : Procedures.hi
SystemInterface.o : TopLevel.hi
SystemInterface.o : Bindings.hi
Procedures.o : Procedures.hs
Procedures.o : Common/Type.hi
Procedures.o : Common/Subtype.hi
Procedures.o : Object.hi
Procedures.o : Conversions.hi
Procedures.o : Evaluate.hi
Lambda.o : Lambda.hs
Lambda.o : Common/Type.hi
Lambda.o : Common/Subtype.hi
Lambda.o : Object.hi
Lambda.o : Conversions.hi
Lambda.o : Procedures.hi
Lambda.o : Evaluate.hi
Lambda.o : TopLevel.hi
PortProcedures.o : PortProcedures.hs
PortProcedures.o : Common/Type.hi
PortProcedures.o : Port.hi
PortProcedures.o : Object.hi
PortProcedures.o : Conversions.hi
Equality.o : Equality.hs
Equality.o : Common/Type.hi
Equality.o : Numerics.hi
Equality.o : Object.hi
SExpChars.o : SExpChars.hs
SExpChars.o : Unicode.hi
SExpParser.o : SExpParser.hs
SExpParser.o : Common/Type.hi
SExpParser.o : Common/Subtype.hi
SExpParser.o : Common/LiftedMonad.hi
SExpParser.o : Unicode.hi
SExpParser.o : SExpChars.hi
SExpParser.o : InputPortParser.hi
SExpParser.o : Port.hi
SExpParser.o : Object.hi
SExpParser.o : Conversions.hi
SExpParser.o : Procedures.hi
SExpParser.o : PortProcedures.hi
Syntax.o : Syntax.hs
Syntax.o : Common/Type.hi
Syntax.o : Common/Subtype.hi
Syntax.o : Object.hi
Syntax.o : Conversions.hi
Bindings.o : Bindings.hs
Bindings.o : Common/Type.hi
Bindings.o : Common/Subtype.hi
Bindings.o : Object.hi
Bindings.o : Conversions.hi
NumericProcedures.o : NumericProcedures.hs
NumericProcedures.o : Common/Type.hi
NumericProcedures.o : Numerics.hi
NumericProcedures.o : Object.hi
NumericProcedures.o : Conversions.hi
StandardBindings.o : StandardBindings.hs
StandardBindings.o : Numerics.hi
StandardBindings.o : Object.hi
StandardBindings.o : Bindings.hi
StandardBindings.o : Evaluate.hi
StandardBindings.o : TopLevel.hi
StandardBindings.o : Procedures.hi
StandardBindings.o : NumericProcedures.hi
StandardBindings.o : Equality.hi
StandardBindings.o : Lambda.hi
StandardBindings.o : PortProcedures.hi
StandardBindings.o : ./Syntax.hi
FMapBindings.o : FMapBindings.hs
FMapBindings.o : Object.hi
FullProcedures.o : FullProcedures.hs
FullProcedures.o : Common/Type.hi
FullProcedures.o : Object.hi
FullProcedures.o : Conversions.hi
FullProcedures.o : Evaluate.hi
FullStandardBindings.o : FullStandardBindings.hs
FullStandardBindings.o : Object.hi
FullStandardBindings.o : Bindings.hi
FullStandardBindings.o : StandardBindings.hi
FullStandardBindings.o : FullProcedures.hi
FullStandardBindings.o : Equality.hi
IOBindings.o : IOBindings.hs
IOBindings.o : Common/Type.hi
IOBindings.o : Common/LiftedMonad.hi
IOBindings.o : Port.hi
IOBindings.o : Object.hi
IOBindings.o : SystemInterface.hi
SchemeCPS.o : SchemeCPS.hs
SchemeCPS.o : Common/Subtype.hi
SchemeCPS.o : Common/LiftedMonad.hi
SchemeCPS.o : Common/Ref.hi
SchemeCPS.o : Common/MoreRef.hi
SchemeCPS.o : Object.hi
SchemeCPS.o : ContinuationPassing.hi
SchemeCPS.o : Conversions.hi
Pure.o : Pure.hs
Pure.o : Object.hi
Full.o : Full.hs
Full.o : Common/Ref.hi
Full.o : Common/MoreRef.hi
Full.o : Object.hi
Interactive.o : Interactive.hs
Interactive.o : Common/Type.hi
Interactive.o : Common/Subtype.hi
Interactive.o : Port.hi
Interactive.o : InputPortParser.hi
Interactive.o : Object.hi
Interactive.o : Conversions.hi
Interactive.o : Procedures.hi
Interactive.o : Bindings.hi
Interactive.o : StandardBindings.hi
Interactive.o : SExpParser.hi
Interactive.o : FullStandardBindings.hi
Interactive.o : FMapBindings.hi
Interactive.o : SystemInterface.hi
InteractiveMain.o : InteractiveMain.hs
InteractiveMain.o : Common/Type.hi
InteractiveMain.o : Pure.hi
InteractiveMain.o : Full.hi
InteractiveMain.o : SchemeCPS.hi
InteractiveMain.o : ContinuationPassing.hi
InteractiveMain.o : Interactive.hi
InteractiveMain.o : SystemInterface.hi
InteractiveMain.o : IOBindings.hi
# DO NOT DELETE: End of Haskell dependencies
