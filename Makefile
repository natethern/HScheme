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
HCOPTS			= -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances
HCFLAGS			= $(HCOPTS) $(HCPACKAGES) $(HCIMPORTDIRS)
LDH				= $(HC) $(HCPACKAGES)
LDFLAGS			=

HC_SRCS			= \
	Common/Ref.hs	\
	Common/Subtype.hs	\
	Common/Type.hs	\
	Common/LiftedMonad.hs	\
	UnicodeDefs.hs	\
	UnicodeData.hs	\
	Unicode.hs	\
	Parser.hs	\
	Numerics.hs	\
	Object.hs	\
	SExpChars.hs	\
	SExpParser.hs	\
	Evaluate.hs	\
	Procedures.hs	\
	Conversions.hs	\
	Bindings.hs	\
	StandardBindings.hs	\
	ContinuationPassing.hs	\
	FMapBindings.hs	\
	SchemeCPS.hs	\
	Pure.hs	\
	Interactive.hs

OBJS = $(patsubst %.hs,%.o,$(HC_SRCS))


default: build

clean:
	rm -f UnicodeData.hs UnicodeData.m4 UnicodeData-3.1.0.txt Makefile.bak
	rm -f *.hi *.o Common/*.hi Common/*.o

build: hscheme

hscheme: $(OBJS)
	$(LDH) $(filter %.o,$^) $(LDFLAGS) -o $@

test: hscheme
	./$<

# Standard suffix rules

.SUFFIXES: .o .hs .hi .lhs .hc .s

UnicodeData.hs: UnicodeData.hs.m4 UnicodeData.m4
	m4 -DDATAFILE="UnicodeData.m4" $< > $@

UnicodeData.m4: UnicodeData-3.1.0.txt
	sed -e 's/;/'\'',`/g; s/^/uchar(`/; s/$$/'\'')dnl/' $< > $@

UnicodeData-3.1.0.txt: 
	rm -f $@
	wget http://www.unicode.org/Public/3.1-Update/UnicodeData-3.1.0.txt

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
Common/Ref.o : Common/Ref.hs
Common/Subtype.o : Common/Subtype.hs
Common/Subtype.o : Common/Type.hi
Common/Type.o : Common/Type.hs
Common/LiftedMonad.o : Common/LiftedMonad.hs
UnicodeDefs.o : UnicodeDefs.hs
UnicodeData.o : UnicodeData.hs
UnicodeData.o : UnicodeDefs.hi
Unicode.o : Unicode.hs
Unicode.o : UnicodeDefs.hi
Unicode.o : UnicodeData.hi
Parser.o : Parser.hs
Numerics.o : Numerics.hs
Object.o : Object.hs
Object.o : Common/Subtype.hi
Object.o : Numerics.hi
SExpChars.o : SExpChars.hs
SExpChars.o : Unicode.hi
SExpChars.o : Parser.hi
SExpParser.o : SExpParser.hs
SExpParser.o : Common/LiftedMonad.hi
SExpParser.o : Unicode.hi
SExpParser.o : SExpChars.hi
SExpParser.o : Parser.hi
SExpParser.o : Object.hi
SExpParser.o : Procedures.hi
Evaluate.o : Evaluate.hs
Evaluate.o : Object.hi
Procedures.o : Procedures.hs
Procedures.o : Common/Type.hi
Procedures.o : Common/Subtype.hi
Procedures.o : Object.hi
Conversions.o : Conversions.hs
Conversions.o : Common/Type.hi
Conversions.o : Common/Subtype.hi
Conversions.o : Object.hi
Bindings.o : Bindings.hs
Bindings.o : Common/Type.hi
Bindings.o : Common/Subtype.hi
Bindings.o : Object.hi
Bindings.o : Conversions.hi
StandardBindings.o : StandardBindings.hs
StandardBindings.o : Object.hi
StandardBindings.o : Bindings.hi
StandardBindings.o : Procedures.hi
ContinuationPassing.o : ContinuationPassing.hs
ContinuationPassing.o : Common/LiftedMonad.hi
FMapBindings.o : FMapBindings.hs
FMapBindings.o : Object.hi
SchemeCPS.o : SchemeCPS.hs
SchemeCPS.o : Common/Subtype.hi
SchemeCPS.o : Object.hi
SchemeCPS.o : ContinuationPassing.hi
SchemeCPS.o : Conversions.hi
Pure.o : Pure.hs
Pure.o : Common/Subtype.hi
Pure.o : Object.hi
Interactive.o : Interactive.hs
Interactive.o : Common/Type.hi
Interactive.o : Common/Subtype.hi
Interactive.o : Pure.hi
Interactive.o : SchemeCPS.hi
Interactive.o : FMapBindings.hi
Interactive.o : Common/LiftedMonad.hi
Interactive.o : Parser.hi
Interactive.o : Object.hi
Interactive.o : Evaluate.hi
Interactive.o : Procedures.hi
Interactive.o : StandardBindings.hi
Interactive.o : Bindings.hi
Interactive.o : SExpParser.hi
Interactive.o : ContinuationPassing.hi
# DO NOT DELETE: End of Haskell dependencies
