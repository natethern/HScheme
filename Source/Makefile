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

default: build


# Install Directories

prefix			= /usr
datadir			= $(prefix)/share
libdir			= $(prefix)/lib
bindir			= $(prefix)/bin
importdir		= $(datadir)/haskell-import


# Tools

HC				= ghc
HCI				= ghci
HC_PKG			= ghc-pkg
HDOC			= haddock -h


# Build Files

PACKAGENAME		= HScheme
PACKAGEFILE		= $(PACKAGENAME).ghc-pkg
LIBNAME			= $(PACKAGENAME)
LIBFILE_REL		= lib$(LIBNAME).a
LIBFILE_PROF	= lib$(LIBNAME)_p.a
HROOT			= Org/Org/Semantic
HIER			= $(HROOT)/HScheme
COMMAND_REL		= hscheme
COMMAND_PROF	= hscheme_p


# Flags

HCPACKAGES		= -package HBase
HCIMPORTDIRS	= -i.
HCOPTS			= -fno-implicit-prelude -package-name $(PACKAGENAME)
HCFLAGS			= $(HCOPTS) $(HCPACKAGES) $(HCIMPORTDIRS)
HCPROFOPTS		= -prof -hisuf p_hi -osuf p_o
HCFLAGS_REL		=
HCFLAGS_PROF	= $(HCPROFOPTS) -auto-all +RTS -K32M -RTS
LDH				= $(HC) $(HCPACKAGES)
LDFLAGS			=


# Sources

LIB_HC_SRCS			= \
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
	$(HIER).hs

HC_SRCS		= $(LIB_HC_SRCS) InteractiveMain.hs CGIMain.hs


# Install

postinstall: install $(PACKAGEFILE)
	-$(HC_PKG) -r $(PACKAGENAME)
	$(HC_PKG) -a < $(PACKAGEFILE)

install:	\
	$(libdir)/$(LIBFILE_REL)	\
	$(libdir)/$(LIBFILE_PROF)	\
	$(patsubst %.hs,$(importdir)/%.hi,$(LIB_HC_SRCS))	\
	$(patsubst %.hs,$(importdir)/%.p_hi,$(LIB_HC_SRCS))	\
	$(bindir)/$(COMMAND_REL)	\
	$(datadir)/hscheme/init.pure.scm	\
	$(datadir)/hscheme/init.full.scm


# Test

test: bin/$(COMMAND_REL)
	./$<

test_p: bin/$(COMMAND_PROF)
	./$< +RTS -p -RTS


# Documentation

doc: Doc/index.html

# --source="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/hscheme/Source/"
Doc/index.html: $(LIB_HC_SRCS)
	mkdir -p Doc
	$(HDOC) --no-implicit-prelude \
		--html	\
		-o Doc/ $^


# Building

build: $(LIBFILE_REL) $(LIBFILE_PROF) bin/$(COMMAND_REL) bin/$(COMMAND_PROF) $(PACKAGEFILE)

cgi: interpret.cgi

interpret.cgi: CGIMain.o $(LIBFILE_REL) $(libdir)/libHBase.a
	$(LDH) $(filter %.o,$^) $(LIBFILE_REL) $(LDFLAGS) -o $@

bin/$(COMMAND_REL): InteractiveMain.o $(LIBFILE_REL) $(libdir)/libHBase.a
	mkdir -p bin
	$(LDH) $(filter %.o,$^) $(LIBFILE_REL) $(LDFLAGS) -o $@

bin/$(COMMAND_PROF): InteractiveMain.p_o $(LIBFILE_PROF) $(libdir)/libHBase_p.a
	mkdir -p bin
	$(LDH) $(HCPROFOPTS) $(filter %.p_o,$^) $(LIBFILE_PROF) $(LDFLAGS) $(libdir)/libHBase_p.a -o $@

$(LIBFILE_REL): $(patsubst %.hs,%.o,$(LIB_HC_SRCS))
	rm -f $@
	ar cru $@ $(filter %.o,$^)
	ranlib $@

$(LIBFILE_PROF): $(patsubst %.hs,%.p_o,$(LIB_HC_SRCS))
	rm -f $@
	ar cru $@ $(filter %.p_o,$^)
	ranlib $@


# Clean

clean: haskellclean
	rm -f Makefile.bak
	rm -f *.ghc-pkg
	rm -rf Doc bin
	rm -f interpret.cgi
	rm -f *.prof
	rm -f $(LIBFILE_REL) $(LIBFILE_PROF) $(PACKAGEFILE)

haskellclean: $(HROOT)
	for ff in '*' '$(HIER)/*' '$(HIER)/*/*';do rm -f $${ff}.hi $${ff}.o $${ff}.p_hi $${ff}.p_o; done


# Standard Suffix Rules for Installing

$(datadir)/hscheme/%: %
	install -d -m 755 $(dir $@)
	install -c -p -m 644 $< $@
	touch $@

$(importdir)/%.hi: %.hi
	install -d -m 755 $(dir $@)
	install -c -p -m 644 $< $@
	touch $@

$(importdir)/%.p_hi: %.p_hi
	install -d -m 755 $(dir $@)
	install -c -p -m 644 $< $@
	touch $@

$(libdir)/%.a: %.a
	install -c -p -m 644 $< $@
	ranlib $@
	touch $@

$(bindir)/%: bin/%
	install -c -p -m 755 $< $@
	touch $@


# Standard Suffix Rules for Building

.SUFFIXES: .hs .lhs .o .hi .p_o .p_hi .hc .s .ghc-pkg

%.ghc-pkg: %.ghc-pkg.m4
	m4 \
	 -Dimportdir="$(importdir)" \
	 -Dlibdir="$(libdir)" \
	 -DPACKAGENAME="$(PACKAGENAME)" \
	 -DLIBNAME="$(LIBNAME)" \
	 $< > $@

%.int:
	$(HCI) $(HCFLAGS) $*

%.hi: %.o
	@:

%.p_hi: %.p_o
	@:

%.o: %.hs
	$(HC) $(HCFLAGS) $(HCFLAGS_REL) -c $< -o $@

%.o: %.lhs
	$(HC) $(HCFLAGS) $(HCFLAGS_REL) -c $< -o $@

%.p_o: %.hs
	$(HC) $(HCFLAGS) $(HCFLAGS_PROF) -c $< -o $@

%.p_o: %.lhs
	$(HC) $(HCFLAGS) $(HCFLAGS_PROF) -c $< -o $@


# Automatic Dependency Generation

depend: $(HC_SRCS)
	$(HC) -M -optdep-s -optdepp $(HCFLAGS) $^


# Root S-Link Creation

$(HROOT):
	mkdir -p Org/Org/
	cd Org/Org/; ln -s ../../ Semantic

# DO NOT DELETE: Beginning of Haskell dependencies
Org/Org/Semantic/HScheme/Numerics.o Org/Org/Semantic/HScheme/Numerics.p_o : Org/Org/Semantic/HScheme/Numerics.hs
Org/Org/Semantic/HScheme/Port.o Org/Org/Semantic/HScheme/Port.p_o : Org/Org/Semantic/HScheme/Port.hs
Org/Org/Semantic/HScheme/InputPortParser.o Org/Org/Semantic/HScheme/InputPortParser.p_o : Org/Org/Semantic/HScheme/InputPortParser.hs
Org/Org/Semantic/HScheme/InputPortParser.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/InputPortParser.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/Object.o Org/Org/Semantic/HScheme/Object.p_o : Org/Org/Semantic/HScheme/Object.hs
Org/Org/Semantic/HScheme/Object.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Object.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/Object.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Object.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/Conversions.o Org/Org/Semantic/HScheme/Conversions.p_o : Org/Org/Semantic/HScheme/Conversions.hs
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Conversions.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Conversions.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/Conversions.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Conversions.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Evaluate.o Org/Org/Semantic/HScheme/Evaluate.p_o : Org/Org/Semantic/HScheme/Evaluate.hs
Org/Org/Semantic/HScheme/Evaluate.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Evaluate.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Evaluate.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Evaluate.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/TopLevel.o Org/Org/Semantic/HScheme/TopLevel.p_o : Org/Org/Semantic/HScheme/TopLevel.hs
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/TopLevel.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/TopLevel.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/TopLevel.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/TopLevel.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/SystemInterface.hs
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/SExpParser.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/SExpParser.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/PortProcedures.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/TopLevel.p_hi
Org/Org/Semantic/HScheme/SystemInterface.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/SystemInterface.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme/Procedures.o Org/Org/Semantic/HScheme/Procedures.p_o : Org/Org/Semantic/HScheme/Procedures.hs
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Procedures.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Procedures.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Procedures.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Procedures.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Procedures.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/Lambda.o Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/Lambda.hs
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/Lambda.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/Lambda.p_o : Org/Org/Semantic/HScheme/TopLevel.p_hi
Org/Org/Semantic/HScheme/PortProcedures.o Org/Org/Semantic/HScheme/PortProcedures.p_o : Org/Org/Semantic/HScheme/PortProcedures.hs
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/PortProcedures.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/PortProcedures.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/PortProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/PortProcedures.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Equality.o Org/Org/Semantic/HScheme/Equality.p_o : Org/Org/Semantic/HScheme/Equality.hs
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/Equality.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Equality.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Equality.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Equality.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/SExpChars.o Org/Org/Semantic/HScheme/SExpChars.p_o : Org/Org/Semantic/HScheme/SExpChars.hs
Org/Org/Semantic/HScheme/SExpParser.o Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/SExpParser.hs
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/SExpChars.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/SExpChars.p_hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/SExpParser.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/SExpParser.p_o : Org/Org/Semantic/HScheme/PortProcedures.p_hi
Org/Org/Semantic/HScheme/Syntax.o Org/Org/Semantic/HScheme/Syntax.p_o : Org/Org/Semantic/HScheme/Syntax.hs
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Syntax.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/Syntax.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Syntax.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Syntax.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/Syntax.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/Bindings.o Org/Org/Semantic/HScheme/Bindings.p_o : Org/Org/Semantic/HScheme/Bindings.hs
Org/Org/Semantic/HScheme/Bindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Bindings.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Bindings.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Bindings.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/NumericProcedures.o Org/Org/Semantic/HScheme/NumericProcedures.p_o : Org/Org/Semantic/HScheme/NumericProcedures.hs
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/NumericProcedures.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/NumericProcedures.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/NumericProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/NumericProcedures.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/StandardBindings.hs
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/TopLevel.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/NumericProcedures.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/NumericProcedures.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Equality.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Equality.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Lambda.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Lambda.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/PortProcedures.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/Syntax.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/Syntax.p_hi
Org/Org/Semantic/HScheme/StandardBindings.o : Org/Org/Semantic/HScheme/SExpParser.hi
Org/Org/Semantic/HScheme/StandardBindings.p_o : Org/Org/Semantic/HScheme/SExpParser.p_hi
Org/Org/Semantic/HScheme/FMapBindings.o Org/Org/Semantic/HScheme/FMapBindings.p_o : Org/Org/Semantic/HScheme/FMapBindings.hs
Org/Org/Semantic/HScheme/FMapBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FMapBindings.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/FullProcedures.o Org/Org/Semantic/HScheme/FullProcedures.p_o : Org/Org/Semantic/HScheme/FullProcedures.hs
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FullProcedures.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/FullProcedures.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme/FullProcedures.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme/FullProcedures.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/FullProcedures.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/FullStandardBindings.hs
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/TopLevel.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/StandardBindings.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/StandardBindings.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/FullProcedures.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/FullProcedures.p_hi
Org/Org/Semantic/HScheme/FullStandardBindings.o : Org/Org/Semantic/HScheme/Equality.hi
Org/Org/Semantic/HScheme/FullStandardBindings.p_o : Org/Org/Semantic/HScheme/Equality.p_hi
Org/Org/Semantic/HScheme/IOBindings.o Org/Org/Semantic/HScheme/IOBindings.p_o : Org/Org/Semantic/HScheme/IOBindings.hs
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/IOBindings.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/IOBindings.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/IOBindings.o : Org/Org/Semantic/HScheme/SystemInterface.hi
Org/Org/Semantic/HScheme/IOBindings.p_o : Org/Org/Semantic/HScheme/SystemInterface.p_hi
Org/Org/Semantic/HScheme/SchemeCPS.o Org/Org/Semantic/HScheme/SchemeCPS.p_o : Org/Org/Semantic/HScheme/SchemeCPS.hs
Org/Org/Semantic/HScheme/SchemeCPS.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/SchemeCPS.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/SchemeCPS.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/SchemeCPS.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Interactive.o Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Interactive.hs
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/InputPortParser.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/InputPortParser.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/PortProcedures.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/PortProcedures.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/StandardBindings.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/StandardBindings.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/SExpParser.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/SExpParser.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/FullStandardBindings.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/FullStandardBindings.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/FMapBindings.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/FMapBindings.p_hi
Org/Org/Semantic/HScheme/Interactive.o : Org/Org/Semantic/HScheme/SystemInterface.hi
Org/Org/Semantic/HScheme/Interactive.p_o : Org/Org/Semantic/HScheme/SystemInterface.p_hi
Org/Org/Semantic/HScheme.o Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme.hs
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Numerics.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Numerics.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Port.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Port.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Object.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Object.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Conversions.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Conversions.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Evaluate.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Evaluate.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/TopLevel.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/TopLevel.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/SystemInterface.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/SystemInterface.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Procedures.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Procedures.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Bindings.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Bindings.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/StandardBindings.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/StandardBindings.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/FMapBindings.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/FMapBindings.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/FullStandardBindings.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/FullStandardBindings.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/IOBindings.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/IOBindings.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/SchemeCPS.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/SchemeCPS.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Interactive.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Interactive.p_hi
InteractiveMain.o InteractiveMain.p_o : InteractiveMain.hs
InteractiveMain.o : Org/Org/Semantic/HScheme.hi
InteractiveMain.p_o : Org/Org/Semantic/HScheme.p_hi
CGIMain.o CGIMain.p_o : CGIMain.hs
CGIMain.o : Org/Org/Semantic/HScheme.hi
CGIMain.p_o : Org/Org/Semantic/HScheme.p_hi
# DO NOT DELETE: End of Haskell dependencies
