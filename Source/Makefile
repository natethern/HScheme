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
HCOPTS			= -Wall -fno-warn-unused-matches -fno-implicit-prelude -package-name $(PACKAGENAME)
HCFLAGS			= $(HCOPTS) $(HCPACKAGES) $(HCIMPORTDIRS)
HCPROFOPTS		= -prof -hisuf p_hi -osuf p_o
HCFLAGS_REL		=
HCFLAGS_PROF	= $(HCPROFOPTS) -auto-all +RTS -K32M -RTS
LDH				= $(HC) $(HCPACKAGES)
LDFLAGS			=


# Sources

LIB_HC_SRCS			= \
	$(HIER)/Core/Numerics.hs	\
	$(HIER)/Core/Port.hs	\
	$(HIER)/Core/Build.hs	\
	$(HIER)/Core/Symbol.hs	\
	$(HIER)/Core/Binding.hs	\
	$(HIER)/Core/Object.hs	\
	$(HIER)/Core/Scheme.hs	\
	$(HIER)/Core/Conversions.hs	\
	$(HIER)/Core/Throw.hs	\
	$(HIER)/Core/Stream.hs	\
	$(HIER)/Core.hs	\
	\
	$(HIER)/Interpret/FunctorLambda.hs	\
	$(HIER)/Interpret/SymbolExpression.hs	\
	$(HIER)/Interpret/Assemble.hs	\
	$(HIER)/Interpret/TopLevel.hs	\
	$(HIER)/Interpret/Evaluate.hs	\
	$(HIER)/Interpret.hs	\
	\
	$(HIER)/MacroLib/Syntax.hs	\
	$(HIER)/MacroLib/Macros.hs	\
	$(HIER)/MacroLib/Load.hs	\
	$(HIER)/MacroLib.hs	\
	\
	$(HIER)/Parse/SExpChars.hs	\
	$(HIER)/Parse/SExpParser.hs	\
	$(HIER)/Parse.hs	\
	\
	$(HIER)/RunLib/ArgumentList.hs	\
	$(HIER)/RunLib/Equality.hs	\
	$(HIER)/RunLib/Control.hs	\
	$(HIER)/RunLib/NumericProcedures.hs	\
	$(HIER)/RunLib/PortProcedures.hs	\
	$(HIER)/RunLib/Procedures.hs	\
	$(HIER)/RunLib/FullProcedures.hs	\
	$(HIER)/RunLib/SystemPorts.hs	\
	$(HIER)/RunLib.hs	\
	\
	$(HIER)/Bind/Add.hs	\
	$(HIER)/Bind/Macro.hs	\
	$(HIER)/Bind/TopLevel.hs	\
	$(HIER)/Bind/Run.hs	\
	$(HIER)/Bind/FMapBindings.hs	\
	$(HIER)/Bind.hs	\
	\
	$(HIER)/MainProg/Interactive.hs	\
	$(HIER)/MainProg/Batch.hs	\
	$(HIER)/MainProg.hs	\
	\
	$(HIER)/Imperative/IOSystem.hs	\
	$(HIER)/Imperative/IOLoad.hs	\
	$(HIER)/Imperative/SchemeIO.hs	\
	$(HIER)/Imperative/SchemeCPS.hs	\
	$(HIER)/Imperative.hs	\
	\
	$(HIER).hs

HC_SRCS		= $(LIB_HC_SRCS) BatchMain.hs InteractiveMain.hs CGIMain.hs


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

test: test-full test-pure test_p

test/%.scm.diff: test/%.scm.ref test/%.scm.out
	diff -u $^

test/%.scm.out: test/%.scm bin/$(COMMAND_REL)
	time bin/$(COMMAND_REL) --noinit $< > $@

test-full: \
 test/constants.scm.diff \
 test/helloworld.scm.diff \
 test/several.scm.diff \
 test/syntax.scm.diff \
 test/syntax-rec.scm.diff \
 test/letrec.scm.diff \
 test/binding.scm.diff \
 test/setvar.scm.diff \
 test/boundlate.scm.diff \
 test/factorial.scm.diff \
 bin/$(COMMAND_REL)
	time ./$< test/helloworld.scm

test-pure: bin/$(COMMAND_REL)
	time ./$< --pure --noinit test/helloworld.scm
	time ./$< --pure --noinit test/several.scm
	time ./$< --pure --noinit test/binding.scm
	time ./$< --pure --noinit test/boundlate.scm
	time ./$< --pure --noinit test/factorial.scm
	time ./$< --pure test/helloworld.scm

test_p: bin/$(COMMAND_PROF)
	time ./$< +RTS -p -RTS --noinit test/helloworld.scm
	time ./$< +RTS -p -RTS --noinit test/several.scm
	time ./$< +RTS -p -RTS --noinit test/binding.scm
	time ./$< +RTS -p -RTS --noinit test/setvar.scm
	time ./$< +RTS -p -RTS --noinit test/boundlate.scm
	time ./$< +RTS -p -RTS --noinit test/factorial.scm
	time ./$< +RTS -p -RTS test/helloworld.scm


# Documentation

doc: Doc/index.html

# --source="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/hscheme/Source/"
Doc/index.html: $(LIB_HC_SRCS)
	mkdir -p Doc
	$(HDOC) --no-implicit-prelude \
		--html	\
		-o Doc/ $^


# Building

#build: $(LIBFILE_REL) $(LIBFILE_PROF) bin/$(COMMAND_REL) bin/$(COMMAND_PROF) $(PACKAGEFILE) cgi
build: $(LIBFILE_REL) $(LIBFILE_PROF) bin/$(COMMAND_REL) $(PACKAGEFILE) cgi

cgi: interpret.cgi

interpret.cgi: CGIMain.o $(LIBFILE_REL) $(libdir)/libHBase.a
	$(LDH) $(filter %.o,$^) $(LIBFILE_REL) $(LDFLAGS) -o $@

bin/$(COMMAND_REL): BatchMain.o $(LIBFILE_REL) $(libdir)/libHBase.a
	mkdir -p bin
	$(LDH) $(filter %.o,$^) $(LIBFILE_REL) $(LDFLAGS) -o $@

bin/$(COMMAND_PROF): BatchMain.p_o $(LIBFILE_PROF) $(libdir)/libHBase_p.a
	mkdir -p bin
	$(LDH) $(HCPROFOPTS) $(filter %.p_o,$^) $(LIBFILE_PROF) $(LDFLAGS) -o $@

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
Org/Org/Semantic/HScheme/Core/Numerics.o Org/Org/Semantic/HScheme/Core/Numerics.p_o : Org/Org/Semantic/HScheme/Core/Numerics.hs
Org/Org/Semantic/HScheme/Core/Port.o Org/Org/Semantic/HScheme/Core/Port.p_o : Org/Org/Semantic/HScheme/Core/Port.hs
Org/Org/Semantic/HScheme/Core/Build.o Org/Org/Semantic/HScheme/Core/Build.p_o : Org/Org/Semantic/HScheme/Core/Build.hs
Org/Org/Semantic/HScheme/Core/Symbol.o Org/Org/Semantic/HScheme/Core/Symbol.p_o : Org/Org/Semantic/HScheme/Core/Symbol.hs
Org/Org/Semantic/HScheme/Core/Binding.o Org/Org/Semantic/HScheme/Core/Binding.p_o : Org/Org/Semantic/HScheme/Core/Binding.hs
Org/Org/Semantic/HScheme/Core/Object.o Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Object.hs
Org/Org/Semantic/HScheme/Core/Object.o : Org/Org/Semantic/HScheme/Core/Numerics.hi
Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Numerics.p_hi
Org/Org/Semantic/HScheme/Core/Object.o : Org/Org/Semantic/HScheme/Core/Port.hi
Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Port.p_hi
Org/Org/Semantic/HScheme/Core/Object.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core/Object.o : Org/Org/Semantic/HScheme/Core/Symbol.hi
Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Symbol.p_hi
Org/Org/Semantic/HScheme/Core/Object.o : Org/Org/Semantic/HScheme/Core/Binding.hi
Org/Org/Semantic/HScheme/Core/Object.p_o : Org/Org/Semantic/HScheme/Core/Binding.p_hi
Org/Org/Semantic/HScheme/Core/Scheme.o Org/Org/Semantic/HScheme/Core/Scheme.p_o : Org/Org/Semantic/HScheme/Core/Scheme.hs
Org/Org/Semantic/HScheme/Core/Scheme.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core/Scheme.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core/Scheme.o : Org/Org/Semantic/HScheme/Core/Object.hi
Org/Org/Semantic/HScheme/Core/Scheme.p_o : Org/Org/Semantic/HScheme/Core/Object.p_hi
Org/Org/Semantic/HScheme/Core/Conversions.o Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Conversions.hs
Org/Org/Semantic/HScheme/Core/Conversions.o : Org/Org/Semantic/HScheme/Core/Numerics.hi
Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Numerics.p_hi
Org/Org/Semantic/HScheme/Core/Conversions.o : Org/Org/Semantic/HScheme/Core/Port.hi
Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Port.p_hi
Org/Org/Semantic/HScheme/Core/Conversions.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core/Conversions.o : Org/Org/Semantic/HScheme/Core/Symbol.hi
Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Symbol.p_hi
Org/Org/Semantic/HScheme/Core/Conversions.o : Org/Org/Semantic/HScheme/Core/Object.hi
Org/Org/Semantic/HScheme/Core/Conversions.p_o : Org/Org/Semantic/HScheme/Core/Object.p_hi
Org/Org/Semantic/HScheme/Core/Throw.o Org/Org/Semantic/HScheme/Core/Throw.p_o : Org/Org/Semantic/HScheme/Core/Throw.hs
Org/Org/Semantic/HScheme/Core/Throw.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core/Throw.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core/Throw.o : Org/Org/Semantic/HScheme/Core/Symbol.hi
Org/Org/Semantic/HScheme/Core/Throw.p_o : Org/Org/Semantic/HScheme/Core/Symbol.p_hi
Org/Org/Semantic/HScheme/Core/Throw.o : Org/Org/Semantic/HScheme/Core/Object.hi
Org/Org/Semantic/HScheme/Core/Throw.p_o : Org/Org/Semantic/HScheme/Core/Object.p_hi
Org/Org/Semantic/HScheme/Core/Throw.o : Org/Org/Semantic/HScheme/Core/Conversions.hi
Org/Org/Semantic/HScheme/Core/Throw.p_o : Org/Org/Semantic/HScheme/Core/Conversions.p_hi
Org/Org/Semantic/HScheme/Core/Stream.o Org/Org/Semantic/HScheme/Core/Stream.p_o : Org/Org/Semantic/HScheme/Core/Stream.hs
Org/Org/Semantic/HScheme/Core/Stream.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core/Stream.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core/Stream.o : Org/Org/Semantic/HScheme/Core/Object.hi
Org/Org/Semantic/HScheme/Core/Stream.p_o : Org/Org/Semantic/HScheme/Core/Object.p_hi
Org/Org/Semantic/HScheme/Core/Stream.o : Org/Org/Semantic/HScheme/Core/Throw.hi
Org/Org/Semantic/HScheme/Core/Stream.p_o : Org/Org/Semantic/HScheme/Core/Throw.p_hi
Org/Org/Semantic/HScheme/Core/Stream.o : Org/Org/Semantic/HScheme/Core/Scheme.hi
Org/Org/Semantic/HScheme/Core/Stream.p_o : Org/Org/Semantic/HScheme/Core/Scheme.p_hi
Org/Org/Semantic/HScheme/Core.o Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core.hs
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Numerics.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Numerics.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Port.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Port.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Build.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Build.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Symbol.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Symbol.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Binding.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Binding.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Object.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Object.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Scheme.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Scheme.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Conversions.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Conversions.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Throw.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Throw.p_hi
Org/Org/Semantic/HScheme/Core.o : Org/Org/Semantic/HScheme/Core/Stream.hi
Org/Org/Semantic/HScheme/Core.p_o : Org/Org/Semantic/HScheme/Core/Stream.p_hi
Org/Org/Semantic/HScheme/Interpret/FunctorLambda.o Org/Org/Semantic/HScheme/Interpret/FunctorLambda.p_o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.hs
Org/Org/Semantic/HScheme/Interpret/SymbolExpression.o Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.hs
Org/Org/Semantic/HScheme/Interpret/SymbolExpression.o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.hi
Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.p_hi
Org/Org/Semantic/HScheme/Interpret/Assemble.o Org/Org/Semantic/HScheme/Interpret/Assemble.p_o : Org/Org/Semantic/HScheme/Interpret/Assemble.hs
Org/Org/Semantic/HScheme/Interpret/Assemble.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Interpret/Assemble.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Interpret/Assemble.o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.hi
Org/Org/Semantic/HScheme/Interpret/Assemble.p_o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.p_hi
Org/Org/Semantic/HScheme/Interpret/Assemble.o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.hi
Org/Org/Semantic/HScheme/Interpret/Assemble.p_o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.o Org/Org/Semantic/HScheme/Interpret/TopLevel.p_o : Org/Org/Semantic/HScheme/Interpret/TopLevel.hs
Org/Org/Semantic/HScheme/Interpret/TopLevel.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.p_o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.p_hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.p_o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.o : Org/Org/Semantic/HScheme/Interpret/Assemble.hi
Org/Org/Semantic/HScheme/Interpret/TopLevel.p_o : Org/Org/Semantic/HScheme/Interpret/Assemble.p_hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.o Org/Org/Semantic/HScheme/Interpret/Evaluate.p_o : Org/Org/Semantic/HScheme/Interpret/Evaluate.hs
Org/Org/Semantic/HScheme/Interpret/Evaluate.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.p_o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.o : Org/Org/Semantic/HScheme/Interpret/Assemble.hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.p_o : Org/Org/Semantic/HScheme/Interpret/Assemble.p_hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.o : Org/Org/Semantic/HScheme/Interpret/TopLevel.hi
Org/Org/Semantic/HScheme/Interpret/Evaluate.p_o : Org/Org/Semantic/HScheme/Interpret/TopLevel.p_hi
Org/Org/Semantic/HScheme/Interpret.o Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret.hs
Org/Org/Semantic/HScheme/Interpret.o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.hi
Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret/FunctorLambda.p_hi
Org/Org/Semantic/HScheme/Interpret.o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.hi
Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret/SymbolExpression.p_hi
Org/Org/Semantic/HScheme/Interpret.o : Org/Org/Semantic/HScheme/Interpret/Assemble.hi
Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret/Assemble.p_hi
Org/Org/Semantic/HScheme/Interpret.o : Org/Org/Semantic/HScheme/Interpret/TopLevel.hi
Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret/TopLevel.p_hi
Org/Org/Semantic/HScheme/Interpret.o : Org/Org/Semantic/HScheme/Interpret/Evaluate.hi
Org/Org/Semantic/HScheme/Interpret.p_o : Org/Org/Semantic/HScheme/Interpret/Evaluate.p_hi
Org/Org/Semantic/HScheme/MacroLib/Syntax.o Org/Org/Semantic/HScheme/MacroLib/Syntax.p_o : Org/Org/Semantic/HScheme/MacroLib/Syntax.hs
Org/Org/Semantic/HScheme/MacroLib/Syntax.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/MacroLib/Syntax.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/MacroLib/Syntax.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/MacroLib/Syntax.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/MacroLib/Macros.o Org/Org/Semantic/HScheme/MacroLib/Macros.p_o : Org/Org/Semantic/HScheme/MacroLib/Macros.hs
Org/Org/Semantic/HScheme/MacroLib/Macros.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/MacroLib/Macros.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/MacroLib/Macros.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/MacroLib/Macros.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/MacroLib/Load.o Org/Org/Semantic/HScheme/MacroLib/Load.p_o : Org/Org/Semantic/HScheme/MacroLib/Load.hs
Org/Org/Semantic/HScheme/MacroLib/Load.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/MacroLib/Load.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/MacroLib/Load.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/MacroLib/Load.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/MacroLib.o Org/Org/Semantic/HScheme/MacroLib.p_o : Org/Org/Semantic/HScheme/MacroLib.hs
Org/Org/Semantic/HScheme/MacroLib.o : Org/Org/Semantic/HScheme/MacroLib/Syntax.hi
Org/Org/Semantic/HScheme/MacroLib.p_o : Org/Org/Semantic/HScheme/MacroLib/Syntax.p_hi
Org/Org/Semantic/HScheme/MacroLib.o : Org/Org/Semantic/HScheme/MacroLib/Macros.hi
Org/Org/Semantic/HScheme/MacroLib.p_o : Org/Org/Semantic/HScheme/MacroLib/Macros.p_hi
Org/Org/Semantic/HScheme/MacroLib.o : Org/Org/Semantic/HScheme/MacroLib/Load.hi
Org/Org/Semantic/HScheme/MacroLib.p_o : Org/Org/Semantic/HScheme/MacroLib/Load.p_hi
Org/Org/Semantic/HScheme/Parse/SExpChars.o Org/Org/Semantic/HScheme/Parse/SExpChars.p_o : Org/Org/Semantic/HScheme/Parse/SExpChars.hs
Org/Org/Semantic/HScheme/Parse/SExpParser.o Org/Org/Semantic/HScheme/Parse/SExpParser.p_o : Org/Org/Semantic/HScheme/Parse/SExpParser.hs
Org/Org/Semantic/HScheme/Parse/SExpParser.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Parse/SExpParser.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Parse/SExpParser.o : Org/Org/Semantic/HScheme/Parse/SExpChars.hi
Org/Org/Semantic/HScheme/Parse/SExpParser.p_o : Org/Org/Semantic/HScheme/Parse/SExpChars.p_hi
Org/Org/Semantic/HScheme/Parse.o Org/Org/Semantic/HScheme/Parse.p_o : Org/Org/Semantic/HScheme/Parse.hs
Org/Org/Semantic/HScheme/Parse.o : Org/Org/Semantic/HScheme/Parse/SExpChars.hi
Org/Org/Semantic/HScheme/Parse.p_o : Org/Org/Semantic/HScheme/Parse/SExpChars.p_hi
Org/Org/Semantic/HScheme/Parse.o : Org/Org/Semantic/HScheme/Parse/SExpParser.hi
Org/Org/Semantic/HScheme/Parse.p_o : Org/Org/Semantic/HScheme/Parse/SExpParser.p_hi
Org/Org/Semantic/HScheme/RunLib/ArgumentList.o Org/Org/Semantic/HScheme/RunLib/ArgumentList.p_o : Org/Org/Semantic/HScheme/RunLib/ArgumentList.hs
Org/Org/Semantic/HScheme/RunLib/ArgumentList.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/ArgumentList.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/Equality.o Org/Org/Semantic/HScheme/RunLib/Equality.p_o : Org/Org/Semantic/HScheme/RunLib/Equality.hs
Org/Org/Semantic/HScheme/RunLib/Equality.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/Equality.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/Control.o Org/Org/Semantic/HScheme/RunLib/Control.p_o : Org/Org/Semantic/HScheme/RunLib/Control.hs
Org/Org/Semantic/HScheme/RunLib/Control.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/Control.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/Control.o : Org/Org/Semantic/HScheme/RunLib/ArgumentList.hi
Org/Org/Semantic/HScheme/RunLib/Control.p_o : Org/Org/Semantic/HScheme/RunLib/ArgumentList.p_hi
Org/Org/Semantic/HScheme/RunLib/NumericProcedures.o Org/Org/Semantic/HScheme/RunLib/NumericProcedures.p_o : Org/Org/Semantic/HScheme/RunLib/NumericProcedures.hs
Org/Org/Semantic/HScheme/RunLib/NumericProcedures.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/NumericProcedures.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/PortProcedures.o Org/Org/Semantic/HScheme/RunLib/PortProcedures.p_o : Org/Org/Semantic/HScheme/RunLib/PortProcedures.hs
Org/Org/Semantic/HScheme/RunLib/PortProcedures.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/PortProcedures.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/Procedures.o Org/Org/Semantic/HScheme/RunLib/Procedures.p_o : Org/Org/Semantic/HScheme/RunLib/Procedures.hs
Org/Org/Semantic/HScheme/RunLib/Procedures.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/Procedures.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/FullProcedures.o Org/Org/Semantic/HScheme/RunLib/FullProcedures.p_o : Org/Org/Semantic/HScheme/RunLib/FullProcedures.hs
Org/Org/Semantic/HScheme/RunLib/FullProcedures.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/FullProcedures.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib/FullProcedures.o : Org/Org/Semantic/HScheme/RunLib/Procedures.hi
Org/Org/Semantic/HScheme/RunLib/FullProcedures.p_o : Org/Org/Semantic/HScheme/RunLib/Procedures.p_hi
Org/Org/Semantic/HScheme/RunLib/SystemPorts.o Org/Org/Semantic/HScheme/RunLib/SystemPorts.p_o : Org/Org/Semantic/HScheme/RunLib/SystemPorts.hs
Org/Org/Semantic/HScheme/RunLib/SystemPorts.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/RunLib/SystemPorts.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/RunLib.o Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib.hs
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/ArgumentList.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/ArgumentList.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/Equality.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/Equality.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/Control.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/Control.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/NumericProcedures.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/NumericProcedures.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/PortProcedures.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/PortProcedures.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/Procedures.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/Procedures.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/FullProcedures.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/FullProcedures.p_hi
Org/Org/Semantic/HScheme/RunLib.o : Org/Org/Semantic/HScheme/RunLib/SystemPorts.hi
Org/Org/Semantic/HScheme/RunLib.p_o : Org/Org/Semantic/HScheme/RunLib/SystemPorts.p_hi
Org/Org/Semantic/HScheme/Bind/Add.o Org/Org/Semantic/HScheme/Bind/Add.p_o : Org/Org/Semantic/HScheme/Bind/Add.hs
Org/Org/Semantic/HScheme/Bind/Add.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind/Add.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind/Add.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/Bind/Add.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/Bind/Add.o : Org/Org/Semantic/HScheme/RunLib.hi
Org/Org/Semantic/HScheme/Bind/Add.p_o : Org/Org/Semantic/HScheme/RunLib.p_hi
Org/Org/Semantic/HScheme/Bind/Macro.o Org/Org/Semantic/HScheme/Bind/Macro.p_o : Org/Org/Semantic/HScheme/Bind/Macro.hs
Org/Org/Semantic/HScheme/Bind/Macro.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind/Macro.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind/Macro.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/Bind/Macro.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/Bind/Macro.o : Org/Org/Semantic/HScheme/MacroLib.hi
Org/Org/Semantic/HScheme/Bind/Macro.p_o : Org/Org/Semantic/HScheme/MacroLib.p_hi
Org/Org/Semantic/HScheme/Bind/Macro.o : Org/Org/Semantic/HScheme/Bind/Add.hi
Org/Org/Semantic/HScheme/Bind/Macro.p_o : Org/Org/Semantic/HScheme/Bind/Add.p_hi
Org/Org/Semantic/HScheme/Bind/TopLevel.o Org/Org/Semantic/HScheme/Bind/TopLevel.p_o : Org/Org/Semantic/HScheme/Bind/TopLevel.hs
Org/Org/Semantic/HScheme/Bind/TopLevel.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind/TopLevel.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind/TopLevel.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/Bind/TopLevel.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/Bind/TopLevel.o : Org/Org/Semantic/HScheme/MacroLib.hi
Org/Org/Semantic/HScheme/Bind/TopLevel.p_o : Org/Org/Semantic/HScheme/MacroLib.p_hi
Org/Org/Semantic/HScheme/Bind/TopLevel.o : Org/Org/Semantic/HScheme/Bind/Add.hi
Org/Org/Semantic/HScheme/Bind/TopLevel.p_o : Org/Org/Semantic/HScheme/Bind/Add.p_hi
Org/Org/Semantic/HScheme/Bind/Run.o Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/Bind/Run.hs
Org/Org/Semantic/HScheme/Bind/Run.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind/Run.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/Bind/Run.o : Org/Org/Semantic/HScheme/Parse.hi
Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/Parse.p_hi
Org/Org/Semantic/HScheme/Bind/Run.o : Org/Org/Semantic/HScheme/RunLib.hi
Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/RunLib.p_hi
Org/Org/Semantic/HScheme/Bind/Run.o : Org/Org/Semantic/HScheme/Bind/Add.hi
Org/Org/Semantic/HScheme/Bind/Run.p_o : Org/Org/Semantic/HScheme/Bind/Add.p_hi
Org/Org/Semantic/HScheme/Bind/FMapBindings.o Org/Org/Semantic/HScheme/Bind/FMapBindings.p_o : Org/Org/Semantic/HScheme/Bind/FMapBindings.hs
Org/Org/Semantic/HScheme/Bind/FMapBindings.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind/FMapBindings.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind.o Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind.hs
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Bind/Add.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind/Add.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Bind/Macro.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind/Macro.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Bind/TopLevel.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind/TopLevel.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Bind/Run.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind/Run.p_hi
Org/Org/Semantic/HScheme/Bind.o : Org/Org/Semantic/HScheme/Bind/FMapBindings.hi
Org/Org/Semantic/HScheme/Bind.p_o : Org/Org/Semantic/HScheme/Bind/FMapBindings.p_hi
Org/Org/Semantic/HScheme/MainProg/Interactive.o Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/MainProg/Interactive.hs
Org/Org/Semantic/HScheme/MainProg/Interactive.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/MainProg/Interactive.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/MainProg/Interactive.o : Org/Org/Semantic/HScheme/Parse.hi
Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/Parse.p_hi
Org/Org/Semantic/HScheme/MainProg/Interactive.o : Org/Org/Semantic/HScheme/RunLib.hi
Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/RunLib.p_hi
Org/Org/Semantic/HScheme/MainProg/Interactive.o : Org/Org/Semantic/HScheme/Bind.hi
Org/Org/Semantic/HScheme/MainProg/Interactive.p_o : Org/Org/Semantic/HScheme/Bind.p_hi
Org/Org/Semantic/HScheme/MainProg/Batch.o Org/Org/Semantic/HScheme/MainProg/Batch.p_o : Org/Org/Semantic/HScheme/MainProg/Batch.hs
Org/Org/Semantic/HScheme/MainProg/Batch.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/MainProg/Batch.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/MainProg/Batch.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme/MainProg/Batch.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme/MainProg/Batch.o : Org/Org/Semantic/HScheme/Bind.hi
Org/Org/Semantic/HScheme/MainProg/Batch.p_o : Org/Org/Semantic/HScheme/Bind.p_hi
Org/Org/Semantic/HScheme/MainProg.o Org/Org/Semantic/HScheme/MainProg.p_o : Org/Org/Semantic/HScheme/MainProg.hs
Org/Org/Semantic/HScheme/MainProg.o : Org/Org/Semantic/HScheme/MainProg/Batch.hi
Org/Org/Semantic/HScheme/MainProg.p_o : Org/Org/Semantic/HScheme/MainProg/Batch.p_hi
Org/Org/Semantic/HScheme/Imperative/IOSystem.o Org/Org/Semantic/HScheme/Imperative/IOSystem.p_o : Org/Org/Semantic/HScheme/Imperative/IOSystem.hs
Org/Org/Semantic/HScheme/Imperative/IOSystem.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Imperative/IOSystem.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.o Org/Org/Semantic/HScheme/Imperative/IOLoad.p_o : Org/Org/Semantic/HScheme/Imperative/IOLoad.hs
Org/Org/Semantic/HScheme/Imperative/IOLoad.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.o : Org/Org/Semantic/HScheme/Parse.hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.p_o : Org/Org/Semantic/HScheme/Parse.p_hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.o : Org/Org/Semantic/HScheme/Imperative/IOSystem.hi
Org/Org/Semantic/HScheme/Imperative/IOLoad.p_o : Org/Org/Semantic/HScheme/Imperative/IOSystem.p_hi
Org/Org/Semantic/HScheme/Imperative/SchemeIO.o Org/Org/Semantic/HScheme/Imperative/SchemeIO.p_o : Org/Org/Semantic/HScheme/Imperative/SchemeIO.hs
Org/Org/Semantic/HScheme/Imperative/SchemeIO.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Imperative/SchemeIO.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Imperative/SchemeIO.o : Org/Org/Semantic/HScheme/RunLib.hi
Org/Org/Semantic/HScheme/Imperative/SchemeIO.p_o : Org/Org/Semantic/HScheme/RunLib.p_hi
Org/Org/Semantic/HScheme/Imperative/SchemeCPS.o Org/Org/Semantic/HScheme/Imperative/SchemeCPS.p_o : Org/Org/Semantic/HScheme/Imperative/SchemeCPS.hs
Org/Org/Semantic/HScheme/Imperative/SchemeCPS.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme/Imperative/SchemeCPS.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme/Imperative/SchemeCPS.o : Org/Org/Semantic/HScheme/MainProg.hi
Org/Org/Semantic/HScheme/Imperative/SchemeCPS.p_o : Org/Org/Semantic/HScheme/MainProg.p_hi
Org/Org/Semantic/HScheme/Imperative.o Org/Org/Semantic/HScheme/Imperative.p_o : Org/Org/Semantic/HScheme/Imperative.hs
Org/Org/Semantic/HScheme/Imperative.o : Org/Org/Semantic/HScheme/Imperative/IOSystem.hi
Org/Org/Semantic/HScheme/Imperative.p_o : Org/Org/Semantic/HScheme/Imperative/IOSystem.p_hi
Org/Org/Semantic/HScheme/Imperative.o : Org/Org/Semantic/HScheme/Imperative/IOLoad.hi
Org/Org/Semantic/HScheme/Imperative.p_o : Org/Org/Semantic/HScheme/Imperative/IOLoad.p_hi
Org/Org/Semantic/HScheme/Imperative.o : Org/Org/Semantic/HScheme/Imperative/SchemeIO.hi
Org/Org/Semantic/HScheme/Imperative.p_o : Org/Org/Semantic/HScheme/Imperative/SchemeIO.p_hi
Org/Org/Semantic/HScheme/Imperative.o : Org/Org/Semantic/HScheme/Imperative/SchemeCPS.hi
Org/Org/Semantic/HScheme/Imperative.p_o : Org/Org/Semantic/HScheme/Imperative/SchemeCPS.p_hi
Org/Org/Semantic/HScheme.o Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme.hs
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Core.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Core.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Interpret.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Interpret.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/MacroLib.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/MacroLib.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Parse.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Parse.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/RunLib.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/RunLib.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Bind.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Bind.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/MainProg.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/MainProg.p_hi
Org/Org/Semantic/HScheme.o : Org/Org/Semantic/HScheme/Imperative.hi
Org/Org/Semantic/HScheme.p_o : Org/Org/Semantic/HScheme/Imperative.p_hi
BatchMain.o BatchMain.p_o : BatchMain.hs
BatchMain.o : Org/Org/Semantic/HScheme.hi
BatchMain.p_o : Org/Org/Semantic/HScheme.p_hi
InteractiveMain.o InteractiveMain.p_o : InteractiveMain.hs
InteractiveMain.o : Org/Org/Semantic/HScheme.hi
InteractiveMain.p_o : Org/Org/Semantic/HScheme.p_hi
CGIMain.o CGIMain.p_o : CGIMain.hs
CGIMain.o : Org/Org/Semantic/HScheme.hi
CGIMain.p_o : Org/Org/Semantic/HScheme.p_hi
# DO NOT DELETE: End of Haskell dependencies
