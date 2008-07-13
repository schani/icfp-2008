DISTNAME = rover
BINFILES = rover grover
SBINFILES =
LIBFILES =
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile Makefile.ocaml
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES = communication statemachines geometry telemetry discrete run
LIBRARIES = str unix
LIBRARIES-grover = lablgtk2/lablgtk
DISTVERSION = $(shell cat VERSION)
#VARIANT=.profile

LIBDIRS = $(shell ocamlc -where)/lablgtk2
BUILDSUBDIRS = 
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include Makefile.ocaml$(VARIANT)

