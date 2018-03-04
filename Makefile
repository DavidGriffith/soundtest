# This Makefile is designed specifically for building programs written
# in Inform.  Feel free to adopt and adapt it for your own purposes.
#

VERSION = r0
BINNAME = soundtest
EXTENSION = .z5
BLORB_EXTENSION = .zblorb

INFORM = inform
BLORB = pblorb.pl
#BLORB = cBlorb

NODEBUG = -~D~S
DEBUG = -D

GLULX = -G
SWITCHES = -iecd2 '$$MAX_OBJECTS=768'

DISTNAME = $(BINNAME)-$(VERSION)
distdir = $(DISTNAME)

#exclude = --exclude=misc --exclude=junk

blurbfile = $(BINNAME).blurb

$(BINNAME): blorb
	$(INFORM) $(SWITCHES) $(DEBUG) $(BINNAME).inf $(BINNAME)$(EXTENSION)

nodebug: blorb
	$(INFORM) $(SWITCHES) $(NODEBUG) $(BINNAME).inf $(BINNAME)$(EXTENSION)

blorb:
	touch $(BINNAME)$(EXTENSION) $(BINNAME)$(BLORB_EXTENSION)
	$(BLORB) $(BINNAME).blurb $(BINNAME)$(BLORB_EXTENSION) > blurb.inf

clean:
	rm -f *zblorb blurb.inf

help:
	@echo "Makefile for Inform compilation"
	@echo "Targets:"
	@echo "  $(BINNAME)	Build the game."
	@echo "  nodebug	Build the game without debugging code."
	@echo "  blorb		Build Blorb file."
	@echo "  clean		Get rid of temporary files."
	@echo "  help		This help message."
	@echo "Default target is \"$(BINNAME)\""
