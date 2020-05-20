all: generate compile

init:
ifeq ($(OS),Windows_NT)     # true for Windows_NT or later
  COPY := batch\copyfile
  REMOVE := batch\remove
  REMOVEDIR := batch\removedir
  THEN := &
  RUN :=
else
  COPY := cp
  REMOVE := rm -f
  REMOVEDIR := rm -rf
  THEN := ;
  RUN := ./
  RUN := $(strip $(RUN))
endif

clean: init clean_bgrabitmap clean_generate

clean_bgrabitmap:
	$(REMOVE) "bgrabitmap/generatedcolorspace.inc"
	$(REMOVE) "bgrabitmap/generatedunicode.inc"
	$(REMOVE) "bgrabitmap/generatedutf8.inc"
	$(REMOVEDIR) "bgrabitmap/lib"
	$(REMOVEDIR) "bgrabitmap/lib4nogui"
	$(REMOVEDIR) "bgrabitmap/backup"

clean_generate:
	$(REMOVE) "dev/colorspace/generatecolorspaces"
	$(REMOVE) "dev/colorspace/generatedcolorspace.inc"
	$(REMOVEDIR) "dev/colorspace/lib"
	$(REMOVEDIR) "dev/colorspace/backup"
	$(REMOVE) "dev/parseunicode/parseunicodeclasses"
	$(REMOVE) "dev/parseunicode/generatedunicode.inc"
	$(REMOVE) "dev/parseunicode/generatedutf8.inc"
	$(REMOVE) "dev/parseunicode/generatedkerningfallback.inc"
	$(REMOVEDIR) "dev/parseunicode/lib"
	$(REMOVEDIR) "dev/parseunicode/backup"

generate: init bgrabitmap/generatedcolorspace.inc bgrabitmap/generatedunicode.inc

bgrabitmap/generatedcolorspace.inc: dev/colorspace/generatecolorspaces
	cd dev $(THEN) cd colorspace $(THEN) $(RUN)generatecolorspaces
	$(COPY) dev/colorspace/generatedcolorspace.inc bgrabitmap/generatedcolorspace.inc
dev/colorspace/generatecolorspaces: dev/colorspace/generatecolorspaces.lpr dev/colorspace/unitmakerunit.pas
	lazbuild dev/colorspace/generatecolorspaces.lpi

bgrabitmap/generatedunicode.inc: dev/parseunicode/parseunicodeclasses dev/parseunicode/ArabicShaping.txt dev/parseunicode/BidiBrackets.txt dev/parseunicode/BidiMirroring.txt dev/parseunicode/UnicodeData.txt
	cd dev $(THEN) cd parseunicode $(THEN) $(RUN)parseunicodeclasses
	$(COPY) dev/parseunicode/generatedunicode.inc bgrabitmap/generatedunicode.inc
	$(COPY) dev/parseunicode/generatedutf8.inc bgrabitmap/generatedutf8.inc
dev/parseunicode/parseunicodeclasses: dev/parseunicode/parseunicodeclasses.lpr
	lazbuild dev/parseunicode/parseunicodeclasses.lpi

compile: init bgrabitmappack bgrabitmappack4nogui
bgrabitmappack: bgrabitmap/bgrabitmappack.lpk
	lazbuild bgrabitmap/bgrabitmappack.lpk
bgrabitmappack4nogui: bgrabitmap/bgrabitmappack4nogui.lpk
	lazbuild bgrabitmap/bgrabitmappack4nogui.lpk

