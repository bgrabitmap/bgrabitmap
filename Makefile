ifeq ($(OS),Windows_NT)     # true for Windows_NT or later
  COPY := winmake\copyfile
  REMOVE := winmake\remove
  REMOVEDIR := winmake\removedir
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

all: generate compile

install: not_installable
uninstall: not_installable

not_installable:
	echo "The library cannot be installed on the system but statically linked to another Lazarus package or application."

clean: clean_bgrabitmap clean_generate

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

generate: bgrabitmap/generatedcolorspace.inc bgrabitmap/generatedunicode.inc

bgrabitmap/generatedcolorspace.inc: dev/colorspace/generatecolorspaces.lpr dev/colorspace/unitmakerunit.pas
	lazbuild dev/colorspace/generatecolorspaces.lpi
	cd dev $(THEN) cd colorspace $(THEN) $(RUN)generatecolorspaces
	$(COPY) dev/colorspace/generatedcolorspace.inc bgrabitmap/generatedcolorspace.inc

bgrabitmap/generatedunicode.inc: dev/parseunicode/parseunicodeclasses.lpr dev/parseunicode/ArabicShaping.txt dev/parseunicode/BidiBrackets.txt dev/parseunicode/BidiMirroring.txt dev/parseunicode/UnicodeData.txt
	lazbuild dev/parseunicode/parseunicodeclasses.lpi
	cd dev $(THEN) cd parseunicode $(THEN) $(RUN)parseunicodeclasses
	$(COPY) dev/parseunicode/generatedunicode.inc bgrabitmap/generatedunicode.inc
	$(COPY) dev/parseunicode/generatedutf8.inc bgrabitmap/generatedutf8.inc

compile: BGRABitmapPack BGRABitmapPack4NoGUI
lazbuild:
	#lazbuild will determine what to recompile
BGRABitmapPack: lazbuild bgrabitmap/bgrabitmappack.lpk
	lazbuild bgrabitmap/bgrabitmappack.lpk
BGRABitmapPack4NoGUI: lazbuild bgrabitmap/bgrabitmappack4nogui.lpk
	lazbuild bgrabitmap/bgrabitmappack4nogui.lpk

