all: generate compile

TargetCPU=$$(arch)
TargetOS=linux
FPCVer=$$(fpc -h | head -n 1 | awk '{print $$5;}')
LibSubDir=$(TargetCPU)-$(TargetOS)/$(FPCVer)

clean: clean_bgrabitmap clean_generate

clean_bgrabitmap:
	rm -f "bgrabitmap/generatedcolorspace.inc"
	rm -f "bgrabitmap/generatedunicode.inc"
	rm -f "bgrabitmap/generatedutf8.inc"
	rm -rf "bgrabitmap/lib/$(LibSubDir)"
	rm -rf "bgrabitmap/lib4nogui/$(LibSubDir)"
	rm -rf "bgrabitmap/backup"

clean_generate:
	rm -f "dev/colorspace/generatecolorspaces"
	rm -f "dev/colorspace/generatedcolorspace.inc"
	rm -rf "dev/colorspace/lib"
	rm -rf "dev/colorspace/backup"
	rm -f "dev/parseunicode/parseunicodeclasses"
	rm -f "dev/parseunicode/generatedunicode.inc"
	rm -f "dev/parseunicode/generatedutf8.inc"
	rm -f "dev/parseunicode/generatedkerningfallback.inc"
	rm -rf "dev/parseunicode/lib"
	rm -rf "dev/parseunicode/backup"

generate: bgrabitmap/generatedcolorspace.inc bgrabitmap/generatedunicode.inc

bgrabitmap/generatedcolorspace.inc: dev/colorspace/generatecolorspaces
	cd dev/colorspace; ./generatecolorspaces
	cp dev/colorspace/generatedcolorspace.inc bgrabitmap/generatedcolorspace.inc
dev/colorspace/generatecolorspaces: dev/colorspace/generatecolorspaces.lpr dev/colorspace/unitmakerunit.pas
	lazbuild dev/colorspace/generatecolorspaces.lpi

bgrabitmap/generatedunicode.inc: dev/parseunicode/parseunicodeclasses dev/parseunicode/ArabicShaping.txt dev/parseunicode/BidiBrackets.txt dev/parseunicode/BidiMirroring.txt dev/parseunicode/UnicodeData.txt
	cd dev/parseunicode; ./parseunicodeclasses
	cp dev/parseunicode/generatedunicode.inc bgrabitmap/generatedunicode.inc
	cp dev/parseunicode/generatedutf8.inc bgrabitmap/generatedutf8.inc
dev/parseunicode/parseunicodeclasses: dev/parseunicode/parseunicodeclasses.lpr
	lazbuild dev/parseunicode/parseunicodeclasses.lpi

compile: force bgrabitmappack bgrabitmappack4nogui
force:
	#lazbuild will check what to rebuild
bgrabitmappack: bgrabitmap/bgrabitmappack.lpk
	lazbuild bgrabitmap/bgrabitmappack.lpk
bgrabitmappack4nogui: bgrabitmap/bgrabitmappack4nogui.lpk
	lazbuild bgrabitmap/bgrabitmappack4nogui.lpk

