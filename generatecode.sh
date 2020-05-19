#!/bin/bash
rm -f bgrabitmap/generated*.inc
lazbuild bgrabitmap/bgrabitmappack.lpk dev/colorspace/generatecolorspaces.lpi
dev/colorspace/generatecolorspaces
if [ -f generatedcolorspace.inc ]; then
    echo "Colorspaces successfully generated."
    mv generatedcolorspace.inc bgrabitmap/generatedcolorspace.inc
else
    echo "Error generating colorspaces."
    exit 1
fi
lazbuild bgrabitmap/bgrabitmappack4nogui.lpk dev/parseunicode/parseunicodeclasses.lpi
cd dev/parseunicode
./parseunicodeclasses
cd ../..
if [ -f dev/parseunicode/generatedkerningfallback.inc ]; then
    rm dev/parseunicode/generatedkerningfallback.inc
    echo "Unicode data successfully generated."
    mv dev/parseunicode/generated*.inc bgrabitmap
else
    echo "Error generating Unicode data."
    exit 1
fi

