GTK2 drawable-offset regression test
===================================

Build test_groupbox.lpi with lazbuild --ws=gtk2. Run under a real X display
or xvfb-run, passing a screenshot filename and a drawing mode (0 through 3):

    xvfb-run -a ./test_groupbox screenshot.png 0

Modes: 0 = Draw; 1 = DataDrawTransparent/DataDrawOpaque; 2 = DrawPart;
3 = stretched Draw. Run all four modes.

The test compares standard canvas rectangles with transparent and opaque BGRA
rectangles on the form and in group, radio and check groups, with and without
captions. It checks pixels throughout the height at two vertical positions.
Exit status is 0 when all sampled pixels have their expected colors, otherwise 1.

Validated with Lazarus 3.0 and 4.8, FPC 3.2.2, GTK2. Before the drawable-offset
fix, captioned groups fail; with the fix, all four modes pass.