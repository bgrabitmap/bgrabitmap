SVG actual-tag regression test
==============================

Build test_tags.lpi and run test_tags. This is a console test without a widgetset
or font dependency. For FPC directly, include the BGRABitmap source directory and
pass -dBGRABITMAP_DONT_USE_LAZFREETYPE.

Checks ActualDOMTag for parsed polygons, polylines, rectangles and unknown SVG
elements; preservation of the class-level GetDOMTag API; direct closed/open
TSVGPolypoints construction; both AppendPolygon overloads; and XML serialization
and reloading of tags and point data.

ActualDOMTag reads the tag of the existing DOM node. GetDOMTag remains the class
tag used to create nodes and can be empty for shared or generic element classes.