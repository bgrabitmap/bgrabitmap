DOCSPATH=web
if [ ! -d "$DOCSPATH" ]; then
	mkdir $DOCSPATH
fi
echo Removing previously generated pages...
rm $DOCSPATH/*.html
rm $DOCSPATH/*.dot
rm $DOCSPATH/*.svg

./pasdoc @pasdoc.cfg -X --graphviz-uses --link-gv-uses=svg --graphviz-classes --link-gv-classes=svg

perl -ni -e 'print unless /"(SysUtils|Classes|BGRAGraphics|BGRAClasses|BGRABitmapTypes|Math|Types|FPImgCanv|FPImage|fgl)"/i' $DOCSPATH/GVUses.dot

perl -ni -e 'print unless /"(TObject)"/i' $DOCSPATH/GVClasses.dot

dot -Grankdir=LR -T svg $DOCSPATH/GVUses.dot > $DOCSPATH/GVUses.svg
dot -Grankdir=LR -T svg $DOCSPATH/GVClasses.dot > $DOCSPATH/GVClasses.svg

perl -i -pe 's|(<h1 class="allitems">All Units</h1>)|$1\n<p><a href="GVUses.svg">🔍 Dependency graph</a></p>|' $DOCSPATH/AllUnits.html
perl -i -pe 's|(<h1 class="allitems">Class Hierarchy</h1>)|$1\n<p><a href="GVClasses.svg">🔍 Hierarchy graph</a></p>|' $DOCSPATH/ClassHierarchy.html
perl -i -pe 's|(<h1 class="unit">Unit ([A-Za-z][A-Za-z0-9_]+)</h1>)|<p style="float: right; margin-block-start: 0.5em"><a href="https://github.com/bgrabitmap/bgrabitmap/blob/master/bgrabitmap/\L$2\E.pas">📄 Source</a></p>\n$1|' $DOCSPATH/*.html
perl -i -pe 's|Classes, Interfaces, Objects and Records|Structures|' $DOCSPATH/*.html