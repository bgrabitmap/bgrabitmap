DOCSPATH=web
if [ ! -d "$DOCSPATH" ]; then
	mkdir $DOCSPATH
else
	echo Removing previously generated pages...
	rm $DOCSPATH/*.html
	rm $DOCSPATH/*.dot
	rm $DOCSPATH/*.svg
	if [ -d "$DOCSPATH/tipuesearch" ]; then
		rm $DOCSPATH/tipuesearch/*
	fi
fi

./pasdoc @pasdoc.cfg -X --graphviz-uses --link-gv-uses=svg --graphviz-classes --link-gv-classes=svg

echo Generating graphs...
testcmd () {
    command -v "$1" >/dev/null
}
if testcmd dot; then
	DOT="dot"
else
	DOT="./dot.bat"
fi
perl -ni -e 'print unless /"(SysUtils|Classes|BGRAGraphics|BGRAClasses|BGRABitmapTypes|Math|Types|FPImgCanv|FPImage|fgl)"/i' $DOCSPATH/GVUses.dot
perl -ni -e 'print unless /"(TObject)"/i' $DOCSPATH/GVClasses.dot
$DOT -Grankdir=LR -T svg $DOCSPATH/GVUses.dot > $DOCSPATH/GVUses.svg
$DOT -Grankdir=LR -T svg $DOCSPATH/GVClasses.dot > $DOCSPATH/GVClasses.svg

echo Formatting HTML...
cp navigation.js $DOCSPATH
perl -i -pe '
($filename) = $ARGV =~ m|([^/]+)\.html$|;
s|Classes, Interfaces, Objects and Records|Structures|;
s|(<h2 class="description">Description</h2>)|<script type="text/javascript" src="navigation.js"></script>\n$1|;
s|(<h1 class="allitems">[\w ]+</h1>)|$1\n<script type="text/javascript" src="navigation.js"></script>|;
s|(<h1 class="unit">Unit ([A-Za-z][A-Za-z0-9_]+))</h1>|$1\n<p class="float-boton"><a class="boton" href="https://github.com/bgrabitmap/bgrabitmap/blob/master/bgrabitmap/\L$2\E.pas">📄 Source code</a></p></h1>|;
s|<li><a href="AllIdentifiers.html">Identifiers</a></li><li><a href="GVUses.svg">Unit dependency graph</a></li><li><a href="GVClasses.svg">Classes hierarchy graph</a></li>|<li><a href="AllIdentifiers.html">All Identifiers</a></li>|;
s|"pasdoc\.css"|"pasdoc.css?v=4"|;
s|href="\Q$filename\E.html#([^"]+)"|href="#$1"|g;
' $DOCSPATH/*.html

echo Adding buttons for graphs...
perl -i -pe 's|(<h1 class="allitems">All Units)</h1>|$1\n<p class="float-boton"><a class="boton" href="GVUses.svg">🔍 Dependency graph</a></p></h1>|' $DOCSPATH/AllUnits.html
perl -i -pe 's|(<h1 class="allitems">Class Hierarchy)</h1>|$1\n<p class="float-boton"><a class="boton" href="GVClasses.svg">🔍 Hierarchy graph</a></p></h1>|' $DOCSPATH/ClassHierarchy.html