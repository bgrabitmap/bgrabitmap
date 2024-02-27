DOCSPATH=web
if [ ! -d "$DOCSPATH" ]; then
	mkdir $DOCSPATH
fi
echo Removing previously generated pages...
rm $DOCSPATH/*.html
rm $DOCSPATH/*.dot
rm $DOCSPATH/*.svg

./pasdoc @pasdoc.cfg -X --graphviz-uses --link-gv-uses=svg --graphviz-classes --link-gv-classes=svg

perl -ni -e 'print unless /"(SysUtils|Classes|BGRAClasses|BGRABitmapTypes|Math|Types|FPImgCanv|FPImage|fgl)"/i' $DOCSPATH/GVUses.dot

dot -Grankdir=LR -T svg $DOCSPATH/GVUses.dot > $DOCSPATH/GVUses.svg
dot -Grankdir=LR -T svg $DOCSPATH/GVClasses.dot > $DOCSPATH/GVClasses.svg
