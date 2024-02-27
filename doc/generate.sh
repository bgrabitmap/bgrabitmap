if [ ! -d "web" ]; then
	mkdir web
fi
echo Removing previously generated pages...
rm web/*.html
./pasdoc @pasdoc.cfg --graphviz-uses --link-gv-uses=svg --graphviz-classes --link-gv-classes=svg