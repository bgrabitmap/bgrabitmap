@echo off
set DOCSPATH=web
if not exist %DOCSPATH% mkdir %DOCSPATH%
echo Removing previously generated pages...
del %DOCSPATH%\*.html
del %DOCSPATH%\*.dot
del %DOCSPATH%\*.svg
del /q %DOCSPATH%\tipuesearch

pasdoc @pasdoc.cfg -X --graphviz-uses --link-gv-uses=svg --graphviz-classes --link-gv-classes=svg

echo Generating graphs...
call dot.bat -Grankdir=LR -T svg %DOCSPATH%/GVUses.dot > %DOCSPATH%/GVUses.svg
call dot.bat -Grankdir=LR -T svg %DOCSPATH%/GVClasses.dot > %DOCSPATH%/GVClasses.svg

pause