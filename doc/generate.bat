@echo off
if not exist web mkdir web
del web\*.html
pasdoc @pasdoc.cfg
pause