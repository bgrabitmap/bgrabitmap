#!/bin/bash
#git checkout HEAD~ -- <file to cancel>
git add .
git status
echo "Type commit description (or press Enter to cancel):"
read commitdesc
if test -z "$commitdesc"
then
  git reset --
else
  git commit -m "$commitdesc"
fi
cd ..
