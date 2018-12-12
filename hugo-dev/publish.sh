#!/bin/sh
git add -p &&\
HUGO_ENV=production hugo -t learn &&\
git add ../hugo/ &&\
git commit -m "$1" &&\
git push
