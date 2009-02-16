#!/usr/bin/bash


for file in `find . -name "*.xpm" -print $*`; do
    newname=`echo $file | sed -e "s|\(.*\)\.xpm$|\1.png|;"`
    convert $file $newname
done

