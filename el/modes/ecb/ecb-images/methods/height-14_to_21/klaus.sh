#!/usr/bin/bash


# for file in `find . -name "*.xpm" -print $*`; do
#     echo $file
# done

ECBPWD=`pwd`
XECBMETHODSDIR=/C/Programme/XEmacs/cvs-package-tree/packages/xemacs-packages/ecb/ecb-images/methods

for file in *.xpm; do
    cd $ECBPWD
    echo $file
    MYPWD= `pwd`
    echo "Current path: $MYPWD"
    cp $file $XECBMETHODSDIR/height-14_to_21
    cd $XECBMETHODSDIR/height-15_to_21
    rm $file
    cvs remove $file
    cd $XECBMETHODSDIR/height-14_to_21
    cvs add $file
done

# cd $XECBMETHODSDIR/height-15_to_21
# cvs commit -m "Renamed height-15_to_21 to height-14_to_21"
 
# cd $XECBMETHODSDIR/height-14_to_21
# cvs commit -m "Renamed height-15_to_21 to height-14_to_21"

# cd $PWD