#!/usr/bin/env bash
#
set -xe

#faces directory
indir=$HOME/nimstim
outdir=/home/foranw/remotes/B/bea_res/Personal/Will/CogEmoFaceReward/faces/
scale=50   # from original (506x650)
# what face to use for each emotion (open mouth faces)
 happy=HA_O 
  fear=FE_O
neutral=NE_C

cd $indir
ls *{$happy,$fear,$neutral}*{bmp,BMP}|
 # remove bad images
 grep -v 01 | 
 # list number of faces and name (like 3 02F)
 cut -f1 -d_ | sort|uniq -c|
 # check all three faces exist
 # list like xxGender y: eg.  02F 1 -- where 02F is the image to use, and 1 is the number we'll call it by
 perl -lne 'if(m/^\s+3 (\d\d(F|M))/){push @{$a{$2}}, $1} END{ for $n ( @{$a{F}}[0..9],@{$a{M}}[0..9] ){print "$n ",++$i} }' |
while read inf outf;  do
   for emo in happy fear neutral; do 
    faceType=${!emo}
    od="$outdir/$scale/$emo"
    [ ! -d "$od" ] && mkdir -p $od

    # make sure the size is the same for all
    # take an elipse out of the bmp, scale it by $scale, and call it a png
    convert -size 506x650 $indir/${inf}_$faceType* \( -size 506x650 xc:none -fill black -draw 'ellipse 250,325 250,325 0,360' \) -alpha Set -compose Dst_In  -composite -scale ${scale}% $od/${emo}_$outf.png

   done
done

# run matlab
matlab -nodisplay -nojvm -r 'scale=50; scrambleFace.m'

# make spheres again
for f in $outdir/$scale/scram/scram_*; do 
  convert -size 253x325 $f \( -size 253x325 xc:none -fill black -draw 'ellipse 127,163 127,163 0,360' \) -alpha Set -compose Dst_In  -composite $f; 
done

# check our work
ls $scale/*/*.png | wc -l
feh $scale/*/*.png -i -E 100

mv $outdir/$scale/{scram,happy,fear}/* $outdir/

