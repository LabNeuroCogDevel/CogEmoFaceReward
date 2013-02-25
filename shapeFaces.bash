#!/usr/bin/env bash
#
set -xe

#faces directory
indir=$HOME/nimstim
#outdir=/home/foranw/remotes/B/bea_res/Personal/Will/CogEmoFaceReward/faces/
outdir=$HOME/nimstim_transform
scale=35   # from original (506x650)
# what face to use for each emotion (open mouth faces)
happy=HA_O 
fear=FE_O
neutral=NE_C

cd $indir
ls *{$happy,$fear,$neutral}*{bmp,BMP} |
# remove bad images
grep -v 01 | 
# list number of faces and name (like 3 02F)
cut -f1 -d_ | sort| uniq -c |
# check all three faces exist
# list like xxGender y: eg.  02F 1 -- where 02F is the image to use, and 1 is the number we'll call it by
perl -lne 'if(m/^\s+3 (\d\d(F|M))/){push @{$a{$2}}, $1} END{ for $n ( @{$a{F}}[0..12],@{$a{M}}[0..12] ){print "$n ",++$i} }' |
while read inf outf;  do
    for emo in happy fear neutral; do 
	faceType=${!emo}
	od="$outdir/$scale/$emo"
	[ ! -d "$od" ] && mkdir -p $od

        # make sure the size is the same for all
        # take an elipse out of the bmp, scale it by $scale, and call it a png
	#convert -size 506x650 $indir/${inf}_$faceType* \( -size 506x650 xc:none -fill black -draw 'ellipse 250,325 250,325 0,360' \) -alpha Set \
	#    -compose Dst_In  -composite -scale ${scale}% $od/${emo}_$outf.png
	    
        # try out circle (looks more reasonable for scrambled)
	convert \( -size 650x650 -gravity center -extent 650x650 $indir/${inf}_$faceType* \) \( -size 650x650 xc:none -fill black -draw 'circle 325,325 554,554' \) -alpha Set \
		-compose Dst_In -composite -scale ${scale}% $od/${emo}_$outf.png

    done
done

exit 1

# run matlab
echo 'run matlab:' 
echo "matlab -nodisplay -nojvm -r 'scale=$scale; scrambleFace.m'"
read

# make spheres again
# sizes is the widthxheight and then half of that (for sphere center)
sizes=($(identify $outdir/$scale/scram/scram_1.png | perl -lne '$,=" "; print $1,$2,int($1/2),int($2/2) if m/(\d+)x(\d+)/'))
for f in $outdir/$scale/scram/scram_*; do 
    convert -size ${sizes[0]}x${sizes[1]} $f \( -size ${sizes[0]}x${sizes[1]} xc:none -fill black -draw "ellipse ${sizes[2]},${sizes[3]} ${sizes[2]},${sizes[3]} 0,360" \) -alpha Set -compose Dst_In  -composite $f; 
done

# check our work
ls $outdir/$scale/*/*.png | wc -l
feh $outdir/$scale/*/*.png -i -E 100

cp $outdir/$scale/{scram,happy,fear}/* $outdir/

