### Random trial order
 setupOrder.R → FaceITI.csv

### Faces
faces/ and faces_large/

# cirlce
for f in *bmp; do convert -size 506x650 $f \( -size 506x650 xc:none -fill black -draw 'ellipse 250,325 250,325 0,360' \) -alpha Set -compose Dst_In  -composite ${f%.bmp}.png; done
# or just make white trans
# convert -size 506x650 -fuzz 15% -transparent white ${f%.bmp}.png
# size
for f in *.png; do convert -scale 50% $f $f; done

#### EPrime

CogEmoFaceReward.es2

 used animationAttempts/eprime_example/AnimateCanvasIntersection.es2 
 as template for sprite use



#### Matlab

CogEmoFaceReward.m

 http://psychtoolbox.org/PsychtoolboxDownload#hn_Installation_Instructions
 [installed with DownloadPsychtoolbox('C:\psychtoolbox') ]


###  Michael Frank code

Frank/
see Frank/email.txt
