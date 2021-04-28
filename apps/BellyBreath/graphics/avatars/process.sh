
inkscape=/Applications/Inkscape.app/Contents/Resources/bin/inkscape

xa=94
ya=743
# 335
xb=338
yb=955

tmpsize=600
finalsize=136

rm *.png 2> /dev/null

here=`pwd`
files=`ls -1 *.svg`

for f in $files; do
  echo $f..
  tgt=`basename "$f" | sed 's/svg$/png/'`
  $inkscape -z "$here/$f" -w $tmpsize -a $xa:$ya:$xb:$yb -e "$here/$tgt"
  convert "$here/$tgt" -resize ${finalsize}x${finalsize} "png32:$here/../../textures/$tgt"
  rm "$here/$tgt"
done

exit

# proof - for development only 

johnnys="smile happy worry sad blow breathe"
avatars="redhead pigtail royal nerd emo super emogirl"
eyes="up down straight"

for j in $johnnys; do
  for e in $eyes; do
    for a in $avatars; do
      cmpfile=tmp-${j}-${e}-${a}.png
      echo "$cmpfile..."
      convert \
        -page +0+0 output/johnny-${j}.png \
        -page +0+0 output/eyes-${e}.png \
        -page +0+0 output/avatar-$a.png \
        -layers merge +repage $cmpfile 
      #composite output/avatar-$a.png output/eyes-${e}.png output/johnny-${j}.png $cmpfile 
    done
  done
done

montage tmp-*.png johnny-proof.png
rm tmp-*.png

