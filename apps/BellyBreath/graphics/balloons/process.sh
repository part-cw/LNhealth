# balloon crunch

inkscape=/Applications/Inkscape.app/Contents/Resources/bin/inkscape

xa=94
ya=743
# 335
xb=338
yb=955

tmpsize=2400
finalsize=500

rm *.png 2> /dev/null

here=`pwd`
files=`ls -1 balloon*.svg`

for f in $files; do
  echo $f..
  tgt=`basename "$f" | sed 's/svg$/png/'`
#  $inkscape -z "$here/$f" -h $tmpsize -a $xa:$ya:$xb:$yb -e "$here/$tgt"
  $inkscape -z "$here/$f" -h $tmpsize -e "$here/$tgt"
  convert "$here/$tgt" -trim "$here/${tgt}2.png"
  convert "$here/${tgt}2.png" -bordercolor transparent -border 20x20 "$here/${tgt}3.png"
  convert "$here/${tgt}3.png" -resize ${finalsize}x${finalsize} "png32:$here/../../textures/$tgt"
#  convert "$here/$tgt" -resize ${finalsize}x${finalsize} "png32:$here/../textures/$tgt"
#  rm "$here/$tgt"
done

rm *.png 2> /dev/null

