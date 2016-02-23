
inkscape=/Applications/Inkscape.app/Contents/Resources/bin/inkscape

tmpsize=1024

crunch_svg() 
{
  svg=$1
  png=$2
  finalsize=$3
  pad=$4
  if [ "X$pad" = "X" ]; then
    pad="20x20"
  fi
  here=`pwd`
  $inkscape -z "$here/$svg" -h $tmpsize -e "$here/tmp.png"
  convert "$here/tmp.png" -trim "$here/tmp2.png"
  convert "$here/tmp2.png" -bordercolor transparent -border $pad "$here/tmp3.png"
  convert "$here/tmp3.png" -resize ${finalsize}x${finalsize} "png32:$here/$png"
  rm tmp.png tmp2.png tmp3.png
}

crunch_svg field.svg ../../textures/field.png 480 0x20
crunch_svg rainbow.svg ../../textures/rainbow.png 480 0x20
crunch_svg cloud.svg ../../textures/cloud.png 250
crunch_svg title.svg ../../textures/title.png 400
crunch_svg buttons.svg ../../textures/buttons.png 480
crunch_svg banner.svg ../../textures/banner.png 480
crunch_svg bcch.svg ../../textures/bcch.png 250
tmpsize=4096
crunch_svg trophy.svg ../../textures/trophy.png 480

#eof
