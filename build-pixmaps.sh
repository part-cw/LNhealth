#!/bin/sh

texture_dir=textures
if [ ! -d "./$texture_dir" ]; then
    mkdir ./$texture_dir
fi

#Font family
font=cmssdc

#Small two column labels
size=16
../../utils/txt2scm "PR\\\\[-1mm] bpm"    $font $size label_pr  > ./$texture_dir/label_pr.scm
../../utils/txt2scm "PRl\\\\[-1mm] bpm"    $font $size label_prl  > ./$texture_dir/label_prl.scm
../../utils/txt2scm "SpO2\\\\[-1mm] \\%"    $font $size label_spo2  > ./$texture_dir/label_spo2.scm
../../utils/txt2scm "SPO2l\\\\[-1mm] \\%"    $font $size label_spo2l  > ./$texture_dir/label_spo2l.scm

#Full text
size=24
#needs special attention as iOS doesn't like textures wider than 1024px
../../utils/txt2scm "0\\hspace{2pt}1\\hspace{2pt}2\\hspace{2pt}3\\hspace{2pt}4\\hspace{2pt}5\\hspace{2pt}6\\hspace{2pt}7\\hspace{2pt}8\\hspace{2pt}9\\hspace{2pt}:\\hspace{2pt}.\\hspace{2pt}A\\hspace{2pt}B\\hspace{2pt}C\\hspace{2pt}D\\hspace{2pt}E\\hspace{2pt}F\\hspace{2pt}G\\hspace{2pt}H\\hspace{2pt}I\\hspace{2pt}J\\hspace{2pt}K\\hspace{2pt}L\\hspace{2pt}M\\hspace{2pt}N\\hspace{2pt}O\\hspace{2pt}P\\hspace{2pt}Q\\hspace{2pt}R\\hspace{2pt}S\\hspace{2pt}T\\hspace{2pt}U\\hspace{2pt}V\\hspace{2pt}W\\hspace{2pt}X\\hspace{2pt}Y\\hspace{2pt}Z\\hspace{2pt}a\\hspace{2pt}b\\hspace{2pt}c\\hspace{2pt}d\\hspace{2pt}e\\hspace{2pt}f\\hspace{2pt}g\\hspace{2pt}h\\hspace{2pt}i\\hspace{2pt}j\\hspace{2pt}k\\hspace{2pt}l\\hspace{2pt}m\\hspace{2pt}n\\hspace{2pt}o\\hspace{2pt}p\\hspace{2pt}q\\hspace{2pt}r\\hspace{2pt}s\\hspace{2pt}t\\hspace{2pt}u\\hspace{2pt}v\\hspace{2pt}w\\hspace{2pt}x\\hspace{2pt}y\\hspace{2pt}z\\hspace{2pt}\_\\hspace{2pt},\\hspace{2pt}!\\hspace{2pt}'\\hspace{2pt}--\\hspace{2pt}(\\hspace{2pt})\\hspace{2pt}/" $font $size ascii24 > ./$texture_dir/ascii24.scm
../../utils/scm2fnt ./$texture_dir/ascii24.scm 48,49,50,51,52,53,54,55,56,57,58,46,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,32,44,33,39,45,40,41,47 > ./$texture_dir/ascii24_fnt.scm
size=16
../../utils/txt2scm "0\\hspace{2pt}1\\hspace{2pt}2\\hspace{2pt}3\\hspace{2pt}4\\hspace{2pt}5\\hspace{2pt}6\\hspace{2pt}7\\hspace{2pt}8\\hspace{2pt}9\\hspace{2pt}:\\hspace{2pt}.\\hspace{2pt}A\\hspace{2pt}B\\hspace{2pt}C\\hspace{2pt}D\\hspace{2pt}E\\hspace{2pt}F\\hspace{2pt}G\\hspace{2pt}H\\hspace{2pt}I\\hspace{2pt}J\\hspace{2pt}K\\hspace{2pt}L\\hspace{2pt}M\\hspace{2pt}N\\hspace{2pt}O\\hspace{2pt}P\\hspace{2pt}Q\\hspace{2pt}R\\hspace{2pt}S\\hspace{2pt}T\\hspace{2pt}U\\hspace{2pt}V\\hspace{2pt}W\\hspace{2pt}X\\hspace{2pt}Y\\hspace{2pt}Z\\hspace{2pt}a\\hspace{2pt}b\\hspace{2pt}c\\hspace{2pt}d\\hspace{2pt}e\\hspace{2pt}f\\hspace{2pt}g\\hspace{2pt}h\\hspace{2pt}i\\hspace{2pt}j\\hspace{2pt}k\\hspace{2pt}l\\hspace{2pt}m\\hspace{2pt}n\\hspace{2pt}o\\hspace{2pt}p\\hspace{2pt}q\\hspace{2pt}r\\hspace{2pt}s\\hspace{2pt}t\\hspace{2pt}u\\hspace{2pt}v\\hspace{2pt}w\\hspace{2pt}x\\hspace{2pt}y\\hspace{2pt}z\\hspace{2pt}\_\\hspace{2pt},\\hspace{2pt}!\\hspace{2pt}'\\hspace{2pt}--\\hspace{2pt}(\\hspace{2pt})\\hspace{2pt}/" $font $size ascii16 > ./$texture_dir/ascii16.scm
../../utils/scm2fnt ./$texture_dir/ascii16.scm 48,49,50,51,52,53,54,55,56,57,58,46,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,32,44,33,39,45,40,41,47 > ./$texture_dir/ascii16_fnt.scm

#Numbers only
size=24
../../utils/txt2scm "0\\ 1\\ 2\\ 3\\ 4\\ 5\\ 6\\ 7\\ 8\\ 9\\ :\\ .\\ (\\ ) \\ /" $font $size num24 > ./$texture_dir/num24.scm
../../utils/scm2fnt ./$texture_dir/num24.scm 48,49,50,51,52,53,54,55,56,57,58,46,40,41,47 > ./$texture_dir/num24_fnt.scm
size=40
../../utils/txt2scm "0\\ 1\\ 2\\ 3\\ 4\\ 5\\ 6\\ 7\\ 8\\ 9\\ :\\ .\\ (\\ ) \\ /\\ \, \\ \_" $font $size num40 > ./$texture_dir/num40.scm
../../utils/scm2fnt ./$texture_dir/num40.scm 48,49,50,51,52,53,54,55,56,57,58,46,40,41,47,32,33 > ./$texture_dir/num40_fnt.scm
size=18
../../utils/txt2scm "0\\ 1\\ 2\\ 3\\ 4\\ 5\\ 6\\ 7\\ 8\\ 9\\ :\\ ." $font $size num18 > ./$texture_dir/num18.scm
../../utils/scm2fnt ./$texture_dir/num18.scm 48,49,50,51,52,53,54,55,56,57,58,46 > ./$texture_dir/num18_fnt.scm

size=12;
../../utils/txt2scm "PhilipsDataLogger \\copyright \\ 2011-2012 UBC \\&  BC Children's Hospital" $font $size copyright > ./$texture_dir/copyright.scm

#eof