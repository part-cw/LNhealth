#!/bin/sh

# Move files to Sandbox
mkdir sandbox
git mv *.png sandbox/
git mv *.sx sandbox/
git mv *.csv sandbox/
git mv admin.db sandbox/
git add sandbox/

# fix some variable names in main.sx
sed -i '' 's/store/lnhealth:store/g' sandbox/main.sx
sed -i '' 's/sxrun/lnhealth:sxrun/g' sandbox/main.sx
sed -i '' 's/ gui/ lnhealth:gui/g' sandbox/main.sx
git add sandbox/main.sx

# Create embed file
ls -1 sandbox/* > files.tmp
sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g' files.tmp > EMBED
rm files.tmp
git add EMBED

# Add main.scm
cat <<_EOF > main.scm
(main
;; initialization
  (lambda (w h) (lnhealth-init w h))
;; events
  (lambda (t x y) (lnhealth-events t x y))
;; termination
  (lambda () (lnhealth-terminate))
;; suspend
  (lambda () (lnhealth-suspend))
;; resume
  (lambda () (lnhealth-resume))
)
;; eof
_EOF
git add main.scm

# Add VERSION
if [ ! -f VERSION ]; then
  echo "1.0" > VERSION
fi
git add VERSION

# Add MODULES
if [ -f MODULES ]; then
  sed -i '' 's/$/ lnhealth/' MODULES
else
  echo "lnhealth" > MODULES
fi
git add MODULES

# eof
