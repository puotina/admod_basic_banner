[ -e "Makefile" ]
ex=$((!$?))
"echo" "-e" "$ex"
[ -e "Makefile" ]
if [ -e "Makefile" ]; then
  "echo" "-e" "Yes"
fi
if [ 