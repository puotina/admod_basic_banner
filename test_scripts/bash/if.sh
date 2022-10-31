if [ $((2 < 10)) == 1 ]; then
  "echo" "-e" "Yes"
fi
if [ $((1)) == 1 ]; then
  if [ $((0)) == 1 ]; then
    v=$((4 + 1))
  else
    v=$((2))
  f