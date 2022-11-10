function loop {
  local num
  num="$1"
  "echo" "-e" "$num"
  if [ $(($num > 0)) == 1 ]; then
    "loop" $(($num - 1))  
fi
}
"loop" $((10))
function fact {
  local num