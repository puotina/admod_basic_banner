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
  local _0
  num="$1"
  if [ $(($num == 0)) == 1 ]; then
    "echo" "-ne" $((1))
    return
  else
    _0=$("fact" $(($num - 1)))
    "echo" "-ne" $(($_0 * $num))
    return
  fi
}
"echo" "-e" $("fact" $((5)))
function fibonacci {
  local _1
  local num
  local _0
  num="$1"
  if [ $(($num == 0