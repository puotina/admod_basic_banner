# Function call
function func1 {
  local p1
  local p2
  p1="$1"
  p2="$2"
  "echo" "-e" "$p1" "$p2"
}
"func1" "Hello" "World"
# Global and local variables
v1="Global V1"
v2="Global V2"
v3="Global V3"
function func2 {
  local v1
  local p
  p="$1"
  v1="Local ""$p"
  "echo" "-e" "$v1"
  "echo" "-e" "$v2"
  
  v3="V3 Modified."
}
"func2" "Var"
"echo" "-e" "$v1"
"echo" "-e" "$v3"
# Return value
function func3 {
  local num
  num