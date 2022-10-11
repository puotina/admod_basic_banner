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
v3