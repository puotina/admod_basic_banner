# Function call
function func1 {
  local p1
  local p2
  p1="$1"
  p2="$2"
  "echo" "-e" "$p1" "$p2"
}
"func1" "Hello" "World"
# Global and l