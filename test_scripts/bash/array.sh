a=("" "y" $((-1)) $((1)))
a[0]=$((2 * 9))
a[2]="abx"
a[4]="5""${a[0]}"
"echo" "-e" "${a[0]}" "${a[1]}" "${a[2]}" "${a[3]}" "${a[4]}"
a=($((1)) $((2)) $((3)))
"echo" "-e" "${a[0]}" "${a[1]}" "${a[2]}"
_0="10""${a[0]}"
"echo" "-e" $(($_0 * 2))
"echo" "-e" "${#a[@]}"
_1="${#a[@]