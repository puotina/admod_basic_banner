a=("" "y" $((-1)) $((1)))
a[0]=$((2 * 9))
a[2]="abx"
a[4]="5""${a[0]}"
"echo" "-e" "${a[0]}" "${a[1]}" "${a[2]}" "${a[3]}" "${a[4]}"
a=($((1)) $((2)) $((3)))
"echo"