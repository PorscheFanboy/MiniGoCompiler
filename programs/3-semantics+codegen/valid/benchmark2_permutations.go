// Print all permutations of [1 .. n]
package main


var n int
var pick[100] bool
var arr[100] int

func Permutations(k int) {
    if (k > n) {
        for i := 1; i <= n; i++ {
            print(arr[i], " ")
        }
        println()
        return
    }
    for i := 1; i <= n; i++ {
        if (pick[i]) {
            continue;
        }
        pick[i] = true
        arr[k] = i
        Permutations(k+1)
        pick[i] = false
    }

}


func main() {
    n = 8
    Permutations(1)
}