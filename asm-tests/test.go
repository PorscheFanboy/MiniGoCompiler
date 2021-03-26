package main

// func gg(k int) {
//     var j int = 1;

// }

// func gg(k int, j int, p int) int {
//     if (k == 0) {
//         return 8
//     }
//     return p+k;
// }

// func fib(n int) int {
//     if (n == 0) {
//         return 0
//     }
//     if (n == 1) {
//         return 1
//     }
//     return fib(n-1) + fib(n-2)
// }

// var g int = 6


// var count int
// var arr []int
var w [][] int

// func towers(num int, from int, to int, aux int) {
//     if num == 1 {
//         count++
//         return
//     }
//     towers(num-1, from, aux, to)
//     count++
//     towers(num-1, aux, to, from)
// }

// func foo(A []int) {
//     var swap int
//     A[swap] = A[swap]
// }

func main() {
    // var index []int
    // var swap int
    // var x = 1
    // var y = 2
    // var z = 4
    // z = y + x
    // z = y
    // println(z)
    // println(y)
    // for i := 1; i < 9; i++ {
    //     y++
    //     println(y)
        // swap = i % 2 * index[i]
        // arr[swap] = index[i]
        // w[1] = append(w[1], 6)
        // w[1][3] = 2
        // var v []int
        // w[1] = v

        // println (w[i])
    // }
    var arr []int
    // arr = append(arr, 9)
    // arr = append(arr, 10)
    // arr = append(arr, 10)
    // arr = append(arr, 11)
    // println(arr[1])
    // println(arr[0])

    for i := 1; i <  9; i++ {
        arr = append(arr, i)
    }
    w = append(w, arr)
    for i := 0; i< 6; i++ {
        println(w[0][i])
    }
    println (999 % w[0][4])

    // println(w[count])
    // println(arr[1])
    // towers(28, 1, 2, 3)
}

