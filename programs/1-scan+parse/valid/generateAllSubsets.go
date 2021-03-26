package main

// generate all subsets of the set {1,2,..,n} using recursion
var n = 6
var pick [100]bool

func generate(at int) {
    if at == n {
        for i := 0; i < n; i++ {
            if pick[i] {
                print(i+1)
                print(" ")
            }
        }
        println()
    } else {
        pick[at] = true
        generate(at+1)

        pick[at] = false
        generate(at+1)
    }
}

func main() {
    for i := 0; i < n; i++ {
        pick[i] = false;
    }
    generate(0);
}