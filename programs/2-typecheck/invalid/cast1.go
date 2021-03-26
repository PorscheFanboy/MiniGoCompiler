// Casting to non base type
package main

type point struct {
    x, y float64
}


func main() {
    var c int
    var p point
    p = point(c)
} 