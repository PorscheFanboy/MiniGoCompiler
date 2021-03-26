// Refering to non existing struct field
package main


type point struct {
    x, y float64
}


func main() {
    var p point
    print(p.a)
} 