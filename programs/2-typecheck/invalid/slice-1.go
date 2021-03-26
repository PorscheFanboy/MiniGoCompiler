// Error: cannot index slices with string
package main

var x []int

func main () {
	println(x["test"])
}