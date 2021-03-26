// Error: f expects an int
package main

func f(x int) {

}

func main () {
	type number int
	var x number = number(3)
	f(x)
}