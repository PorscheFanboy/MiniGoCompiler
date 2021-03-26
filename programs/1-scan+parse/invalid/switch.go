// Error: two default cases
package main

func main () {
	var x = 0
	switch {
		default: println("hmm")
		case x > 1: println("bigger than one")
		case default: println("?")
	}
}