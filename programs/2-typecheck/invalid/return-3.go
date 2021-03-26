// Error: not all paths return a value
package main

func positive(x int) bool {
	switch {
	case x > 0: return true
	case x < 0: return false
	}
}