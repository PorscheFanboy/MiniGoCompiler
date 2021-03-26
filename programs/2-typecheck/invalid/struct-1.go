// error: cannot compare different structures
package main

var x struct {a int; b int;}
var y struct {b int; a int;}

func f () {
	if x == y {}
}