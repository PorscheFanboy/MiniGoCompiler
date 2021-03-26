//~x: 0
//~x: 0
//~x: 1
//~x: 2
//~i: 0
//~x: 3
//~i: 1
//~x: 4
//~x: 4
package main

var i = 0

func f() {
	println("i:", i)
	i++
}

func main () {
	x := 0
	println("x:", x)

	for {
		break;
		x++;
	}
	println("x:", x)

	for x < 1 {
		x++;
	}
	println("x:", x)

	for ; i < 10; f() {
		x++;
		println("x:", x)
		if i == 0 {
			continue
		} else if i == 2 {
			break
		} else {
			
		}
	}
	println("x:", x)
}