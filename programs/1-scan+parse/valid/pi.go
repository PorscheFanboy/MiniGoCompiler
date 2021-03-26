package main

// Compute pi using Leibniz formula

func pi(n int) float64 {
	var x float64
	var a = 1
	var b = 1
	for i := 0; i < n; i++ {
		x += float64(a) / float64(b)
		a *= -1
		b += 2
	}
	return x * 4.0
}

func main () {
	var n = 150
	println("Computing pi with", n, "iterations:")
	println(pi(n))
}
