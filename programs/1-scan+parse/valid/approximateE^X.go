package main

// Compute e^x using taylor approximation, increase k for better accuracy
func compute(x float64) float64 {
    var k = 10
    var result = 1.0

    for k > 0 {
        var counter = k
        var fact = 1.0
        var product = 1.0
        for counter > 0 {
            product *= x
            fact *= float64(counter)
            counter--
        }
        result = result + product / fact
        k--
    }
    return result
}

func main() {
    println(compute(3.0))
}