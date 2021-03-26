//~bob
//~90
//~85
//~bob
//~90
//~85
//~alice
//~100
//~85
package main

type student struct {
  name string
  grades [2]int
}

// no effects
func curve(grades [2]int) {
	grades[0] += 15
	grades[1] += 10
}

func printStudent(x student) {
	println(x.name)
	println(x.grades[0])
	println(x.grades[1])
}

func main () {
	var bob, alice student
	bob.name = "bob"
	bob.grades[0] = 90
	bob.grades[1] = 85
	printStudent(bob)
	
	alice = bob
	alice.name = "alice"
	alice.grades = bob.grades;
	alice.grades[0] += 10;
	
	curve(bob.grades)
	printStudent(bob)
	printStudent(alice)
}
