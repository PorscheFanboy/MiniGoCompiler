package main

var password [5]rune

func findPassword () {
    println("Trying to find the password...")
    var guess [5]rune
    var i = 0
    for guess != password {
        guess[i]++
        for j := 0; j < 5; j++ {
            if guess[j] != 'z' + rune(1) {
                 break
            } else {
                guess[j] = 'a'
                guess[j+1]++
            }
	}
    }
    print("Found it: ")
    for i := 0; i < 5; i++ {
         print(string(guess[i]));
    }
}

func main () {
	password[0] = 'h'
	password[1] = 'e'
	password[2] = 'l'
	password[3] = 'l'
	password[4] = 'o'
	findPassword()
}
