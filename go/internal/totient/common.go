package totient

type Result struct {
	Lower, Upper int64
	Sum          int64
	TimeElapsed  float64 // in seconds
}

type Calculator interface {
	Calculate(lower, upper int64) Result
}

func HCF(x, y int64) int64 {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	return x
}

func RelPrime(x, y int64) bool {
	return HCF(x, y) == 1
}

func Euler(n int64) int64 {
	var length int64 = 0
	var i int64
	for i = 1; i < n; i++ {
		if RelPrime(n, i) {
			length++
		}
	}
	return length
}
