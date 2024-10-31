// totientRange.go -- Sequential Euler Totient Function (Go Version)
// compile -- go build
// run --    totientRange lower_num upper_num

// Based on code from Phil Trinder from earlier work by: Greg Michaelson,
// Patrick Maier, Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin
// Runciman

// This program calculates the sum of the totients between a lower and an upper
// limit

package main

import (
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

// Compute the Highest Common Factor, hcf of two numbers x and y
// hcf x 0 = x
// hcf x y = hcf y (mod x y)
func hcf(x, y int64) int64 {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	return x
}

// relprime x y = hcf x y == 1
func relprime(x, y int64) bool {
	return hcf(x, y) == 1
}

// euler(n) computes the Euler totient function, i.e. counts the number of
// positive integers up to n that are relatively prime to n
func euler(n int64) int64 {
	var length int64
	var i int64
	// do parallel magic here
	for i = 1; i < n; i++ {
		if relprime(n, i) {
			length++
		}
	}
	return length
}

// sumTotient lower upper sums the Euler totient values for all numbers
// between "lower" and "upper".
func sumTotient(lower, upper int64) int64 {
	var sum int64

	for i := lower; i <= upper; i++ {
		sum += euler(i)
	}
	return sum
}

func something(lower, upper int64) int64 {

	numWorkers := runtime.NumCPU()

	sumJobs := make(chan int64, numWorkers*3)
	results := make(chan int64)

	for i := lower; i <= upper; i++ {
		sumJobs <- i
	}

	var sum int64
	go func() {
		sum += <-results
	}()

	go func() {
		<-results
		close(sumJobs)
		close(results)
	}()

	return sum

}

func eulerWorker(sumJobs <-chan int64, results chan<- int64) {
	var i int64
	for n := range sumJobs {
		for i = 1; i < n; i++ {
			if relprime(n, i) {
				results <- i
			}
		}
	}
}

func main() {
	var lower, upper int64
	var err error
	// Read and validate lower and upper arguments
	if len(os.Args) < 3 {
		panic(fmt.Sprintf("Usage: must provide lower and upper range limits as arguments"))
	}

	// Go if supports "If with a short statement"
	if lower, err = strconv.ParseInt(os.Args[1], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse first argument"))
	}
	if upper, err = strconv.ParseInt(os.Args[2], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse second argument"))
	}

	start := time.Now()
	totients := sumTotient(lower, upper)
	elapsed := time.Since(start)
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totients)
	fmt.Println("Elapsed time", elapsed)
}
