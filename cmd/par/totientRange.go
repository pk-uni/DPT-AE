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
	"sync"
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

func sumTotientPar(lower, upper int64) int64 {

	numWorkers := runtime.NumCPU()

	jobs := make(chan int64, upper-lower)
	results := make(chan int64, upper-lower)

	var wg sync.WaitGroup

	for w := 0; w < numWorkers; w++ {
		wg.Add(1)
		go eulerWorker(jobs, results, &wg)
	}

	go func() {
		for i := lower; i <= upper; i++ {
			jobs <- i
		}
		close(jobs)
	}()

	go func() {
		wg.Wait()
		close(results)
	}()

	var sum int64
	for r := range results {
		sum += r
	}

	return sum

}

func eulerWorker(jobs <-chan int64, results chan<- int64, wg *sync.WaitGroup) {
	defer wg.Done()
	var count, i int64

	for n := range jobs {
		count = 0
		for i = 1; i < n; i++ {
			if relprime(n, i) {
				count++
			}
		}
		results <- count
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
	totients := sumTotientPar(lower, upper)
	elapsed := time.Since(start)
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totients)
	fmt.Println("Elapsed time", elapsed)
}
