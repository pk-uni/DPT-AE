package bench

import (
	"fmt"
	"sort"
	"time"
	"totient/internal/totient"
)

type Runner struct {
	Calculator totient.Calculator
}

func NewRunner(calc totient.Calculator) *Runner {
	return &Runner{Calculator: calc}
}

func (r *Runner) Run(lower, upper int64) totient.Result {
	result := r.Calculator.Calculate(lower, upper)
	r.printResult(result)
	return result
}

func (r *Runner) RunN(lower, upper int64, n int) []totient.Result {
	results := make([]totient.Result, n)

	for i := 0; i < n; i++ {
		// small delay between runs to let system settle
		if i > 0 {
			time.Sleep(100 * time.Millisecond)
		}
		results[i] = r.Calculator.Calculate(lower, upper)
	}

	r.printMedianResult(results)
	return results
}

func (r *Runner) printResult(result totient.Result) {
	fmt.Printf("Sum of Totients between [%d..%d] is %d\n",
		result.Lower, result.Upper, result.Sum)
	fmt.Printf("Time taken: %.3f seconds\n", result.TimeElapsed)
}

func (r *Runner) printMedianResult(results []totient.Result) {
	n := len(results)
	if n == 0 {
		return
	}

	times := make([]float64, n)
	for i, r := range results {
		times[i] = r.TimeElapsed
	}

	sort.Float64s(times)

	var median float64
	if n%2 == 0 {
		median = (times[n/2-1] + times[n/2]) / 2
	} else {
		median = times[n/2]
	}

	fmt.Printf("Sum of Totients between [%d..%d] is %d\n", results[0].Lower, results[0].Upper, results[0].Sum)
	fmt.Printf("Median Time Taken: %.6f s\n", median)
}
