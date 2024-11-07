package parallel

import (
	"sync"
	"time"
	"totient/internal/totient"
)

type Parallel struct {
	NumWorkers int
}

func (p *Parallel) Calculate(lower, upper int64) totient.Result {
	start := time.Now()

	jobs := make(chan int64, upper-lower)
	results := make(chan int64, upper-lower)

	var wg sync.WaitGroup

	for w := 0; w < p.NumWorkers; w++ {
		wg.Add(1)
		go p.eulerWorker(jobs, results, &wg)
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

	return totient.Result{
		Lower:       lower,
		Upper:       upper,
		Sum:         sum,
		TimeElapsed: time.Since(start).Seconds(),
	}
}

func (p *Parallel) eulerWorker(jobs <-chan int64, results chan<- int64, wg *sync.WaitGroup) {
	defer wg.Done()
	for n := range jobs {
		results <- totient.Euler(n)
	}
}
