package sequential

import (
	"time"
	"totient/internal/totient"
)

type Sequential struct{}

func New() *Sequential {
	return &Sequential{}
}

func (s *Sequential) Calculate(lower, upper int64) totient.Result {
	start := time.Now()

	var sum int64 = 0
	for i := lower; i <= upper; i++ {
		sum += totient.Euler(i)
	}

	return totient.Result{
		Lower:       lower,
		Upper:       upper,
		Sum:         sum,
		TimeElapsed: time.Since(start).Seconds(),
	}
}
