package main

import (
	"fmt"
	"os"
	"strconv"
	"time"

	"totientRange/lib/par"
	"totientRange/lib/seq"
)

func main() {
	if len(os.Args) < 4 {
		panic(fmt.Sprintf("Usage: must provide either 'seq' or 'par' as first argument"))
	}

	if os.Args[1] == "seq" {
		runSeq()
	} else if os.Args[1] == "par" {
		runPar()
	} else {
		panic(fmt.Sprintf("Usage: first argument must be either 'seq' or 'par'"))
	}
}

func runPar() {
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
	totients := par.SumTotient(lower, upper)
	elapsed := time.Since(start)
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totients)
	fmt.Println("Elapsed time", elapsed)
}

func runSeq() {
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
	totients := seq.SumTotient(lower, upper)
	elapsed := time.Since(start)
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totients)
	fmt.Println("Elapsed time", elapsed)
}
