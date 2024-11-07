# DPT AE1

C+OpenMP vs Go

## Overview

This tests the performance of the different parallelism models provided by the OpenMP C library and the channels and go routines offered in Go.

The task used in this experiment is to calculate the sum of totients in a given range. That is the sum of all the relative primes for every number in a range. A sequential program solving this problem was provided in both C and Go and parallel version had to be created for both.

Both versions for both languages were tested with the three different datasets, on the GPG cluster. The ranges corresponding to the three datasets are listed below:
- `DS1` 1..15000
- `DS2` 1..30000
- `DS3` 1..60000

