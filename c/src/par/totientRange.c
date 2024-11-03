// TotientRange.c - Sequential Euler Totient Function (C Version)
// compile: gcc -Wall -O3 -o TotientRange TotientRange.c
// run:     ./TotientRange lower_num upper_num

// Based on code from Phil Trinder from earlier work by: Greg Michaelson,
// Patrick Maier, Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin
// Runciman

// This program calculates the sum of the totients between a lower and an upper
// limit

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <time.h>
#include <omp.h>

// Compute the Highest Common Factor, hcf of two numbers x and y
// hcf x 0 = x
// hcf x y = hcf y (rem x y)
int64_t hcf(int64_t x, int64_t y) {
  int64_t t;
  while (y != 0) {
    t = x % y;
    x = y;
    y = t;
  }
  return x;
}

// relprime x y = (hcf x y == 1)
int relprime(int64_t x, int64_t y) {
  return hcf(x, y) == 1;
}

// euler n = length (filter (relprime n) [1 .. n-1])
int64_t euler(int64_t n) {
  int64_t length = 0;
  #pragma omp parallel reduction(+:length)
  {
    #pragma omp for schedule(dynamic, 100)
    for (int64_t i = 1; i < n; i++) {
      if (relprime(n, i)) {
        length++;
      }
    }
  }
  return length;
}


// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
int64_t sumTotient(int64_t lower, int64_t upper) {
  int64_t sum = 0;
  #pragma omp parallel reduction(+:sum)
  {
    #pragma omp for schedule(dynamic, 1)
    for (int64_t i = lower; i <= upper; i++) {
      sum = sum + euler(i);
    }
  }
  return sum;
}


int main(int argc, char ** argv) {
  int64_t lower, upper;

  if (argc != 3) {
    printf("not 2 arguments\n");
    return 1;
  }

  sscanf(argv[1], "%" SCNd64, &lower);
  sscanf(argv[2], "%" SCNd64, &upper);

  struct timespec start, end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  int64_t sum = sumTotient(lower, upper);
  clock_gettime(CLOCK_MONOTONIC, &end);

  double time_taken_ns = 1000000000L * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;

  printf("C: Sum of Totients  between [%ld..%ld] is %ld\n",
         lower, upper, sum);
  printf("Time Taken: %f s\n", time_taken_ns / 1000 / 1000 / 1000);

  return 0;
}
