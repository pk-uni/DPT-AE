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
#include <stdlib.h>


#define DEFAULT_NUM_RUNS 5

int compare_doubles(const void* a, const void* b) {
    const double *x = a;
    const double *y = b;
    if (*x < *y) return -1;
    if (*x > *y) return 1;
    return 0;
}

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
  for (int64_t i = 1; i < n; i++) {
    if (relprime(n, i)) {
      length++;
    }
  }
  return length;
}


// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
int64_t sumTotient(int64_t lower, int64_t upper) {
  int64_t sum = 0;
  for (int64_t i = lower; i <= upper; i++) {
    sum = sum + euler(i);
  }
  return sum;
}


int main(int argc, char ** argv) {
  int64_t lower, upper;
  unsigned int num_runs = DEFAULT_NUM_RUNS;

  const char* num_runs_env = getenv("NUM_RUNS");
  if (num_runs_env != NULL) {
    num_runs = atoi(num_runs_env);
  }

  if (argc != 3) {
    printf("Usage: %s lower_bound upper_bound\n", argv[0]);
    return 1;
  }

  sscanf(argv[1], "%" SCNd64, &lower);
  sscanf(argv[2], "%" SCNd64, &upper);

  double times[num_runs];
  int64_t sum = 0;

  for (int i = 0; i < num_runs; i++) {
      struct timespec start, end;
      clock_gettime(CLOCK_MONOTONIC, &start);
      sum = sumTotient(lower, upper);  // only need to store sum once since it'll be the same
      clock_gettime(CLOCK_MONOTONIC, &end);
        
      times[i] = 1000000000L * (end.tv_sec - start.tv_sec) + end.tv_nsec - start.tv_nsec;
      times[i] = times[i] / 1000000000.0;  // convert to seconds
  }

  qsort(times, num_runs, sizeof(double), compare_doubles);
  double median_time = times[num_runs/2];

  printf("Sum of Totients between [%lld..%lld] is %lld\n", lower, upper, sum);
  printf("Median Time Taken: %f s\n", median_time);

  return 0;
}
