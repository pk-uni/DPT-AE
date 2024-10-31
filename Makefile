totientSeq : gcc-14 -Wall -03 -o seq/totientRange seq/totientRange.c

totientPar : gcc-14 -Wall -O3 -fopenmp -o par/totientRange par/totientRange.c