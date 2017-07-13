gcc -fopenmp -O2 test.c d_s1s0_1d_sym8.c d_s0s1_1d_sym8.c d_s0s0_1d_sym8_imd.c d_s0s0_1d_sym8_md.c
mv a.out test_openmp
gcc -O2 test.c d_s1s0_1d_sym8.c d_s0s1_1d_sym8.c d_s0s0_1d_sym8_imd.c d_s0s0_1d_sym8_md.c
mv a.out test_noopenmp
gcc -fopenmp -O2 testfor.c d_s1s0_1d_sym8.c d_s0s1_1d_sym8.c d_s0s0_1d_sym8_imd.c d_s0s0_1d_sym8_md.c
mv a.out testfor_openmp
gcc -O2 testfor.c d_s1s0_1d_sym8.c d_s0s1_1d_sym8.c d_s0s0_1d_sym8_imd.c d_s0s0_1d_sym8_md.c
mv a.out testfor_noopenmp

#clock_gettime

