#include /home/stage/INSTALL/tau-2.26.2p1/x86_64/lib/Makefile.tau-ompt-pdt-openmp


#TAU_OPTIONS = -optKeepFiles -optVerbose


CC = gcc
CFLAGS=-O2
LDFLAGS=
EXEC= test_for test_tasks


all:$(EXEC)


test_for.o:test_for.c
	$(CC) -fopenmp -c $< $(CFLAGS)

test_tasks.o:test_tasks.c
	$(CC) -fopenmp -c $< $(CFLAGS)

d_s1s0_1d_sym8.o:d_s1s0_1d_sym8.c
	$(CC) -c $< $(CFLAGS)

d_s0s1_1d_sym8.o:d_s0s1_1d_sym8.c
	$(CC) -c $< $(CFLAGS)

d_s0s0_1d_sym8_imd.o:d_s0s0_1d_sym8_imd.c
	$(CC) -c $< $(CFLAGS)

d_s0s0_1d_sym8_md.o:d_s0s0_1d_sym8_md.c
	$(CC) -c $< $(CFLAGS)


test_for: test_for.o d_s1s0_1d_sym8.o d_s0s1_1d_sym8.o  d_s0s0_1d_sym8_imd.o d_s0s0_1d_sym8_md.o
	$(CC) -o $@ $^ -fopenmp $(CFLAGS)

test_tasks: test_tasks.o d_s1s0_1d_sym8.o d_s0s1_1d_sym8.o  d_s0s0_1d_sym8_imd.o d_s0s0_1d_sym8_md.o
	$(CC) -o $@ $^ -fopenmp $(CFLAGS)

clean:
	rm -rf test.o test_tasks.o test_for.o test_for test_tasks



