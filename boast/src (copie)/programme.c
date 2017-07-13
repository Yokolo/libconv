#include <omp.h>
#include <stdio.h>
int main(){
	#pragma omp parallel
	{
	#pragma omp single
	{
	printf("Je suis seul\n");
	}
	printf("npous somme nombreux\n");
	}
	return 0;
}
