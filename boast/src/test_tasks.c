#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <omp.h>

//#include <ddot.f>

#define NBTESTS 50
#define PERIODIC 0
#define GROW 1
#define SHRINK -1


void d_s0s1_1d_sym8(const int32_t d, const int32_t idim, const int32_t * n, const int32_t bc, const double * __restrict__ x, double * __restrict__ y, const double a, const double a_y);

void d_s1s0_1d_sym8(const int32_t d, const int32_t idim, const int32_t * n, const int32_t bc, const double * __restrict__ x, double * __restrict__ y, const double a, const double a_y);
void d_s0s0_1d_sym8_imd(const int32_t d, const int32_t idim, const int32_t * n, const int32_t bc, const double * __restrict__ x, double * __restrict__ y, const double a, const double a_x, const double a_y);
void d_s0s0_1d_sym8_md(const int32_t d, const int32_t idim, const int32_t * n, const int32_t bc, const double * __restrict__ x, double * __restrict__ y, const double a, const double a_x, const double a_y);

double frand_a_b(double a, double b){
    return ( rand()/(double)RAND_MAX ) * (b-a) + a;
}
int rand_a_b(int a, int b){
    return rand()%(b-a) +a;
}

typedef struct Matrice Matrice;
struct Matrice
{
    double *tab; //Matrice de double
    int dimension; // nombre de dimension du problème
    int *tailles_s0; // taille du coté de la matrice
    int *tailles_s1; // taille du coté de la matrice
};

int * allocTailles(int dim){
	return (int*) malloc(sizeof(int)*dim);
}
double *allocMatrice(size_t taillemem){
	return (double*) malloc(taillemem*sizeof(double));
}

size_t getMatriceSize(Matrice *mat){
        int i;
	size_t taille_memoire=1;
	for( i=0; i<mat->dimension; i++) {
  		taille_memoire *= mat->tailles_s0[i];
	}
	return taille_memoire;
}

void initMatricePeriodic(Matrice *mat, int dimension, int *tailles){
	int i;

	mat->dimension=dimension;
	mat->tailles_s0=allocTailles(dimension);
	mat->tailles_s1=allocTailles(dimension);
	for( i=0; i<mat->dimension; i++) {
		mat->tailles_s0[i] = tailles[i]*2;
		mat->tailles_s1[i] = tailles[i];
	}

	mat->tab=allocMatrice(getMatriceSize(mat));
}

void setMatrice(Matrice *mat){
	int i;
	size_t taille_memoire=getMatriceSize(mat);        
	for(i=0;i<taille_memoire;i++){
		mat->tab[i]=frand_a_b(-1,1);
	}
}

void printMatrice(Matrice *mat){
	int i;
	size_t taille_memoire=getMatriceSize(mat);
	for(i=0;i<taille_memoire;i++){
		fprintf(stderr,"%f  ",mat->tab[i]);
	}
	printf("\n");
}
void printDiffMatrice(Matrice *mat,Matrice *m2){
	int i;
	size_t taille_memoire=getMatriceSize(mat);
	for(i=0;i<taille_memoire;i++){
		fprintf(stderr,"%f  ",fabs(mat->tab[i]-m2->tab[i]));
	}
	printf("\n");
}

int compareMatrice(Matrice *mat1, Matrice *mat2, double epsilon){
	size_t taille_m1 = getMatriceSize(mat1);
	size_t taille_m2 = getMatriceSize(mat2);
	int i;
	for(i=0; i < taille_m1 && i < taille_m2; i++){
		if(fabs(mat1->tab[i] - mat2->tab[i]) > epsilon)
			return 0; // différence dans les matrices
	}
	return 1; // matrice semblable
}
void multPoint(Matrice *m1, Matrice *m2){
	double *tmpin = m1->tab;
	double *tmppot = m2->tab;
	int i;
	size_t t = getMatriceSize(m1);
	#pragma omp task depend(inout:tmpin[:t]) depend(in:tmppot[:t])
	for(i = 0; i < getMatriceSize(m1); i++){
			m1->tab[i]=m2->tab[i] * m1->tab[i];
	}
}
double ddotMatrix(Matrice *m1, Matrice *m2){
	size_t size = getMatriceSize(m1);
	double res = 0.0l;
	int i,j;
	for( i = 0 ; i < size ; i++){
		res = m1->tab[i] * m2->tab[i] + res;	
		//printf("%Lf",res);
		//printf("debug flood massif, m1 = %f , m2 = %f \n",m1->tab[i], m2->tab[i]);
	}
	//printf("debug res = %f \n",res);
	return res;
}
void freeMatrix(Matrice *m1){
	free(m1->tab);
	
}
int main(){
	/*
@params d: la dimension du prob
@params idim: la dimension courante
@params n: tableau des dimensions
@params bc: condition périodic ou grow ou shrink
@params x: matrice entrée
@params y: sortie 
@params a:coeff 1
@params a_y: coeff 0
*/
	//void d_s0s1_1d_sym8(const int32_t d, const int32_t idim, const int32_t * n, const int32_t bc, const double * __restrict__ x, double * __restrict__ y,
	// const double a, const double a_y){
	//d_s0s1_1d_sym8
	int i;
	Matrice m1[NBTESTS]; // matrices de départ de l'espace S1 et résultat à la fin de l'algo
	Matrice m2[NBTESTS]; // matrices first step de l'espace S0
	Matrice m3[NBTESTS]; // matrices d'après MD , second step de l'espace réel
	Matrice res[NBTESTS]; // matrices de retour depuis l'espace réel vers S0 , avant dernier step

	Matrice random[NBTESTS]; // matrices random pour la multiplication avec la matrice arrivant dans l'espace réel pour l'énergie potentiel
	Matrice tmp[NBTESTS];
	double energie[NBTESTS];
	srand(time(NULL));

	int dim[3];
	for(i=0; i < NBTESTS; i++){
		dim[0]= 31+(2*i/5);
		dim[1]= 28+(2*i/5);
		dim[2]= 42+(2*i/5);
		initMatricePeriodic(&m1[i], 3, dim);
		initMatricePeriodic(&m2[i], 3, dim);
		initMatricePeriodic(&m3[i], 3, dim);
		initMatricePeriodic(&tmp[i], 3, dim);
		initMatricePeriodic(&res[i], 3, dim);
		initMatricePeriodic(&random[i], 3, dim);
		setMatrice(&m1[i]);
		setMatrice(&random[i]);
		//printf("Test mémoire &m1[%d].tab = %p, &m2[%d].tab = %p, &m3[%d].tab = %p \n",i,&m1[i].tab[24],i,&m2[i].tab[24],i,&m3[i].tab[24]);
	}
	double start_time = omp_get_wtime();
	#pragma omp parallel default(shared)
	//default(none) private(i) shared(m1,m2,m3,res,random,tmp,energie)
	{
		#pragma omp single
		{
			for(i=0; i<NBTESTS; i++){

				//printMatrice(m1);
				/* Ondelettes vers pseudo_réel ! */
				size_t t = getMatriceSize(m3);
				double *tmptabin = m1[i].tab;
				double *tmppot;
				double *tmptabout = m2[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s1s0_1d_sym8(m1[i].dimension, 0, m1[i].tailles_s1, PERIODIC, m1[i].tab, m2[i].tab, 1.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = tmp[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s1s0_1d_sym8(m1[i].dimension, 1, m2[i].tailles_s1, PERIODIC, m2[i].tab, tmp[i].tab, 1.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = m2[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s1s0_1d_sym8(m1[i].dimension, 2, tmp[i].tailles_s1, PERIODIC, tmp[i].tab, m2[i].tab, 1.0, 0.0);

				/*pseudo_réel vers réel MD */
				tmptabin = tmptabout;
				tmptabout = m3[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_md(m2[i].dimension, 0, m2[i].tailles_s0, PERIODIC, m2[i].tab, m3[i].tab, 1.0, 0.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = tmp[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_md(m2[i].dimension, 1, m2[i].tailles_s0, PERIODIC, m3[i].tab, tmp[i].tab, 1.0, 0.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = m3[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_md(m2[i].dimension, 2, m2[i].tailles_s0, PERIODIC, tmp[i].tab, m3[i].tab, 1.0, 0.0, 0.0);
				tmptabin = tmptabout;
				tmppot = random[i].tab;
				//La multiplication par la matrice random pour EPOT
				//#pragma omp task depend(inout:tmptabin[:t]) depend(in:tmppot[:t])
				{
					multPoint(&m3[i],&random[i]);
				}
				tmptabout = res[i].tab;				
				/*réel vers peudo réel IMD */
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_imd(m3[i].dimension, 0, res[i].tailles_s0, PERIODIC, m3[i].tab, res[i].tab, 1.0, 0.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = tmp[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_imd(m3[i].dimension, 1, res[i].tailles_s0, PERIODIC, res[i].tab, tmp[i].tab, 1.0, 0.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = res[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s0_1d_sym8_imd(m3[i].dimension, 2, res[i].tailles_s0, PERIODIC, tmp[i].tab, res[i].tab, 1.0, 0.0, 0.0);

				//Multiplication entre la première matrice REZ et celle qu'on a généré now POUR ENERGIE CINETIQUE
				tmptabin = tmptabout;
				tmppot = m2[i].tab;
				#pragma omp task depend(in:tmptabin[:t],tmppot[:t]) depend(out:energie[i])
				{
					energie[i]=ddotMatrix(&m2[i], &res[i]);
				}				

				/* Réel vers Ondelettes */
				tmptabout = m1[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s1_1d_sym8(res[i].dimension, 0, m1[i].tailles_s1, PERIODIC, res[i].tab, m1[i].tab, 1.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = tmp[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s1_1d_sym8(res[i].dimension, 1, m1[i].tailles_s1, PERIODIC, m1[i].tab, tmp[i].tab, 1.0, 0.0);
				tmptabin = tmptabout;
				tmptabout = m1[i].tab;
				#pragma omp task depend(in:tmptabin[:t]) depend(out:tmptabout[:t])
				d_s0s1_1d_sym8(res[i].dimension, 2, m1[i].tailles_s1, PERIODIC, tmp[i].tab, m1[i].tab, 1.0, 0.0);
			}
			//printf("fin du single\n");
		}
		#pragma omp taskwait
		//#pragma omp single
		//printMatrice(&res[i]);
	}
	double time = omp_get_wtime() - start_time;
	/*for(i=0; i < NBTESTS ; i++){
		printf(" Energie %d= %f \n",i, energie[i]);
	} */
	printf("Time for this execution : %lf \n",time);
	return 0;
}

	/*

	int dim[3] = {31, 48, 28};
		Matrice m1;
		Matrice m2;
		Matrice tmp;
		Matrice res;""
		Matrice tmp2;
		
		
		initMatricePeriodic(&m1,3,dim);
		initMatricePeriodic(&m2,3,dim);
		initMatricePeriodic(&tmp,3,dim);
		initMatricePeriodic(&res,3,dim);
		initMatricePeriodic(&tmp2,3,dim);
		setMatrice(&m1);
		//printMatrice(m1);
		/* Ondelettes vers pseudo_réel ! 
		d_s1s0_1d_sym8(m1.dimension,0,m1.tailles_s1,PERIODIC,m1.tab,m2.tab,1.0,0.0);
		d_s1s0_1d_sym8(m1.dimension,1,m2.tailles_s1,PERIODIC,m2.tab,tmp.tab,1.0,0.0);
		d_s1s0_1d_sym8(m1.dimension,2,tmp.tailles_s1,PERIODIC,tmp.tab,m2.tab,1.0,0.0);
		/*pseudo_réel vers réel IMD 

		d_s0s0_1d_sym8_imd(m2.dimension,0,m2.tailles_s0,PERIODIC,m2.tab,res.tab,1.0,0.0,0.0);
		d_s0s0_1d_sym8_imd(m2.dimension,1,m2.tailles_s0,PERIODIC,res.tab,tmp.tab,1.0,0.0,0.0);
		d_s0s0_1d_sym8_imd(m2.dimension,2,m2.tailles_s0,PERIODIC,tmp.tab,res.tab,1.0,0.0,0.0);

		
		/*réel vers peudo réel MD 
		d_s0s0_1d_sym8_md(res.dimension,0,res.tailles_s0,PERIODIC,res.tab,tmp2.tab,1.0,0.0,0.0);
		d_s0s0_1d_sym8_md(res.dimension,1,res.tailles_s0,PERIODIC,tmp2.tab,tmp.tab,1.0,0.0,0.0);
		d_s0s0_1d_sym8_md(res.dimension,2,res.tailles_s0,PERIODIC,tmp.tab,m2.tab,1.0,0.0,0.0);
		//printDiffMatrice(&m1,&m2);
		

		

		/* Réel vers Ondelettes 
		/*d_s0s1_1d_sym8(m2.dimension,0,m1.tailles_s1,PERIODIC,m2.tab,res.tab,1.0,0.0);
		d_s0s1_1d_sym8(tmp.dimension,1,m2.tailles_s1,PERIODIC,res.tab,tmp.tab,1.0,0.0);
		d_s0s1_1d_sym8(m2.dimension,2,tmp.tailles_s1,PERIODIC,tmp.tab,res.tab,1.0,0.0); s
		d_s0s1_1d_sym8(m2.dimension, 0, m1.tailles_s1, PERIODIC, m2.tab, res.tab, 1.0, 0.0);
		d_s0s1_1d_sym8(m2.dimension,1,m1.tailles_s1,PERIODIC,res.tab,tmp.tab,1.0,0.0);
		d_s0s1_1d_sym8(m2.dimension,2,m1.tailles_s1,PERIODIC,tmp.tab,res.tab,1.0,0.0);

		printDiffMatrice(&m1,&res);
		if(compareMatrice(&m1,&res,1.0e-7)){
			printf("success ! \n");
		} 
		*/



	
	//d_s0s1_1d_sym8(2,0,n,0,rez->tab,mat->tab,1.0,0.0);
	//printMatrice(m1);





