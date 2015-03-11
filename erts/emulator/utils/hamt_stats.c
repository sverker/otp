#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

typedef unsigned int Index;

#define VEC_SIZE (10*1000*1000)
#define NODE_SIZE 16

typedef struct {
    Index arr[NODE_SIZE];
}Node;

Node vec[VEC_SIZE];

Index vec_ix_top = 0;

#define IS_FREE 0
#define IS_LEAF ((Index)-1)



Index random_ix(void)
{
    return random() % NODE_SIZE;
}

void clear_node(Index node_ix)
{
    int i;
    for (i=0; i < NODE_SIZE; i++)
    {
	vec[node_ix].arr[i] = IS_FREE;
    }
}


void insert(void)
{
    Node* ptr = vec;
    int new_node = 0;
    for (;;) {
        Index ix = random_ix();
        switch (ptr->arr[ix]) {
        case IS_FREE:
            ptr->arr[ix] = IS_LEAF;
            return;
        case IS_LEAF:
	    vec_ix_top++;
            assert(vec_ix_top < VEC_SIZE);
	    clear_node(vec_ix_top);
            ptr->arr[ix] = vec_ix_top;
            ptr = &vec[vec_ix_top];
	    ix = random_ix();
	    assert(ptr->arr[ix] == IS_FREE);
            ptr->arr[ix] = IS_LEAF;
	    new_node = 1;
            break;
        default:
	    assert(!new_node);
	    assert(ptr->arr[ix] > (ptr-vec));
            ptr = &vec[ptr->arr[ix]];
        }
    }
}

#define NVALUES 1000

double sq(double v)
{
    return v*v;
}

void doit(int nkeys)
{
    int i, n;
    int values[NVALUES];
    double sum = 0;
    double sq_sum = 0;
    double sqdiff_sum = 0;
    double avg, sq_avg;
    double varians = 0;


    for (n = 0; n < NVALUES; n++) {
        vec_ix_top = 0;
	clear_node(0);
        for (i = nkeys; i; i--) {
            insert();
        }
        values[n] = vec_ix_top;
        sum += values[n];
        sq_sum += sq(values[n]);
        //printf("Number of nodes: %d\n", values[n]);
    }

    avg = sum / NVALUES;
    sq_avg = sq_sum / NVALUES;
    for (n = 0; n < NVALUES; n++) {
        varians += sq(values[n] - avg) / NVALUES;
    }
    printf("--- %d keys ---\n", nkeys);
    printf("Average: %lf\n", avg);
    printf("Std dev: %lf\n", sqrt(varians));
    printf("Std dev: %lf\n", sqrt(sq_avg - sq(avg)));

    return;
}


int main()
{
    int nkeys[] = {32, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000,
		  1000*1000, 3*1000*1000, 10*1000*1000,
		  0};
    int k;

    for (k=0; nkeys[k]; k++) {
	doit(nkeys[k]);
    }

    return 0;

}
