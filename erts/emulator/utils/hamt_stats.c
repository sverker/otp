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

void clear_hamt(void)
{
    vec_ix_top = 0;
    clear_node(0);
}

int nr_of_nodes()
{
    return vec_ix_top + 1;
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

#define NSTATS (32*128)

struct {
    int nkeys;
    double sum;
    double sq_sum;
}stats[NSTATS];

void doit(int nkeys)
{
    int i, n, top_stat_ix=0;
    int stat_ix;
    int step;
    int step_limit;

    for (i = 0; i < sizeof(stats) / sizeof(*stats); i++) {
	stats[i].nkeys = 0;
	stats[i].sum = 0;
	stats[i].sq_sum = 0;
    }

    for (n = 0; n < NVALUES; n++) {
	stat_ix = 0;
	step = 1;
	step_limit = 128;
	clear_hamt();

        for (i = 1; i <= nkeys; i++) {
            insert();
	    if (i % step == 0) {
		assert(stat_ix < NSTATS);
		assert(n==0 || stats[stat_ix].nkeys == i);
		stats[stat_ix].nkeys = i;
		stats[stat_ix].sum += nr_of_nodes();
		stats[stat_ix].sq_sum += sq(nr_of_nodes());
		stat_ix++;
		if (i > step_limit) {
		    step *= 2;
		    step_limit *= 2;
		}
	    }
        }
    }

    printf("Keys\tNodes/Keys\tsqrt(Keys)\tStdDev/sqrt(Keys)\n");
    for (i = 0; i < stat_ix ; i++) {
	double avg = stats[i].sum / NVALUES;
	double sq_avg = stats[i].sq_sum / NVALUES;
	double sqrt_nkeys = sqrt(stats[i].nkeys);
	double std_dev = sqrt(sq_avg - sq(avg));
	printf("%d\t%lf\t%lf\t%lf\n", stats[i].nkeys, avg / stats[i].nkeys,
	       sqrt_nkeys, std_dev / sqrt_nkeys);
    }
    return;
}


int main(int argc, char* argv[])
{
    int k;

    if (argc == 2) {
	char *endp;
	int nkeys = (int)strtol(argv[1], &endp, 10);
	if (endp != argv[k] && nkeys > 0)
	    doit(nkeys);
	else
	    fprintf(stderr, "ERROR: Invalid number of keys: '%s'\n", argv[k]);
    }
    else {
	fprintf(stderr, "Calculate expected number of nodes for different hamt sizes.\n\n");
	fprintf(stderr, "Syntax: %s <max map size>\n\n", argv[0]);
    }

    return 0;

}
