#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>

#include <Rdefines.h>
#include <R_ext/Altrep.h>

static R_altrep_class_t chunkrep_integer_class;

typedef struct CHUNKREP_CHUNK {
	char* data_map;
	size_t data_map_len;
	SEXP wrapped;
} CHUNKREP_CHUNK;

#define MBLEN 1000
static CHUNKREP_CHUNK chunks[MBLEN]; // FIXME: evil global
static size_t mappedbat_end = 0;


static void
chunkrep_signalhandler(int sig, siginfo_t *si, void *unused) {
	(void) sig;
	(void) unused;
	char* addr = (char*) si->si_addr;

	for (size_t i = 0; i < mappedbat_end && i < MBLEN; i++) {
		CHUNKREP_CHUNK masq = chunks[i];
		if (addr >= masq.data_map && addr < masq.data_map + masq.data_map_len) {
		   fprintf(stderr, "Got signal for our address: 0x%lx\n",
					(long) si->si_addr);
		 //  mprotect(masq.data_map, masq.data_map_len, PROT_WRITE);
//		   memcpy(masq.data_map, b->theap.base, masq.data_map_len);
//		   BBPunfix(masq.bat_cache_id);
		}
	}
	// TODO: longjump out of there if this was a mistake
	   fprintf(stderr, "Eeek @ 0x%lx\n",
				(long) si->si_addr);
	   exit(EXIT_FAILURE);
}

static SEXP chunkrep_wrap(SEXP x) {
	if (!IS_INTEGER(x)) {
		error("need integer vectors");
		return R_NilValue;
	}
	if (!ALTREP(x)) {
		return x;
	}
    SEXP ans = R_new_altrep(chunkrep_integer_class, x, R_NilValue);
	MARK_NOT_MUTABLE(ans);
	return ans;
}


static Rboolean chunkrep_inspect(SEXP x, int pre, int deep, int pvec,
		void (*inspect_subtree)(SEXP, int, int, int)) {

	Rprintf("CHUNKREP \n");
	inspect_subtree(R_altrep_data1(x), pre, deep, pvec);
	return TRUE;
}


static R_xlen_t chunkrep_length(SEXP x) {
	return LENGTH(R_altrep_data1(x));
}


static void* chunkrep_dataptr(SEXP x, Rboolean writeable) {
	if (R_altrep_data2(x) == R_NilValue) {
		CHUNKREP_CHUNK* masq = malloc(sizeof(CHUNKREP_CHUNK));

		// TODO: mmap in batches so we can free them individually
		masq->data_map_len = LENGTH(R_altrep_data1(x)) * sizeof(int); // space for R vector
		masq->data_map = mmap(NULL, masq->data_map_len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		masq->wrapped = R_altrep_data1(x);

		//memcpy(&(mappedbats[mappedbat_end]), masq, sizeof(R_MASQ_BAT));
		//mappedbat_end++;

		mprotect(masq->data_map, masq->data_map_len, PROT_NONE);
		R_set_altrep_data2(x, R_MakeExternalPtr(masq->data_map, R_NilValue, R_NilValue));
	}
	return R_ExternalPtrAddr(R_altrep_data2(x));
}


static void* chunkrep_dataptr_or_null(SEXP x, Rboolean writeable) {
	return DATAPTR_OR_NULL(R_altrep_data1(x), writeable); // contract for method says that it should only return if no allocation is required
}


static int chunkrep_elt_integer(SEXP x, R_xlen_t i) {
	return ALTINTEGER_ELT(R_altrep_data1(x), i);
}


// FIXME
static R_xlen_t chunkrep_region_integer(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf) {
	error("region_integer() called but not supported");
    return 0;
}


static int chunkrep_is_sorted(SEXP x) {
	return INTEGER_IS_SORTED(R_altrep_data1(x));
}


static int chunkrep_no_na(SEXP x) {
	return INTEGER_NO_NA(R_altrep_data1(x));
}


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}
static const R_CallMethodDef R_CallDef[] = {
   CALLDEF(chunkrep_wrap, 1),
   {NULL, NULL, 0}
};


void R_init_chunkrep(DllInfo *dll) {
	R_altrep_class_t cls = R_make_altinteger_class("wrap_integer", "chunkrep", dll);

    struct sigaction sa;
    sa.sa_flags = SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = chunkrep_signalhandler;

    // TODO: which signal on Linux?
    // TODO: what about R's handler?
    sigaction(SIGBUS, &sa, NULL);

	/* override ALTREP methods */
	R_set_altrep_Inspect_method         (cls, chunkrep_inspect);
	R_set_altrep_Length_method          (cls, chunkrep_length);

	/* override ALTVEC methods */
	R_set_altvec_Dataptr_method         (cls, chunkrep_dataptr);
	R_set_altvec_Dataptr_or_null_method (cls, chunkrep_dataptr_or_null);

	/* override ALTINTEGER methods */
	R_set_altinteger_Elt_method         (cls, chunkrep_elt_integer);
	R_set_altinteger_Get_region_method  (cls, chunkrep_region_integer);
	R_set_altinteger_Is_sorted_method   (cls, chunkrep_is_sorted);
	R_set_altinteger_No_NA_method       (cls, chunkrep_no_na);

    chunkrep_integer_class = cls;

	R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);


}
