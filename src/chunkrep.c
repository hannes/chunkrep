#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>

#include <Rdefines.h>
#include <R_ext/Altrep.h>

#include "kbtree.h"

static R_altrep_class_t chunkrep_integer_class;

typedef struct CHUNKREP_CHUNK {
	size_t id;
	char* data_map;
	size_t data_map_len;
	SEXP wrapped;
	size_t wrapped_offset;
	size_t wrapped_max;
} CHUNKREP_CHUNK;

int ct_elem_cmp_fun(const CHUNKREP_CHUNK *a, const CHUNKREP_CHUNK *b) {
	if (a->data_map < b->data_map && a->data_map + a->data_map_len <= b->data_map) {
		return -1;
	}
	if (b->data_map < a->data_map && b->data_map + b->data_map_len <= a->data_map) {
		return 1;
	}
	return 0;
}

#define ct_elem_cmp(a, b) (ct_elem_cmp_fun(&(a), &(b)))

KBTREE_INIT(chunktree, CHUNKREP_CHUNK, ct_elem_cmp)
kbtree_t(chunktree) *chunkrep_chunktree;

static struct sigaction default_signal_action;

static CHUNKREP_CHUNK chunkrep_probe; // avoid malloc-ing in signal handler

static void
chunkrep_signalhandler(int sig, siginfo_t *si, void *unused) {
	(void) sig;
	(void) unused;
	char* addr = (char*) si->si_addr;
	CHUNKREP_CHUNK *masq = NULL;
	chunkrep_probe.data_map = addr;
	chunkrep_probe.data_map_len = 1;

	masq = kb_getp(chunktree, chunkrep_chunktree, &chunkrep_probe);
	if (masq) {
		if (Rf_GetOption1(install("chunkrep.debug")) != R_NilValue) {
			Rprintf( "💣  %p → chunk %llu [%llu:%llu]\n",
				(void*) si->si_addr, masq->id, masq->wrapped_offset, masq->wrapped_max);
		}
	   // convert chunk
	   if (mprotect(masq->data_map, masq->data_map_len, PROT_WRITE) != 0) {
		   goto wrapup;
	   }
	   for (size_t i = 0; i < masq->wrapped_max - masq->wrapped_offset; i++) {
		   // TODO: use region_integer here if available
		   ((int*) masq->data_map)[i] = ALTINTEGER_ELT(masq->wrapped, masq->wrapped_offset + i);
	   }
	   if (mprotect(masq->data_map, masq->data_map_len, PROT_READ) != 0) {
		   goto wrapup;
	   }
	   kb_delp_chunktree(chunkrep_chunktree, masq);
	   return;
	}

wrapup:
	if (default_signal_action.sa_handler) {
		default_signal_action.sa_handler(sig);
		return;
	}
	if (default_signal_action.sa_sigaction) {
		default_signal_action.sa_sigaction(sig, si, unused);
		return;
	}
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
		void* dataptr = NULL;

		size_t sexp_len = LENGTH(R_altrep_data1(x));
		size_t chunk_len = sysconf(_SC_PAGESIZE) * INTEGER(Rf_GetOption1(install("chunkrep.len")))[0];
		size_t nchunks = (size_t) ceil((double)(sexp_len * sizeof(int))/chunk_len);
		size_t wrapper_len = chunk_len * nchunks;

		void* wrapper = mmap(NULL, wrapper_len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (wrapper == NULL) {
			error("mmap failure 1");
			return NULL;
		}
		if (Rf_GetOption1(install("chunkrep.debug")) != R_NilValue) {
			Rprintf("DATAPTR(), setting up %llu maps in [%p, %p]\n", nchunks, wrapper, (char*) wrapper + wrapper_len - 1);
		}

		for (size_t id = 0; id < nchunks; id++) {
			CHUNKREP_CHUNK *chunk = malloc(sizeof(CHUNKREP_CHUNK));
			size_t val_offset = id * chunk_len;
			size_t wrapped_offset = val_offset/sizeof(int);
			size_t max_offset = wrapped_offset + (chunk_len/sizeof(int));
			char* base_addr = ((char*) wrapper) + val_offset;
			if (max_offset > sexp_len) {
				max_offset = sexp_len;
			}
			if (!chunk) { // unlikely
				error("malloc failure");
				return NULL;
			}

			// TODO: do not use chunk_len for last map, wastes space, page-align instead
			chunk->id = id;
			chunk->wrapped        = R_altrep_data1(x);
			chunk->wrapped_offset = wrapped_offset;
			chunk->wrapped_max    = max_offset;
			chunk->data_map_len   = chunk_len;

			chunk->data_map = mmap(base_addr, chunk_len, PROT_READ | PROT_WRITE, MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			if(chunk->data_map == NULL || chunk->data_map != base_addr) {
				error("mmap failure 2");
				return NULL;
			}
			// TODO: check mprotect return code
			if (mprotect(chunk->data_map, chunk_len, PROT_NONE) != 0) {
				error("mprotect failure");
				return NULL;
			}
			kb_putp_chunktree(chunkrep_chunktree, chunk);
			if (!dataptr) {
				dataptr = chunk->data_map;
			}
		}
		R_set_altrep_data2(x, R_MakeExternalPtr(dataptr, R_NilValue, R_NilValue));
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

    sigaction(SIGBUS, &sa, &default_signal_action);

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

    chunkrep_chunktree = kb_init(chunktree, KB_DEFAULT_SIZE);

    // TODO: how to clean up unused entries for tree that is free'd

	R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}
