#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>

#include <Rdefines.h>
#include <R_ext/Altrep.h>

#include "kbtree.h"

static R_altrep_class_t chunkrep_integer_class;

typedef struct CHUNKREP_CHUNK {
	char* data_map;
	size_t data_map_len;

	char* chunk_map;
	size_t chunk_map_len;

	SEXP wrapped;
	size_t wrapped_offset;
	size_t wrapped_max;

	size_t id;
	char converted;
	struct CHUNKREP_CHUNK *next;
} CHUNKREP_CHUNK;

int ct_chunk_cmp_fun(const CHUNKREP_CHUNK *a, const CHUNKREP_CHUNK *b) {
	if (a->chunk_map < b->chunk_map && a->chunk_map + a->chunk_map_len <= b->chunk_map) {
		return -1;
	}
	if (b->chunk_map < a->chunk_map && b->chunk_map + b->chunk_map_len <= a->chunk_map) {
		return 1;
	}
	return 0;
}

#define ct_chunk_cmp(a, b) (ct_chunk_cmp_fun(&(a), &(b)))
KBTREE_INIT(chunktree, CHUNKREP_CHUNK, ct_chunk_cmp)
kbtree_t(chunktree) *chunkrep_chunktree;


static struct sigaction default_signal_action;

static void
chunkrep_signalhandler(int sig, siginfo_t *si, void *unused) {
	(void) sig;
	(void) unused;
	char* addr = (char*) si->si_addr;
	CHUNKREP_CHUNK *masq = NULL;
	CHUNKREP_CHUNK chunkrep_probe;
	chunkrep_probe.chunk_map = addr;
	chunkrep_probe.chunk_map_len = 1;

	masq = kb_getp(chunktree, chunkrep_chunktree, &chunkrep_probe);
	if (masq && !masq->converted) {
		if (Rf_GetOption1(install("chunkrep.debug")) != R_NilValue) {
			Rprintf( "💣  %p %p → chunk %llu [%llu:%llu]\n",
				(void*) si->si_addr, masq->next, masq->id, masq->wrapped_offset, masq->wrapped_max);
		}
	   // convert chunk
	   if (mprotect(masq->chunk_map, masq->chunk_map_len, PROT_WRITE) != 0) {
		   goto wrapup;
	   }
	   for (size_t i = 0; i < masq->wrapped_max - masq->wrapped_offset; i++) {
		   // TODO: use region_integer here if available?
		   ((int*) masq->chunk_map)[i] = ALTINTEGER_ELT(masq->wrapped, masq->wrapped_offset + i);
	   }
	   if (mprotect(masq->chunk_map, masq->chunk_map_len, PROT_READ) != 0) {
		   goto wrapup;
	   }
	   masq->converted = 1;
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


static void chunkrep_finalizer(SEXP x) {
	CHUNKREP_CHUNK probe;
	CHUNKREP_CHUNK *masq = NULL;

	probe.chunk_map = R_ExternalPtrAddr(x);
	probe.chunk_map_len = 1;

	masq = kb_getp(chunktree, chunkrep_chunktree, &probe);
	if (!masq) {
		return;
	}
	if (munmap(masq->data_map, masq->data_map_len) != 0) {
		error("munmap error?!");
	}

	// TODO: inorder traversal for chunk removal? in-place modification, so maybe not supported by kbtree

	while(masq != NULL) {
		CHUNKREP_CHUNK *next = masq->next;
		if (Rf_GetOption1(install("chunkrep.debug")) != R_NilValue) {
			Rprintf( "🗑  %p %p chunk %llu [%llu:%llu]\n", R_ExternalPtrAddr(x), next, masq->id, masq->wrapped_offset, masq->wrapped_max);
		}
		kb_delp_chunktree(chunkrep_chunktree, masq);
		masq = next;
	}
}


static void* chunkrep_dataptr(SEXP x, Rboolean writeable) {
	if (R_altrep_data2(x) == R_NilValue) {
		void* dataptr = NULL;

	    struct sigaction sa_r;

		size_t sexp_len = LENGTH(R_altrep_data1(x));
		size_t chunk_len = sysconf(_SC_PAGESIZE) * INTEGER(Rf_GetOption1(install("chunkrep.len")))[0];
		size_t nchunks = (size_t) ceil((double)(sexp_len * sizeof(int))/chunk_len);
		size_t wrapper_len = chunk_len * nchunks;
		SEXP res;
		CHUNKREP_CHUNK *prev = NULL;

		void* wrapper = mmap(NULL, wrapper_len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (wrapper == NULL) {
			error("mmap failure 1");
			return NULL;
		}
		// check if our signal hander is still installed
		sigaction(SIGBUS, NULL, &sa_r);
		if (sa_r.sa_sigaction != chunkrep_signalhandler) {
			error("someone overwrote the signal handler");
			return NULL;
		}

		if (Rf_GetOption1(install("chunkrep.debug")) != R_NilValue) {
			Rprintf("DATAPTR(), setting up %llu maps in [%p, %p]\n", nchunks, wrapper, (char*) wrapper + wrapper_len - 1);
		}
		// reverse iteration so we can set next properly
		for (ssize_t id = nchunks-1; id >= 0; id--) {
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
			chunk->converted = 0;

			chunk->next = NULL;
			chunk->data_map = wrapper;
			chunk->data_map_len = wrapper_len;

			chunk->wrapped        = R_altrep_data1(x);
			chunk->wrapped_offset = wrapped_offset;
			chunk->wrapped_max    = max_offset;

			chunk->chunk_map_len   = chunk_len;
			chunk->chunk_map = mmap(base_addr, chunk_len, PROT_READ | PROT_WRITE, MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			if(chunk->chunk_map == NULL || chunk->chunk_map != base_addr) {
				error("mmap failure 2");
				return NULL;
			}
			if (mprotect(chunk->chunk_map, chunk_len, PROT_NONE) != 0) {
				error("mprotect failure");
				return NULL;
			}
			if (prev) {
				chunk->next = prev;
			}
			prev = chunk;
			kb_putp_chunktree(chunkrep_chunktree, chunk);
			dataptr = chunk->chunk_map;
		}
		res = PROTECT(R_MakeExternalPtr(dataptr, R_NilValue, R_NilValue));
		R_RegisterCFinalizer(res, chunkrep_finalizer);
		R_set_altrep_data2(x, res);
		UNPROTECT(1);

	}
	return R_ExternalPtrAddr(R_altrep_data2(x));
}


static void* chunkrep_dataptr_or_null(SEXP x, Rboolean writeable) {
	return DATAPTR_OR_NULL(R_altrep_data1(x), writeable); // contract for method says that it should only return if no allocation is required
}


static int chunkrep_elt_integer(SEXP x, R_xlen_t i) {
	return ALTINTEGER_ELT(R_altrep_data1(x), i);
}


// TODO implement this, although nobody seems to be calling this yet
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


static SEXP chunkrep_treesize(void) {
	if (!chunkrep_chunktree) {
		error("chunkrep tree missing");
		return R_NilValue;
	}
	return ScalarInteger(kb_size(chunkrep_chunktree));
}

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}
static const R_CallMethodDef R_CallDef[] = {
   CALLDEF(chunkrep_wrap, 1),
   CALLDEF(chunkrep_treesize, 0),
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

	R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}
