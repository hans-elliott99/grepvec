/*
  xmalloc.c - Simple malloc debugging library implementation

  This software is released under a BSD-style license.
  See the file LICENSE for details and copyright.

*/

/*
  TODO:
   - red zones
   - group dumps by source location
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
//#include <assert.h>
#include <stdio.h>
#define XMALLOC_INTERNAL 1
#include "xmalloc.h"

/* fake definition */
extern void Rf_error(const char *str);
extern void Rprintf(const char *str, ...);
#define assert(a) R_assert(a)

static void assert(int expr)
{
    if(expr == 0)
	Rf_error("internal allocation error in TRE");
}

static void Rabort(void)
{
    Rf_error("internal allocation error in TRE");
}


/*
  Internal stuff.
*/

typedef struct hashTableItemRec {
  void *ptr;
  int bytes;
  const char *file;
  int line;
  const char *func;
  struct hashTableItemRec *next;
} hashTableItem;

typedef struct {
  hashTableItem **table;
} hashTable;

static int xmalloc_peak;
int xmalloc_current;
static int xmalloc_peak_blocks;
int xmalloc_current_blocks;
static int xmalloc_fail_after;

#define TABLE_BITS 8
#define TABLE_MASK ((1 << TABLE_BITS) - 1)
#define TABLE_SIZE (1 << TABLE_BITS)

static hashTable *
hash_table_new(void)
{
  hashTable *tbl;

  tbl = malloc(sizeof(*tbl));

  if (tbl != NULL)
    {
      tbl->table = calloc(TABLE_SIZE, sizeof(*tbl->table));

      if (tbl->table == NULL)
	{
	  free(tbl);
	  return NULL;
	}
    }

  return tbl;
}

static int
hash_void_ptr(void *ptr)
{
  int hash;
  int i;

  /* I took this hash function just off the top of my head, I have
     no idea whether it is bad or very bad. */
  hash = 0;
  for (i = 0; i < (int)sizeof(ptr)*8 / TABLE_BITS; i++)
    {
/* R change: unsigned long may be shorter than ptr */
#ifdef _WIN64
      hash ^= (size_t)ptr >> i*8;
#else
      hash ^= (unsigned long)ptr >> i*8;
#endif
      hash += i * 17;
      hash &= TABLE_MASK;
    }
  return hash;
}

static void
hash_table_add(hashTable *tbl, void *ptr, int bytes,
	       const char *file, int line, const char *func)
{
  int i;
  hashTableItem *item, *new;

  i = hash_void_ptr(ptr);

  item = tbl->table[i];
  if (item != NULL)
    while (item->next != NULL)
      item = item->next;

  new = malloc(sizeof(*new));
  assert(new != NULL);
  new->ptr = ptr;
  new->bytes = bytes;
  new->file = file;
  new->line = line;
  new->func = func;
  new->next = NULL;
  if (item != NULL)
    item->next = new;
  else
    tbl->table[i] = new;

  xmalloc_current += bytes;
  if (xmalloc_current > xmalloc_peak)
    xmalloc_peak = xmalloc_current;
  xmalloc_current_blocks++;
  if (xmalloc_current_blocks > xmalloc_peak_blocks)
    xmalloc_peak_blocks = xmalloc_current_blocks;
}

static void
hash_table_del(hashTable *tbl, void *ptr)
{
  int i;
  hashTableItem *item, *prev;

  i = hash_void_ptr(ptr);

  item = tbl->table[i];
  if (item == NULL)
    {
      Rprintf("xfree: invalid ptr %p\n", ptr);
      Rabort();
    }
  prev = NULL;
  while (item->ptr != ptr)
    {
      prev = item;
      item = item->next;
    }
  if (item->ptr != ptr)
    {
      Rprintf("xfree: invalid ptr %p\n", ptr);
      Rabort();
    }

  xmalloc_current -= item->bytes;
  xmalloc_current_blocks--;

  if (prev != NULL)
    {
      prev->next = item->next;
      free(item);
    }
  else
    {
      tbl->table[i] = item->next;
      free(item);
    }
}

static hashTable *xmalloc_table = NULL;

static void
xmalloc_init(void)
{
  if (xmalloc_table == NULL)
    {
      xmalloc_table = hash_table_new();
      xmalloc_peak = 0;
      xmalloc_peak_blocks = 0;
      xmalloc_current = 0;
      xmalloc_current_blocks = 0;
      xmalloc_fail_after = -1;
    }
  assert(xmalloc_table != NULL);
  assert(xmalloc_table->table != NULL);
}



/*
  Public API.
*/

void
xmalloc_configure(int fail_after)
{
  xmalloc_init();
  xmalloc_fail_after = fail_after;
}

int
xmalloc_dump_leaks(void)
{
  int i;
  int num_leaks = 0;
  int leaked_bytes = 0;
  hashTableItem *item;

  xmalloc_init();

  for (i = 0; i < TABLE_SIZE; i++)
    {
      item = xmalloc_table->table[i];
      while (item != NULL)
	{
	  Rprintf("%s:%d: %s: %d bytes at %p not freed\n",
		 item->file, item->line, item->func, item->bytes, item->ptr);
	  num_leaks++;
	  leaked_bytes += item->bytes;
	  item = item->next;
	}
    }
  if (num_leaks == 0)
    Rprintf("No memory leaks.\n");
  else
    Rprintf("%d unfreed memory chuncks, total %d unfreed bytes.\n",
	   num_leaks, leaked_bytes);
  Rprintf("Peak memory consumption %d bytes (%.1f kB, %.1f MB) in %d blocks ",
	 xmalloc_peak, (double)xmalloc_peak / 1024,
	 (double)xmalloc_peak / (1024*1024), xmalloc_peak_blocks);
  Rprintf("(average ");
  if (xmalloc_peak_blocks)
    Rprintf("%d", ((xmalloc_peak + xmalloc_peak_blocks / 2)
		  / xmalloc_peak_blocks));
  else
    Rprintf("N/A");
  Rprintf(" bytes per block).\n");

  return num_leaks;
}

void *
xmalloc_impl(size_t size, const char *file, int line, const char *func)
{
  void *ptr;

  xmalloc_init();
  assert(size > 0);

  if (xmalloc_fail_after == 0)
    {
      xmalloc_fail_after = -2;
#if 0
      Rprintf("xmalloc: forced failure %s:%d: %s\n", file, line, func);
#endif
      return NULL;
    }
  else if (xmalloc_fail_after == -2)
    {
      Rprintf("xmalloc: called after failure from %s:%d: %s\n",
	     file, line, func);
      assert(0);
    }
  else if (xmalloc_fail_after > 0)
    xmalloc_fail_after--;

  ptr = malloc(size);
  if (ptr != NULL)
    hash_table_add(xmalloc_table, ptr, (int)size, file, line, func);
  return ptr;
}

void *
xcalloc_impl(size_t nmemb, size_t size, const char *file, int line,
	     const char *func)
{
  void *ptr;

  xmalloc_init();
  assert(size > 0);

  if (xmalloc_fail_after == 0)
    {
      xmalloc_fail_after = -2;
#if 0
      Rprintf("xcalloc: forced failure %s:%d: %s\n", file, line, func);
#endif
      return NULL;
    }
  else if (xmalloc_fail_after == -2)
    {
      Rprintf("xcalloc: called after failure from %s:%d: %s\n",
	     file, line, func);
      assert(0);
    }
  else if (xmalloc_fail_after > 0)
    xmalloc_fail_after--;

  ptr = calloc(nmemb, size);
  if (ptr != NULL)
    hash_table_add(xmalloc_table, ptr, (int)(nmemb * size), file, line, func);
  return ptr;
}

void
xfree_impl(void *ptr, const char *file, int line, const char *func)
{
  /*LINTED*/(void)&file;
  /*LINTED*/(void)&line;
  /*LINTED*/(void)&func;
  xmalloc_init();

  if (ptr != NULL)
    hash_table_del(xmalloc_table, ptr);
  free(ptr);
}

void *
xrealloc_impl(void *ptr, size_t new_size, const char *file, int line,
	      const char *func)
{
  void *new_ptr;

  xmalloc_init();
  assert(ptr != NULL);
  assert(new_size > 0);

  if (xmalloc_fail_after == 0)
    {
      xmalloc_fail_after = -2;
      return NULL;
    }
  else if (xmalloc_fail_after == -2)
    {
      Rprintf("xrealloc: called after failure from %s:%d: %s\n",
	     file, line, func);
      assert(0);
    }
  else if (xmalloc_fail_after > 0)
    xmalloc_fail_after--;

  new_ptr = realloc(ptr, new_size);
  if (new_ptr != NULL)
    {
      hash_table_del(xmalloc_table, ptr);
      hash_table_add(xmalloc_table, new_ptr, (int)new_size, file, line, func);
    }
  return new_ptr;
}



/* EOF */
