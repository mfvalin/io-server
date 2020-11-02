
SHELL = /bin/bash

ifeq "$(BASE_ARCH)" "$(EC_ARCH)"
$(error FATAL: EC_ARCH is equal to BASE_ARCH, no compiler architecture is defined, ABORTING)
endif

ifneq (,$(findstring intel,$(EC_ARCH)))
# $(error FATAL: intel compiler  detected)
EXTRA_FTN := -no-wrap-margin
EXTRA_C   := -march=core-avx2
CCOMPILER = s.cc
FCOMPILER = s.f90
endif

ifneq (,$(findstring gfortran,$(EC_ARCH)))
# $(error FATAL: intel compiler  detected)
EXTRA_FTN := 
EXTRA_C   := -mavx2 -mfma
CCOMPILER = s.cc
FCOMPILER = s.f90
endif

ifneq (,$(findstring gnu,$(EC_ARCH)))
# $(error FATAL: intel compiler  detected)
EXTRA_FTN := 
EXTRA_C   := -mavx2 -mfma
CCOMPILER = gcc
FCOMPILER = gfortran
FMPICOMPILER = mpif90
endif

ifneq (,$(findstring llvm,$(EC_ARCH)))
# $(error FATAL: intel compiler  detected)
EXTRA_FTN := 
EXTRA_C   := -mavx2 -mfma
CCOMPILER = clang
FCOMPILER = flang
endif

all: test_arena.Abs test_remote_circular_buffer.Abs

memory_arena.inc: memory_arena.c memory_arena.h
	grep InTf memory_arena.c | sed -e 's:^// ::' -e 's/[ ]*!InTf.*//' >memory_arena.inc

circular_buffer.inc: circular_buffer.c
	grep InTf circular_buffer.c | sed -e 's://::' -e 's/[ !]*InTf.*//' >circular_buffer.inc

test_arena.Abs: test_memory_arena.F90 memory_arena.c memory_arena.h memory_arena.inc circular_buffer.inc circular_buffer.c circular_buffer.h
	rm -f test_arena.Abs
	$(CCOMPILER) -c -Wall -O2 memory_arena.c circular_buffer.c 
	$(FMPICOMPILER) test_memory_arena.F90 memory_arena.o circular_buffer.o -o test_arena.Abs

test_remote_circular_buffer.Abs: test_remote_circular_buffer.F90 circular_buffer.c circular_buffer.inc circular_buffer.h
	$(CCOMPILER) -c -Wall -Wextra circular_buffer.c
	$(FMPICOMPILER) test_remote_circular_buffer.F90 circular_buffer.o -o test_remote_circular_buffer.Abs

clean:
	rm -f *.o memory_arena.inc circular_buffer.inc *.Abs
