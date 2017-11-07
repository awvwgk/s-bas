PROG=sbas

###------------------------------------------------------------------------###

OBJS_MOD  = global.o

OBJS_MISC = help.o read.o error.o

OBJS_PROG = main.o coord.o basis.o

OBJS = $(OBJS_MOD) $(OBJS_MISC) $(OBJS_PROG)

SRCS=$(patsubst %.F, %.o, $(wildcard *.F))
###------------------------------------------------------------------------###
FC=ifort
CC=icc

# compile flags
FCFLAGS = -O -axAVX -qopenmp -g -traceback -check bounds
CCFLAGS = -O -g -DLINUX -DEBUG
# link flags
FL = ifort -static -fopenmp 
LIBS = 

###------------------------------------------------------------------------###
.PHONY: all
.PHONY: clean

all: $(PROG)

###------------------------------------------------------------------------###
%.o: %.f
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

###------------------------------------------------------------------------###
$(PROG): $(OBJS)
	$(FL) $(LIBS) -o $@ $^

clean:
	rm *.o 
	rm *.mod 
	rm $(PROG)
