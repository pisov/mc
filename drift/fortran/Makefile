FC=gfortran
CFLAGS=-O3
EXE=drift.x

all: drift.x

drift.x: drift.o drift.f90
	$(FC) $(CFLAGS) drift.o -o drift.x

drift.o: drift.f90
	$(FC) -c drift.f90

clean:
	rm -f *.o $(EXE)
