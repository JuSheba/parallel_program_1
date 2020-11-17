gf=gfortran -fopenmp
#gf=ifort -Ofast -qopenmp
files=$(wildcard *.f90)
prog: $(patsubst %.f90, %.o, $(files))
	$(gf) $^ -o $@
main.o: Task.o
%.o : %.f90
	$(gf) -c $<
exe: prog
	./$<
clean:
	rm -f *.o prog *.mod
.PHONY: clean, exe
