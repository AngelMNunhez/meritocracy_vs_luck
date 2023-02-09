# Compilador de Fortran
FC=gfortran

# Compilador usado para "linkar"
LD=gfortran

# Opciones de compilacion
FFLAGS= -O3 -fbounds-check

#Borrar ficheros. En Linux/Mac sustituir por  'RM=rm'
RM=del

# Ficheros .o del codigo por orden
OBJS= random.o\
distribsMerit.o\
#distribs2.o\
#distribs.o

# Para compilar y crear ejecutable 'make' o 'make prog'
prog: $(OBJS)
	$(LD) -o $@  $(OBJS)

%.o: %.f95
	$(FC) -c $< $(FFLAGS) -o $@

# Para borrar archivos antiguos 'make clean'
clean:
	$(RM) $(OBJS) *.dat *.exe
