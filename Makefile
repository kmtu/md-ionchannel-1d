# Defining variables ===============================
FC = gfortran
#MODFLAG = -module	#ifort
MODFLAG = -J		#gfortran
FCFLAGS = $(MODFLAG)$(MODDIR)

SRCDIR = ./src
#OBJDIR = ./obj
MODDIR = ./mod
#LIBDIR = $(SRCDIR)/lib
OUTDIR = ./out

#LIB = $(LIBDIR)*.o
PROGRAM = MDSimulation1D
PROGRAM := $(addprefix $(OUTDIR)/, $(PROGRAM))

#vpath %.f90 $(SRCDIR):$(LIBDIR)
#vpath %.o $(OBJDIR)
#vpath % $(OUTDIR)

OBJS = MDSimulation1D.o Initializer.o Interactor.o Integrator.o Sampler.o \
       Generator.o Directives.o MDParameters.o

OBJS := $(addprefix $(SRCDIR)/, $(OBJS))

# Program ==================================
.PHONY : all 
all : $(PROGRAM)
# ---------------------------

$(OBJS): | $(OUTDIR) $(MODDIR)

$(OUTDIR):
	mkdir -p $(OUTDIR)

$(MODDIR):
	mkdir -p $(MODDIR)

$(OUTDIR)/MDSimulation1D : $(OBJS)

$(SRCDIR)/MDSimulation1D.o : $(addprefix $(SRCDIR)/, MDParameters.o Initializer.o Interactor.o\
            Integrator.o Sampler.o Generator.o)

$(SRCDIR)/Initializer.o : $(addprefix $(SRCDIR)/, MDParameters.o Directives.o)

$(addprefix $(SRCDIR)/, Interactor.o, Integrator.o, Sampler.o, Generator.o) : $(SRCDIR)/MDParameters.o


# Library ===================================
#gammq.o : gammq.f90 gcf.o gser.o
#	$(FC) -o $(LIBDIR)/$(@F) -c $< $(FCFLAGS)


# =========================
%.o : %.f90
	$(FC) -c $(FCFLAGS) $< -o $@

#==================================================
.PHONY: clean
clean :
	rm -f $(MODDIR)/*.mod 
	rm -f $(addprefix $(OUTDIR)/, $(PROGRAM) ) 
	rm -f $(SRCDIR)/*~ $(SRCDIR)/*.o
