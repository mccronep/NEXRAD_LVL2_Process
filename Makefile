SHELL=/bin/sh

include MakeInclude
#
# This makefile was produced by /usr/bin/ftnmgen
# at 11:03:17 AM on 05/22/02
#
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/ftnmgen
# and this makefile use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new source file add the name of the source file to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.
#
SRCS=	level_ii_to_ascii.f unpkdt_4.f unpkdt_2.f slen.f \
        print_date_time.f uf_print_date_time.f Lvl2_to_CompZ.f \
	polar_to_polar.f ave.f byte_swap_i2.f byte_swap_i4.f byte_swap_r4.f

OBJS=	level_ii_to_ascii.o unpkdt_4.o unpkdt_2.o slen.o \
        print_date_time.o  uf_print_date_time.o Lvl2_to_CompZ.o \
	polar_to_polar.o ave.o byte_swap_i2.o byte_swap_i4.o  byte_swap_r4.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# FFLAGS	Flags to the compiler
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =		$(RADAR_FC)
FFLAGS =	
LDFLAGS = $(RADAR_LDFLAGS)
LIBS =		
CMD =		level_ii_to_ascii.exe

# Rules to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

level_ii_to_ascii.o:	remap.inc
radar_data_out:	remap.inc

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile
