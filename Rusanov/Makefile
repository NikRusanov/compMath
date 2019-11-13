FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized 
FOPT=-O3 
DIR=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
all: clean build run

clean:
		rm -rf obj/*
			rm -rf bin/*

build:
build:
	$(FC) $(FFLAGS) -c "$(DIR)/../environment_x64.f90" -J "$(DIR)/obj/" -o "$(DIR)/obj/environment.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/DECOMP.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/DECOMP.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/SOLVE.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/SOLVE.o"
	$(FC) $(FFLAGS) $(FOPT) -c "$(DIR)/src/main.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/main.o"
	$(FC) $(FFLAGS) $(FOPT) -o "$(DIR)/bin/app" "$(DIR)/obj/environment.o" "$(DIR)/obj/DECOMP.o" "$(DIR)/obj/SOLVE.o" "$(DIR)/obj/main.o"	
run:
		cd ./bin; ./app;
			cat bin/output.txt
