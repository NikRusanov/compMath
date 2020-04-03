FC=gfortran
FFLAGS=-Wall -fimplicit-none  -std=f2008ts -Wno-maybe-uninitialized 
FOPT=-O0 
DIR=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
all: clean build run

clean:
		rm -rf obj/*
		rm -rf bin/*

build:
	$(FC) $(FFLAGS) -c "$(DIR)/../environment_x64.f90" -J "$(DIR)/obj/" -o "$(DIR)/obj/environment.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/RKF45.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/RKF45.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/RKFS.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/RKFS.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/FEHL.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/FEHL.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/QUANC8.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/QUANC8.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/ZEROIN.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/ZEROIN.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/DECOMP.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/DECOMP.o"
	$(FC) $(FFLAGS) -c "$(DIR)/../_forsythe/src/SOLVE.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/SOLVE.o"
	$(FC) $(FFLAGS) $(FOPT) -c "$(DIR)/src/funA.f90" -I "$(DIR)/obj/" -o "$(DIR)/obj/funA.o"
	$(FC) $(FFLAGS) $(FOPT) -o "$(DIR)/bin/app" "$(DIR)/obj/environment.o" "$(DIR)/obj/RKF45.o" "$(DIR)/obj/RKFS.o" "$(DIR)/obj/FEHL.o" "$(DIR)/obj/QUANC8.o"  "$(DIR)/obj/ZEROIN.o" "$(DIR)/obj/funA.o"	"$(DIR)/obj/SOLVE.o" "$(DIR)/obj/DECOMP.o" 
run:
		cd ./bin; ./app >> output.txt;
		cat ./bin/output.txt

