#!/bin/bash
cd /mnt/a/u/staff/anisimov/scheduler/ccm
/u/staff/anisimov/openmpi/bin/mpirun -v -np 32 --mca btl tcp,self --mca btl_tcp_if_include ipogif0 --hostfile znodelist -npernode 16 /u/staff/anisimov/scheduler/ccm/scheduler-ccm.x joblist /bin/bash > log
