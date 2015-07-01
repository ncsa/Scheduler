#!/bin/bash

#PBS_JOBID=1365082.nid11293

#------------------------------------------------
# set the number of MPI tasks per node
# the range of acceptable values is from 1 to 22
# Cray system bug prevents the top limit being 32
TPN=16

# set number of nodes to run the job on (same as #PBS -l nodes=X)
NNODES=10

# set name of nodelist file
HOSTLIST=znodelist

# set name of launch script
LAUNCH=zstart.sh

# END PARAMETER SECTION
#------------------------------------------------

# create hostlist
cat $HOME/.crayccm/ccm_nodelist.$PBS_JOBID | sort -u | awk -v n=$TPN '{for(i=0;i<n;i++) print $0}' > $HOSTLIST

# compute total number of MPI tasks
let NTASKS=$NNODES*$TPN 

# create MPI start script
echo "#!/bin/bash
cd ${HOME}/scheduler/ccm
$HOME/openmpi/bin/mpirun -v -np $NTASKS --mca btl tcp,self --mca btl_tcp_if_include ipogif0 --hostfile $HOSTLIST -npernode $TPN $HOME/scheduler/ccm/scheduler-ccm.x joblist /bin/bash > out.log" > $LAUNCH
chmod 755 $LAUNCH

