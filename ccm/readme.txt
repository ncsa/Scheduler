# Use of the launcher under Cray Cluster Compatibility Mode (CCM)

# add in $HOME/.modules
module swap PrgEnv-cray PrgEnv-gnu 

# add to $HOME/.bashrc
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/openmpi/lib
export PATH=$PATH:$HOME/openmpi/bin

# Compilation for CCM-mode: install OpenMPI in $HOME/openmpi
cd $HOME
mkdir openmpi
cd openmpi
wget http://www.open-mpi.org/software/ompi/v1.8/downloads/openmpi-1.8.4.tar.gz
tar zxvf openmpi-1.8.4.tar.gz
cd openmpi-1.8.4
./configure --prefix=$HOME/openmpi --enable-orterun-prefix-by-default --enable-mca-no-build=plm-tm,ras-tm
make install

# install the application launcher in $HOME
tar zxvf $HOME/scheduler.tgz
cd scheduler/ccm
mpif90 -o scheduler-ccm.x ../scheduler.F90

# running the tests
cd $HOME/scheduler/ccm

# delete the results of the previous test
rm 0*/*.slog log z*

# start the batch job
qsub run.pbs

# in run.qsub, adjust the number of tasks per node

# in interactive CCM job, use zstart.sh, which is created by first execution of run.pbs
ccmrun $HOME/scheduler/ccm/zstart.sh


