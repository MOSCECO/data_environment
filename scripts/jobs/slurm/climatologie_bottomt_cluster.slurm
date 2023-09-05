#!/bin/bash

# Nombre de noeuds
#SBATCH --nodes=1

# Nombre de processeurs par noeud
#SBATCH --ntasks-per-node=28

# Nom du job
#SBATCH --job-name=climSBT

# Quantité de RAM par noeud
#SBATCH --mem=250G

# Type de machine
#SBATCH --partition=type_2

# Chargement des modules
module load userspace/tr17.10
module load biology

module load lapack/3.7.1
module load gcc/7.2.0
# module load jags/4.3.0
module load jags/gcc72/4.3.0
module load udunits/2.2.26
module load gdal/3.2.2
module load proj/8.0.0
module load proj.4/4.9.3
module load geos/3.11.2
module load R/4.3.1

# script
Rscript /mnt/beegfs/gmaniel/MOSCECO_L2/data_environment/scripts/climatologie_bottomt_cluster.R
