#!/bin/ksh
#
##############################################################
# Derive a composite reflectivity product from a decompressed
# NEXRAD level II data file and output the result to a 
# formatted text file.
#
# Author: Paul Harasti (NRL)
#         June 2016
#
##############################################################
if [ "$#" -lt 2 ]; then
    clear
    echo "Usage: decoder.ksh RSID fname"
    echo "  where "

    echo "   RSID:    The 4-digit radar station ID (UPPER CASE)"
    echo " "

    echo "   fname:   The name of the file to be decoded"
    echo " "

    exit
fi
set -x
ulimit -s 1000000
#
##############################################################
# Get input from the calling scripts
##############################################################
radar_stn=$1
file_name=$2
##############################################################
# if run on a linux machine: set linux = t
# otherwise                  set linux = f
##############################################################
export LINUX=t
##############################################################
# set up directories
##############################################################
#
#------------------------------------------------------------------
##dataDir=/data/harasti/radar_archive/level2/$radar_stn
#
dataDir=/satdat/m4b/NEXRAD/NEXRAD_Display/data/$radar_stn
#------------------------------------------------------------------
##savDir=/users/harasti/FNMOC/NEXRAD_Display/NEXRAD_LVL2_Process
#
savDir=/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_LVL2_Process/save
#------------------------------------------------------------------
##exeDir=/users/harasti/FNMOC/NEXRAD_Display/NEXRAD_LVL2_Process
#
exeDir=/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_LVL2_Process
#------------------------------------------------------------------
##runDir=/data/harasti/radar_archive/level2/run/$radar_stn
#
runDir=/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_LVL2_Process/run/$radar_stn
#--------------------------------------------------------------------------
#
logDir=/satdat/m4b/NEXRAD/NEXRAD_Display/NEXRAD_LVL2_Process/logdata
#
#--------------------------------------------------------------------------

export DATA_PATH=$runDir/
export OUT_DATA_PATH=$savDir/

# Make directories if they aren't already made

if [ ! -d ${savDir} ]
then
   mkdir -p ${savDir}
fi

if [ ! -d ${runDir} ]
then
   mkdir -p ${runDir}
fi

# Run from runDir

cd $runDir

##############################################################
# Check for existence of file_name
##############################################################

  if [[ -z $file_name ]]
  then
    echo 'file name = ' $file_name ' is not available'
    exit
  else 
    typeset -l station=${radar_stn}
    cp $file_name ${station}_newest_file
    #cp $dataDir/$file_name ${station}_newest_file

#   ##############################################################
#   # create composite reflecitivity product 
#   ##############################################################

    export RSID=${station}
    typeset -l lvl2_file=${file_name}
    export FNAME=${station}_newest_file
    #time $exeDir/level_ii_to_ascii.exe > $logDir/log_${lvl2_file}_CompZ.s
    time $exeDir/level_ii_to_ascii.exe
  fi

##############################################################
# job ends
##############################################################
exit
