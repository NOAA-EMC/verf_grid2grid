#!/bin/ksh 
########################################################################################################
#
#  prepg2g.sh is the script to prepare grid2grid verification, doing following steps 
#       after read in user-defined control file: 
#      (1) Check verification types:      
#          case 1: one verification data to verify multiple cycles of previous forecast
#          case 2: one cycle forecast verified by different verification data at different lead time
#      (2) According to the verification type (case 1 or case 2), 
#           (i)  construct the forecast and observation file names according to filename formats set by user
#           (ii) construct the forecast-verification time-macth pair for all forecast times 
#                and store them into a temp file g2g.ctl
#      (3) Saerch GRIB files for both regular variables and tendency variables. Tendency 
#          computation (3hr, 6hr, 12hr and 24hr) needs GRIB files previous 3, 6, 12, 24 hour before 
#      (4) Thin the GRIB files and concatenate all forecast GRIB into one GRIB file and all
#          verification GRIB files into one GRIB file
#      (5) If Wind speed is specified in the control, then also grab its U and V components
#      (6) Save all headers of user-defined parameters into g2g.ctl
#
#    usage: prepg2g.sh < user-defined control file
#
#    Author: Binbin Zhou, NCEP/EMC
#            Feb, 2005
#            B. Zhou 2014-11-15, upgraded to grib2
#    
#########################################################################################################


#function to get field's seraching string from ECMWF member files
get_field_string_special() {

if [ $k4 -eq 16 ] && [ $k5 -eq 196 ] ; then
k4=$1
k5=$2
k6=$3
k7=$4
p=$5

   echo "parmcat=$k4 parm=$k5:entire atmosphere"

else
   echo "No such a field"
fi

} 

get_field_string_ecmwf(){

k4=$1
k5=$2
k6=$3
k7=$4
p=$5

#Temperature
if [ $k4 -eq 0 ] && [ $k5 -eq 0 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo "parm=0:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo "parm=0:$p mb"
  fi
#Height
elif [ $k4 -eq 3 ] && [ $k5 -eq 5 ] ; then

  if [ $k6 -eq 100 ] ; then
   echo "parm=5:$p mb"
  fi

#U wind
elif [ $k4 -eq 2 ] && [ $k5 -eq 2 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo "parm=2:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo "parm=2:$p mb"
  fi

#V wind
elif [ $k4 -eq 2 ] && [ $k5 -eq 3 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo "parm=3:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo "parm=3:$p mb"
  fi
else

  echo "Variable not defined, add it into get_field_string_ecmwf function!"

fi

}


#function to get field's searching string  
get_field_string(){

k4=$1
k5=$2
k6=$3
k7=$4
p=$5

#Temperature
if [ $k4 -eq 0 ] && [ $k5 -eq 0 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo ":TMP:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo ":TMP:$p mb"
  fi

#Dew Point Temperature
elif [ $k4 -eq 0 ] && [ $k5 -eq 6 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo ":DPT:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo ":DPT:$p mb"
  fi



#Height
elif [ $k4 -eq 3 ] && [ $k5 -eq 5 ] ; then

  if [ $k6 -eq 1 ] ; then
   echo ":HGT:surface"
  elif [ $k6 -eq 100 ] ; then
   echo ":HGT:$p mb"
  elif [ $k6 -eq 2 ] ; then
   echo ":HGT:cloud base"
  elif [ $k6 -eq 3 ] ; then
   echo ":HGT:cloud top"
  elif [ $k6 -eq 215 ] ; then
   echo ":HGT:cloud ceiling"
  elif [ $k6 -eq 4 ] ; then
   echo ":HGT:0C isotherm"
  fi

#RH
elif [ $k4 -eq 1 ] && [ $k5 -eq 1 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo ":RH:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo ":RH:$p mb"
  fi

#U wind
elif [ $k4 -eq 2 ] && [ $k5 -eq 2 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo ":UGRD:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo ":UGRD:$p mb"
  fi

#V wind
elif [ $k4 -eq 2 ] && [ $k5 -eq 3 ] ; then

  if [ $k6 -eq 103 ] ; then
   echo ":VGRD:$k7 m "
  elif [ $k6 -eq 100 ] ; then
   echo ":VGRD:$p mb"
  fi

#Reflectivity composite
elif [ $k4 -eq 16 ] && [ $k5 -eq 196 ] ; then

   echo ":REFC:entire atmosphere"

#Reflectivity at different levels 
elif [ $k4 -eq 16 ] && [ $k5 -eq 195 ] ; then

  if [ $k6 -eq 105 ] ; then
   echo ":REFD:1 hybrid"
  elif [ $k6 -eq 103 ] ; then
   echo ":REFD:$k7 m"
  fi

#Hybrid Scan Reflectivity (temporarily used in MOSAIC) 
elif [ $k4 -eq 15 ] && [ $k5 -eq 15 ] ; then
  if [ $k6 -eq 200 ] ; then
   echo ":var discipline=0 master_table=2 parmcat=15 parm=15:"
  fi

#echo-top
elif [ $k4 -eq 16 ] && [ $k5 -eq 197 ] ; then

  if [ $k6 -eq 200 ] ; then
   echo ":RETOP:"
  fi

#1000m Reflectivity
#elif [ $k4 -eq 16 ] && [ $k5 -eq 195 ] ; then
#
#   if [ $k6 -eq 103 ] ; then
#    echo ":REFD:$k7 m "
#   fi
 
#Visibility
elif [ $k4 -eq 19 ] && [ $k5 -eq 0 ] ; then

  if [ $k6 -eq 1 ] ; then
   echo ":VIS:surface"
  elif [ $k6 -eq 3 ] ; then
   echo ":VIS:cloud"
  fi

#Mean Sea Level Pressure
elif [ $k4 -eq 3 ] && [ $k5 -eq 1 ] ; then

  if [ $k6 -eq 101 ] ; then
   echo ":PRMSL:mean sea level"
  fi

#Absolute voticity
elif [ $k4 -eq 2 ] && [ $k5 -eq 10 ] ; then
  if [ $k6 -eq 100 ] ; then
   echo ":ABSV:$p mb"
  fi

#Total cloud
elif [ $k4 -eq 6 ] && [ $k5 -eq 1 ] ; then
#  if [ $k6 -eq 200 ] || [ $k6 -eq 10 ] ; then
   echo ":TCDC:entire atmosphere"
#  fi

#Smoke/dust for forecast file, Smoke  and dust use same ID
elif [ $k4 -eq 13 ] && [ $k5 -eq 195 ] ; then
  if [ $k6 -eq 103 ] ; then
   echo "LIPMF:"
  fi
#Smoke/dust for ovserved file, Smoke and dust use same ID
elif [ $k4 -eq 3 ] && [ $k5 -eq 10 ] ; then
  if [ $k6 -eq 103 ] ; then
   echo "DEN:"
  fi

#CMAQX AOD/AOT
elif [ $k4 -eq 20 ] && [ $k5 -eq 102 ] ; then
  if [ $k6 -eq 0 ] ; then
   echo "AOTK"
  fi
#CMAQ and NESDIS AOD use IMGD
elif [ $k4 -eq 255 ] && [ $k5 -eq 255 ] ; then
  if [ $k6 -eq 200 ] ; then
   echo "IMGD"
  fi

#Cape 
elif [ $k4 -eq 7 ] && [ $k5 -eq 6 ] ; then
  if [ $k6 -eq 1 ] && [ $k7 -eq 0 ] ; then
    echo "CAPE:surface"
  elif [ $k6 -eq 108 ] ; then
    echo "CAPE:$k7"
  else
    echo "CAPE not found"
    exit
  fi


elif [ $k4 -eq 19 ] && [ $k5 -eq 20 ] ; then
  if [ $k6 -eq 100 ] ; then
   echo ":ICIP:$p mb"
  fi

elif [ $k4 -eq 1 ] && [ $k5 -eq 8 ] ; then
  echo ":APCP:surface"

else
 
  echo "Variable not defined, add it into get_field_string function!"

fi


}

set -x 
wgrb=/nwprod/util/exec
gribindex=${gribindex:-/nwprod/util/exec/grbindex}
cp $PARMverf_g2g/verf_g2g.grid104 grid#104
cp $PARMverf_g2g/verf_g2g.regions regions


rm -f g2g.ctl


# Specify the tendency buttons and cloud base/top care here:
export tnd03=${tnd03:-'close'}
export tnd06=${tnd06:-'close'}
export tnd12=${tnd12:-'close'}
export tnd24=${tnd24:-'close'}
export cloud_base_from_sfc=${cloud_base_from_sfc:-"no"}
export lat_weight=${lat_weight:-"no"}



# Now begin to read user-control file #########################################################

 read LINE                     #Header 1
  echo $LINE >> g2g.ctl
  set -A aaa $LINE
  ens2p5=${aaa[2]}             #to test if it is "NAEFS2P5"  
 read LINE                     #Header 2
  echo $LINE >> g2g.ctl
  set -A mdl $LINE
  model=${mdl[1]}
  echo "model="$model

# Check case type #############################################################################
 read LINE                     #Header 3
   set -A tfcst $LINE                                                 
   if [ ${tfcst[1]} -gt 2000000000 ] ; then    #if is case2:diff lead times from 1 forecast cycle vs diff verified data
     fcst[0]=${tfcst[1]}
   else                                        #if is case1:one verified data vs different previous cycle forecasts
     f[0]=${tfcst[1]}                           #cycle
     t=1
     while [ $t -lt ${tfcst[0]} ]
      do 
       read LINE
       f[$t]=$LINE                              #cycle
       t=`echo "$t + 1" | bc`
      done
   fi

# Construct forecast and observation file names for different cases (1 or 2) #####################
# and form tendency file pairs

 read LINE                     #Header 4
   set -A tobsv $LINE

   echo verification time: ${tobsv[1]}
  
   if [ ${tobsv[1]} -gt 2000000000 ] ; then    #case1:one verified data  vs different previous cycle forecasts
     cas=1
     obsv[0]=${tobsv[1]}
     oday[0]=`echo ${obsv[0]} | cut -c 1-8`
     obsv03[0]=`/nwprod/util/exec/ndate -3 ${obsv[0]}`
     oday03[0]=`echo ${obsv03[0]} | cut -c 1-8`
     obsv06[0]=`/nwprod/util/exec/ndate -6 ${obsv[0]}`
     oday06[0]=`echo ${obsv06[0]} | cut -c 1-8`
     obsv12[0]=`/nwprod/util/exec/ndate -12 ${obsv[0]}`
     oday12[0]=`echo ${obsv12[0]} | cut -c 1-8`
     obsv24[0]=`/nwprod/util/exec/ndate -24 ${obsv[0]}`
     oday24[0]=`echo ${obsv24[0]} | cut -c 1-8`

     to=`echo ${obsv[0]} | cut -c 9-10`                           #obsv cycle
     to03=`echo ${obsv03[0]} | cut -c 9-10`                       #obsv cycle for presious 12 hr
     to06=`echo ${obsv06[0]} | cut -c 9-10`                       #obsv cycle for presious 12 hr
     to12=`echo ${obsv12[0]} | cut -c 9-10`                       #obsv cycle for presious 12 hr
     to24=`echo ${obsv24[0]} | cut -c 9-10`                       #obsv cycle for presious 12 hr

     fileobsv[0]=$obsvdir.${oday[0]}/$ohead.t${to}z.${ogrbtype}${otail}$otm
     fileobsv03[0]=$obsvdir.${oday03[0]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
     fileobsv06[0]=$obsvdir.${oday06[0]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
     fileobsv12[0]=$obsvdir.${oday12[0]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
     fileobsv24[0]=$obsvdir.${oday24[0]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm

     echo fileobsv[0]=${fileobsv[0]}

     nt=`expr $t - 1`  #     nt=total # of fcst times, ie ${tfcst[0]}
     t=0
     echo ${tfcst[0]}"  "forecasts:Ovservations >> g2g.ctl    
     while [ $t -le $nt ]
     do
       pass=` /nwprod/util/exec/ndate -${f[$t]} ${obsv[0]}`
       fday[$t]=`echo ${pass} | cut -c 1-8`
       fcst[$t]=$pass
       tf=`echo ${pass} | cut -c 9-10`                                    #fcst cycle


       if [ ${f[$t]} -lt 10 ] ; then
         f[$t]='0'${f[$t]}
       fi
       filefcst[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f[$t]}$ftm

       echo filefcst[$t]=${filefcst[$t]}

                                                                                                                                          
       f03[$t]=`expr ${f[$t]} - 3`
       f06[$t]=`expr ${f[$t]} - 6`
       f12[$t]=`expr ${f[$t]} - 12`
       f24[$t]=`expr ${f[$t]} - 24`


       if [ ${f03[$t]} -lt 0 ] ; then
        f03[$t]='NN'
       elif [ ${f03[$t]} -ge 0 ] && [ ${f03[$t]} -lt 10 ] ; then
        f03[$t]='0'${f03[$t]}
       fi

       if [ ${f06[$t]} -lt 0 ] ; then
        f06[$t]='NN'
       elif [ ${f06[$t]} -ge 0 ] && [ ${f06[$t]} -lt 10 ] ; then
        f06[$t]='0'${f06[$t]}
       fi

       if [ ${f12[$t]} -lt 0 ] ; then
        f12[$t]='NN'
       elif [ ${f12[$t]} -ge 0 ] && [ ${f12[$t]} -lt 10 ] ; then
        f12[$t]='0'${f12[$t]}
       fi

       if [ ${f24[$t]} -lt 0 ] ; then
        f24[$t]='NN'
       elif [ ${f24[$t]} -ge 0 ] && [ ${f24[$t]} -lt 10 ] ; then
        f24[$t]='0'${f24[$t]}
       fi


       filefcst03[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f03[$t]}$ftm
       filefcst06[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f06[$t]}$ftm
       filefcst12[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f12[$t]}$ftm
       filefcst24[$t]=$fcstdir.${fday[$t]}/$fhead.t${tf}z.${fgrbtype}${f24[$t]}$ftm


       if [ -s ${filefcst[$t]} ] ; then
         echo ${fday[$t]}${tf}${f[$t]}" "${oday[0]}${to}00" "${fday[$t]}${tf}${f03[$t]}" "${oday03[0]}${to03}00" "${fday[$t]}${tf}${f06[$t]}" "${oday06[0]}${to06}00" "${fday[$t]}${tf}${f12[$t]}" "${oday12[0]}${to12}00" "${fday[$t]}${tf}${f24[$t]}" "${oday24[0]}${to24}00 >> g2g.ctl
       else
         echo forecast files: ${filefcst[$t]}  not exist
          echo ${fday[$t]}${tf}${f[$t]}" "${oday[0]}${to}00" "${fday[$t]}${tf}${f03[$t]}" "${oday03[0]}${to03}00" "${fday[$t]}${tf}${f06[$t]}" "${oday06[0]}${to06}00" "${fday[$t]}${tf}${f12[$t]}" "${oday12[0]}${to12}00" "${fday[$t]}${tf}${f24[$t]}" "${oday24[0]}${to24}00 >> g2g.ctl
       fi
        t=`expr $t + 1`
     done

   else                   #case2:diff lead times from one forecast  vs diferent later-on verified data
     cas=2
     fday[0]=`echo ${fcst[0]} | cut -c 1-8`
     tf=`echo ${fcst[0]} | cut -c 9-10`      #fcst cycle

     b[0]=${tobsv[1]}

     b03[0]=`expr ${b[0]} - 03`
     b06[0]=`expr ${b[0]} - 06`
     b12[0]=`expr ${b[0]} - 12`
     b24[0]=`expr ${b[0]} - 24`

     if [ b03[0] -lt 0 ] ; then
       b03[0]='NN'
     elif [ b03[0] -ge 0 ] && [ b03[0] -lt 10 ]; then
       b03[0]='0'${b03[0]}
     fi

     if [ b06[0] -lt 0 ] ; then
       b06[0]='NN'
     elif [ b06[0] -ge 0 ] && [ b06[0] -lt 10 ]; then
       b06[0]='0'${b06[0]}
     fi

     if [ b12[0] -lt 0 ] ; then
       b12[0]='NN'
     elif [ b12[0] -ge 0 ] && [ b12[0] -lt 10 ]; then
       b12[0]='0'${b12[0]}
     fi

     if [ b24[0] -lt 0 ] ; then
       b24[0]='NN'
     elif [ b24[0] -ge 0 ] && [ b24[0] -lt 10 ]; then
       b24[0]='0'${b24[0]}
     fi


     pass=`/nwprod/util/exec/ndate +${b[0]} ${fcst[0]}`
     pass03=`/nwprod/util/exec/ndate -3 $pass`
     pass06=`/nwprod/util/exec/ndate -6 $pass`
     pass12=`/nwprod/util/exec/ndate -12 $pass`
     pass24=`/nwprod/util/exec/ndate -24 $pass`

     oday[0]=`echo ${pass} | cut -c 1-8`
     obsv[0]=$pass
     oday03[0]=`echo ${pass03} | cut -c 1-8`  #observed day
     obsv03[0]=$pass03                        #observed time(valid time)
     oday06[0]=`echo ${pass06} | cut -c 1-8`  #observed day
     obsv06[0]=$pass06                        #observed time(valid time)
     oday12[0]=`echo ${pass12} | cut -c 1-8`  #observed day
     obsv12[0]=$pass12                        #observed time(valid time)
     oday24[0]=`echo ${pass24} | cut -c 1-8`  #observed day
     obsv24[0]=$pass24                        #observed time(valid time)

     if [ b[0] -lt 10 ] ; then
       b[0]='0'${b[0]}
     fi

     to=`echo ${pass} | cut -c 9-10`       #obsv cycle 
     filefcst[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b[0]}$ftm
     fileobsv[0]=$obsvdir.${oday[0]}/$ohead.t${to}z.${ogrbtype}${otail}$otm

     to03=`echo ${pass03} | cut -c 9-10`   #obsv cycle
     filefcst03[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b03[0]}$ftm
     fileobsv03[0]=$obsvdir.${oday03[0]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
     to06=`echo ${pass06} | cut -c 9-10`   #obsv cycle
     filefcst06[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b06[0]}$ftm
     fileobsv06[0]=$obsvdir.${oday06[0]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
     to12=`echo ${pass12} | cut -c 9-10`   #obsv cycle
     filefcst12[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b12[0]}$ftm
     fileobsv12[0]=$obsvdir.${oday12[0]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
     to24=`echo ${pass24} | cut -c 9-10`   #obsv cycle
     filefcst24[0]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b24[0]}$ftm
     fileobsv24[0]=$obsvdir.${oday24[0]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm



     if [ -s ${filefcst[0]} ] && [ -s ${fileobsv[0]} ] ; then
         echo ${tobsv[0]}"  "forecasts~Ovservations >> g2g.ctl
         echo ${fday[0]}${tf}${b[0]}" "${oday[0]}${to}00" "${fday[0]}${tf}${b03[0]}" "${oday03[0]}${to03}00" "${fday[0]}${tf}${b06[0]}" "${oday06[0]}${to06}00" "${fday[0]}${tf}${b12[0]}" "${oday12[0]}${to12}00" "${fday[0]}${tf}${b24[0]}" "${oday24[0]}${to24}00 >> g2g.ctl
     else
         echo ${filefcst[0]} or ${fileobsv[0]} not exist
echo ${fday[0]}${tf}${b[0]}" "${oday[0]}${to}00" "${fday[0]}${tf}${b03[0]}" "${oday03[0]}${to03}00" "${fday[0]}${tf}${b06[0]}" "${oday06[0]}${to06}00" "${fday[0]}${tf}${b12[0]}" "${oday12[0]}${to12}00" "${fday[0]}${tf}${b24[0]}" "${oday24[0]}${to24}00 >> g2g.ctl
     fi

     t=1
     while [ $t -lt ${tobsv[0]} ]
      do
       read LINE
       b[$t]=$LINE
       b03[$t]=`expr ${b[$t]} - 3`
       b06[$t]=`expr ${b[$t]} - 6`
       b12[$t]=`expr ${b[$t]} - 12`
       b24[$t]=`expr ${b[$t]} - 24`

       if [ ${b03[$t]} -lt 0 ] ; then
         b03[$t]='NN'
       elif [ ${b03[$t]} -ge 0 ] && [ ${b03[$t]} -lt 10 ] ; then
        b03[$t]='0'${b03[$t]} 
       fi

       if [ ${b06[$t]} -lt 0 ] ; then
         b06[$t]='NN'
       elif [ ${b06[$t]} -ge 0 ] && [ ${b06[$t]} -lt 10 ] ; then
        b06[$t]='0'${b06[$t]}
       fi

       if [ ${b12[$t]} -lt 0 ] ; then
         b12[$t]='NN'
       elif [ ${b12[$t]} -ge 0 ] && [ ${b12[$t]} -lt 10 ] ; then
        b12[$t]='0'${b12[$t]}
       fi

       if [ ${b24[$t]} -lt 0 ] ; then
         b24[$t]='NN'
       elif [ ${b24[$t]} -ge 0 ] && [ ${b24[$t]} -lt 10 ] ; then
        b24[$t]='0'${b24[$t]}
       fi


       pass=`/nwprod/util/exec/ndate +${b[$t]} ${fcst[0]}`
       pass03=`/nwprod/util/exec/ndate -3 $pass`
       pass06=`/nwprod/util/exec/ndate -6 $pass`
       pass12=`/nwprod/util/exec/ndate -12 $pass`
       pass24=`/nwprod/util/exec/ndate -24 $pass`

       oday[$t]=`echo ${pass} | cut -c 1-8`
       obsv[$t]=$pass 
       oday03[$t]=`echo ${pass03} | cut -c 1-8`
       obsv03[$t]=$pass03
       oday06[$t]=`echo ${pass06} | cut -c 1-8`
       obsv06[$t]=$pass06
       oday12[$t]=`echo ${pass12} | cut -c 1-8`
       obsv12[$t]=$pass12
       oday24[$t]=`echo ${pass24} | cut -c 1-8`
       obsv24[$t]=$pass24

       if [ b[$t] -lt 10 ] ; then
         b[$t]='0'${b[$t]}
       fi

       to=`echo ${pass} | cut -c 9-10`
       filefcst[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b[$t]}$ftm
       fileobsv[$t]=$obsvdir.${oday[$t]}/$ohead.t${to}z.${ogrbtype}${otail}$otm

       to03=`echo ${pass03} | cut -c 9-10`
       filefcst03[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b03[$t]}$ftm
       fileobsv03[$t]=$obsvdir.${oday03[$t]}/$ohead.t${to03}z.${ogrbtype}${otail}$otm
       to06=`echo ${pass06} | cut -c 9-10`
       filefcst06[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b06[$t]}$ftm
       fileobsv06[$t]=$obsvdir.${oday06[$t]}/$ohead.t${to06}z.${ogrbtype}${otail}$otm
       to12=`echo ${pass12} | cut -c 9-10`
       filefcst12[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b12[$t]}$ftm
       fileobsv12[$t]=$obsvdir.${oday12[$t]}/$ohead.t${to12}z.${ogrbtype}${otail}$otm
       to24=`echo ${pass24} | cut -c 9-10`
       filefcst24[$t]=$fcstdir.${fday[0]}/$fhead.t${tf}z.${fgrbtype}${b24[$t]}$ftm
       fileobsv24[$t]=$obsvdir.${oday24[$t]}/$ohead.t${to24}z.${ogrbtype}${otail}$otm


       if [ -s ${filefcst[$t]} ] && [ -s ${fileobsv[$t]} ] ; then
         echo ${fday[0]}${tf}${b[$t]}" "${oday[$t]}${to}00" "${fday[0]}${tf}${b03[$t]}" "${oday03[$t]}${to03}00" "${fday[0]}${tf}${b06[$t]}" "${oday06[$t]}${to06}00" "${fday[0]}${tf}${b12[$t]}" "${oday12[$t]}${to12}00" "${fday[0]}${tf}${b24[$t]}" "${oday24[$t]}${to24}00 >> g2g.ctl
       else
         echo ${filefcst[$t]} or ${fileobsv[$t]} not exist
echo ${fday[0]}${tf}${b[$t]}" "${oday[$t]}${to}00" "${fday[0]}${tf}${b03[$t]}" "${oday03[$t]}${to03}00" "${fday[0]}${tf}${b06[$t]}" "${oday06[$t]}${to06}00" "${fday[0]}${tf}${b12[$t]}" "${oday12[$t]}${to12}00" "${fday[0]}${tf}${b24[$t]}" "${oday24[$t]}${to24}00 >> g2g.ctl
       fi

       t=`expr $t + 1`
      done
   fi


 read LINE                     #Header 5
   echo $LINE >> g2g.ctl
   set -A obtyp $LINE
   loop=${obtyp[0]}
   obsrvtype=${obtyp[1]}
   echo obsrvtype=$obsrvtype
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done

 read LINE                     #Header 6
   echo $LINE >> g2g.ctl
   set -A grdtyp $LINE
   loop=${grdtyp[0]}
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done  

 read LINE                    #Header 7
   echo $LINE >> g2g.ctl
   set -A statyp $LINE
   loop=${statyp[0]}
   while [ $loop -gt 1 ]
    do
     read LINE
     echo $LINE >> g2g.ctl
     loop=`expr $loop - 1`
   done

 read LINE                   #Header 8
   set -A var $LINE
   echo $LINE >> g2g.ctl

   k4[1]=${var[2]}
   k5[1]=${var[3]}
   k6[1]=${var[4]}
   k7[1]=${var[5]}
   if [ ${#var[@]} -gt 6 ] ; then
    fho[1]=${var[6]}
    thr[1]=${var[7]}
   else 
    fho[1]='no_prob'
   fi

   nvar=${var[0]}
   loop=2
   while [ $loop -le $nvar ]
    do
     read LINE
     set -A var $LINE
     echo $LINE  >> g2g.ctl
     k4[$loop]=${var[1]}
     k5[$loop]=${var[2]}
     k6[$loop]=${var[3]}
     k7[$loop]=${var[4]}
     if [ ${#var[@]} -gt 5 ] ; then
      fho[$loop]=${var[5]}
      thr[$loop]=${var[6]}
     else
      fho[$loop]='no_prob'
     fi
     loop=`expr $loop + 1`
   done


 read LINE                      #Header 9
   set -A level $LINE
   echo $LINE >> g2g.ctl

   nlevel=${level[0]}
   p[1]=${level[1]:1}
   loop=2
   while [ $loop -le $nlevel ] ; do
     read LINE
     set -A level $LINE
     echo $LINE >> g2g.ctl
     p[$loop]=${level[0]:1}
     loop=`expr $loop + 1`  
   done

  #Now pressure level array p[i], i=1,nlevel (nlevel at least is 1)


# Begin to thin the GRIB files #############################################################################################

   rm -f obsv.grib fcst.grib obsv03.grib fcst03.grib obsv06.grib fcst06.grib obsv12.grib fcst12.grib obsv24.grib fcst24.grib

   varslp=0

ens=` echo $model | cut -c 1-4`
echo CASE $cas Model: $ens
echo CASE $cas Model: $model 
 

   echo "BEGIN to wgrib files ............................................"

 if [ $cas -eq 1 ] ; then                  # case 1

    echo CASE  1  : One verification time vs diff cycles of  forecasts

     varslp=1
     while [ $varslp -le $nvar ]  ; do           # for all variables
      
         if [ ${k4[$varslp]} -eq 2 ] && [ ${k5[$varslp]} -eq 1 ] ; then  #for all wind vector variables

             if [ ${k6[$varslp]} -eq 100 ] ; then
               nlp=$nlevel                  #number of pressure levels for variables on pressure levels 
             else
               nlp=1                        #single level for single level variables 
             fi

             lp=1
             while [ $lp -le $nlp ] ; do

              if [ $ens = 'ecme' ] ; then
                u_string=$(get_field_string_ecmwf 2 2 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
                v_string=$(get_field_string_ecmwf 2 3 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
              else
                u_string=$(get_field_string 2 2 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
                v_string=$(get_field_string 2 3 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
              fi
                #One wind obsv cycle 

                $wgrb/wgrib2 -match  "$u_string" ${fileobsv[0]} |$wgrb/wgrib2 -i  ${fileobsv[0]} -grib uo
                cat uo >>obsv.grib
                $wgrb/wgrib2 -match  "$v_string" ${fileobsv[0]} |$wgrb/wgrib2 -i  ${fileobsv[0]} -grib vo
                cat vo >>obsv.grib
                echo  wgrib2ing ${fileobsv[0]} for $u_string and $v_string ..... 

                #Multiple wind fcst cycles
                timelp=0
                while [ $timelp -lt ${tfcst[0]} ] ; do # for all previous forecast cycles
                    echo  wgrib2ing ${filefcst[$timelp]} .......

                  if [ ${fho[$varslp]:0:5} = 'FHOP>' ] ; then
                    $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                    $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                  elif [ ${fho[$varslp]:0:5} = 'FHOP<' ] ; then
                    $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                    $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                  elif [ ${fho[$varslp]:0:5} = 'FHOP=' ] ; then
                    $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                    $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                  else
                    $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                    $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                  fi
                    cat uf >>fcst.grib
                    cat vf >>fcst.grib
                    echo  wgrib2ing ${filefcst[$timelp]} for $u_string and $v_string ..... 

                    timelp=`expr $timelp + 1`
                done

                lp=$((lp + 1))          # same as lp=`expr $lp +1` 

             done # end of all levels

         else                          #for non-wind vector variables

             if [ ${k6[$varslp]} -eq 100 ] ; then
               nlp=$nlevel                  #number of pressure levels for variables on pressure levels 
             else 
               nlp=1                        #single level for single level variables 
             fi

             lp=1
             while [ $lp -le $nlp ] ; do
           
                echo "ID: $lp $nlp ${k4[$varslp]} ${k5[$varslp]} ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]}" 
 
              if [ $ens = 'ecme' ] ; then
                kpds=$(get_field_string_ecmwf ${k4[$varslp]} ${k5[$varslp]} ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
              elif [ $ens = 'cmce' ] && [ ${k4[$varslp]} -eq 6 ] &&  [ ${k5[$varslp]} -eq 1 ] ; then
                kpds="TCDC"         #CMCE total cloud use diff smybal from "TCDC:entire atmosphere"
              else
                kpds=$(get_field_string ${k4[$varslp]} ${k5[$varslp]} ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
              fi

                echo "Search string: $kpds" 


                #One cycle obsv
                $wgrb/wgrib2 -match "$kpds" ${fileobsv[0]}|$wgrb/wgrib2 -i  ${fileobsv[0]} -grib x 
                cat x >>obsv.grib
                echo wgrib2ing ${fileobsv[0]} for $kpds .........

                if [ ${k4[$varslp]}  -eq 16 ] && [ ${k5[$varslp]} -eq 195 ] ; then #MOSAIC HSR use different ID from models
                  $wgrb/wgrib2 -match "var discipline=0 master_table=2 parmcat=15 parm=15" ${fileobsv[0]}|$wgrb/wgrib2 -i  ${fileobsv[0]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[0]} for MOSAIC HRS .........
                fi 


                if [ ${k4[$varslp]}  -eq 13 ] && [ ${k5[$varslp]} -eq 195 ] ; then #smoke/dust obsv uses different ID from HYSPLT fcst
                  $wgrb/wgrib2 -match "DEN:" ${fileobsv[0]}|$wgrb/wgrib2 -i  ${fileobsv[0]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[0]} for SMOKE obsv file .....
                fi 

               if [ ${k4[$varslp]}  -eq 20 ] && [ ${k5[$varslp]} -eq 102 ] ; then #NESDIS AOD obsv uses different ID from CMAQ fcst
                  $wgrb/wgrib2 -match "parmcat=255 parm=255" ${fileobsv[0]}|$wgrb/wgrib2 -i  ${fileobsv[0]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[0]} for AOD obsv file .....
                fi

                #Multip previous fcst cycles
                timelp=0
                while [ $timelp -lt ${tfcst[0]} ] ; do                 # for all previous forecast cycles

                  #if HRRR's echo-top, use different k4,k5,k6 values
                  #if [ $model = 'HRRR' ] && [ ${k4[$varslp]} -eq 16 ] && [ ${k5[$varslp]} -eq 197 ] && [ ${k6[$varslp]} -eq 200 ] ; then
                  #   kpds=$(get_field_string 16 3 3 0 ${p[$lp]}) 
                  #fi 

                  echo  wgrib2ing ${filefcst[$timelp]} for fcst var $varslp.......

                  if [ ${fho[$varslp]:0:5} = 'FHOP>' ] ; then
                   if [ $model = 'NARRE' ] && [ ${k4[$varslp]}  -eq 16 ] && [ ${k5[$varslp]} -eq 196 ] ; then # reflectivity in NARRE (from cnvgrib)
                    $wgrb/wgrib2 -match "parmcat=16 parm=196:" ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                   else
                    $wgrb/wgrib2 -match "$kpds" ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                   fi
                  elif [ ${fho[$varslp]:0:5} = 'FHOP<' ] ; then
                   $wgrb/wgrib2 -match "$kpds" ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                  elif [ ${fho[$varslp]:0:5} = 'FHOP=' ] ; then
                   $wgrb/wgrib2 -match "$kpds" ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                  else
                   $wgrb/wgrib2 -match "$kpds" ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                  fi

                  #until 20161116, opreational CMAQ is still grib1, after cnvgrib to grib2, its string is IMGD
                  #Only after CMAQ becomes directly output grib2, it is AOTK
                  if [ $model = 'CMAQ' ] && [ ${k4[$varslp]} -eq 20 ] && [ ${k5[$varslp]} -eq 102 ]  ; then
                   $wgrb/wgrib2 -match "IMGD:" ${filefcst[$timelp]}|$wgrb/wgrib2 -i  ${filefcst[$timelp]} -grib y
                  fi

                  cat y >>fcst.grib 
                  echo wgrib2ing ${filefcst[$timelp]} for for $kpds .........                  

                  timelp=`expr $timelp + 1` 
                done   # end of all previous forecast cycles

                lp=`expr $lp + 1`

             done # end of all levels
    
          fi     #end of non-wind variables

        varslp=`expr $varslp + 1`

     done      # end of while foop for all variables     

      if [ $cloud_base_from_sfc = "no" ] ; then
         varslp=1
         while [ $varslp -le $nvar ] ; do            # for cloud base/top, need surface height
           if [ ${k4[$varslp]} -eq 3 ] && [ ${k5[$varslp]} -eq 5 ] ; then
             if [ ${k6[$varslp]} -eq 2 ] || [ ${k6[$varslp]} -eq 3 ] ; then
               $wgrb/wgrib2 -match "HGT:surface" ${filefcst[0]} |$wgrb/wgrib2 -i ${filefcst[0]} -grib sfc.grib2
               echo wgrib2ing ${filefcst[0]} for HGT:surface ........... 
               varslp=$nvar
             fi
           fi
           varslp=$((varslp + 1))
         done
      fi

   else                                      # case 2

     echo CASE  2  : One forecast is verified by different observation at different valid time
     echo $nvar

     varslp=1
     while [ $varslp -le $nvar ] ; do            # for all variable 

       if [ ${k4[$varslp]} -eq 2 ] && [ ${k5[$varslp]} -eq 1 ] ; then #for all wind vector variables

          if [ ${k6[$varslp]} -eq 100 ] ; then
               nlp=$nlevel                  #number of pressure levels for variables on pressure levels 
          else
               nlp=1                        #single level for single level variables 
          fi

          lp=1
          while [ $lp -le $nlp ] ; do

           if [ $ens = 'ecme' ] ; then
             u_string=$(get_field_string_ecmwf 2 2 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
             v_string=$(get_field_string_ecmwf 2 3 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
           else
             u_string=$(get_field_string 2 2 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
             v_string=$(get_field_string 2 3 ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
           fi
             echo  wgrib2ing ${fileobsv[0]} for $u_string $v_string ....... 

             timelp=0
             while [ $timelp -lt ${tobsv[0]} ] ; do # for all later-on validation times


               if [ ${fho[$varslp]:0:5} = 'FHOP>' ] ; then
                  $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                  $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
               elif [ ${fho[$varslp]:0:5} = 'FHOP<' ] ; then
                  $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                  $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                elif [ ${fho[$varslp]:0:5} = 'FHOP=' ] ; then
                  $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                  $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                else
                  $wgrb/wgrib2 -match "$u_string"  ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib uf
                  $wgrb/wgrib2 -match "$v_string"  ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib vf
                fi
                cat uf >>fcst.grib
                cat vf >>fcst.grib

                $wgrb/wgrib2 -match  "$u_string" ${fileobsv[$timelp]} |$wgrb/wgrib2 -i  ${fileobsv[$timelp]} -grib uo
                $wgrb/wgrib2 -match  "$v_string" ${fileobsv[$timelp]} |$wgrb/wgrib2 -i  ${fileobsv[$timelp]} -grib vo
                cat uo >>obsv.grib
                cat vo >>obsv.grib
             
                echo  wgrib2ing ${filefcst[$timelp]} for $u_string $v_string .......
                echo  wgrib2ing ${fileobsv[$timelp]} for $u_string $v_string .......

                timelp=`expr $timelp + 1`

             done                     #done fol all validation times loop

             lp=$((lp + 1))          # same as lp=`expr $lp +1`

          done                      #done fol all levels loop
                                     
       else                        # for if all non-wind vector fields     

          if [ ${k6[$varslp]} -eq 100 ] ; then
            nlp=$nlevel                  #number of pressure levels for variables on pressure levels 
          else
            nlp=1                        #single level for single level variables 
          fi

          lp=1
          while [ $lp -le $nlp ] ; do

           if [ $ens = 'ecme' ] ; then
             kpds=$(get_field_string_ecmwf ${k4[$varslp]} ${k5[$varslp]} ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
           else
             kpds=$(get_field_string ${k4[$varslp]} ${k5[$varslp]} ${k6[$varslp]} ${k7[$varslp]} ${p[$lp]})
           fi
             echo "Search string: $kpds" 

            timelp=0
            while [ $timelp -lt ${tobsv[0]} ] ; do # for all later-on validation times

              #if HRRR's echo-top, use different k4,k5,k6 values
              #if [ $model = 'HRRR' ] && [ ${k4[$varslp]} -eq 16 ] && [ ${k5[$varslp]} -eq 197 ] && [ ${k6[$varslp]} -eq 200 ] ; then
              #     kpdsf=$(get_field_string 16 3 3 0 ${p[$lp]})
              #fi

             
              $wgrb/wgrib2 -match  "$kpds" ${fileobsv[$timelp]} |$wgrb/wgrib2 -i ${fileobsv[$timelp]} -grib x
              cat x >>obsv.grib

              if [ ${fho[$varslp]:0:5} = 'FHOP>' ] ; then

                  if [ $model = 'NARRE' ] && [ ${k4[$varslp]}  -eq 16 ] && [ ${k5[$varslp]} -eq 196 ] ; then #reflectivity in NARRE (using cnvgrib) 
                    $wgrb/wgrib2 -match "parmcat=16 parm=196:" ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                  else
                    $wgrb/wgrib2 -match  "$kpds" ${filefcst[$timelp]} |grep "prob >${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
                  fi

              elif [ ${fho[$varslp]:0:5} = 'FHOP<' ] ; then
               $wgrb/wgrib2 -match  "$kpds" ${filefcst[$timelp]} |grep "prob <${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
              elif [ ${fho[$varslp]:0:5} = 'FHOP=' ] ; then
               $wgrb/wgrib2 -match  "$kpds" ${filefcst[$timelp]} |grep "prob >=${thr[$varslp]}"|$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
              else
               $wgrb/wgrib2 -match  "$kpds" ${filefcst[$timelp]} |$wgrb/wgrib2 -i ${filefcst[$timelp]} -grib y
              fi
              cat y >>fcst.grib

              echo  wgrib2ing ${filefcst[$timelp]} for $kpds .......
              echo  wgrib2ing ${fileobsv[$timelp]} for $kpds .......

                if [ ${k4[$varslp]}  -eq 16 ] && [ ${k5[$varslp]} -eq 195 ] ; then #MOSAIC HSR use different ID from models
                  $wgrb/wgrib2 -match "var discipline=0 master_table=2 parmcat=15 parm=15" ${fileobsv[$timelp]}|$wgrb/wgrib2 -i  ${fileobsv[$timelp]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[$timelp]} for MOSAIC HSR .........
                fi

                if [ ${k4[$varslp]}  -eq 13 ] && [ ${k5[$varslp]} -eq 195 ] ; then #smoke/dust obsv uses different ID from HYSPLT fcast 
                  $wgrb/wgrib2 -match "DEN:" ${fileobsv[$timelp]}|$wgrb/wgrib2 -i  ${fileobsv[$timelp]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[$timelp]} for SMOKE obsv files .........
                fi
 
                if [ ${k4[$varslp]}  -eq 20 ] && [ ${k5[$varslp]} -eq 102 ] ; then #NESDIS AOD obsv uses different ID from CMAQ fcast
                  $wgrb/wgrib2 -match "IMGD:" ${fileobsv[$timelp]}|$wgrb/wgrib2 -i  ${fileobsv[$timelp]} -grib x
                  cat x >>obsv.grib
                  echo wgrib2ing ${fileobsv[$timelp]} for AOD obsv files .........
                fi

              timelp=`expr $timelp + 1`
            done                                  # done for all validation times loop

            lp=$((lp + 1))

          done                                    # done for all level loop

       fi    # done for if all both wind and non-wind variables                       

      varslp=$((varslp + 1)) 

     done    # done loop for all variables 

    if [ $cloud_base_from_sfc = "no" ] ; then
       varslp=1
       while [ $varslp -le $nvar ] ; do            # for cloud base/top, need surface height
         if [ ${k4[$varslp]} -eq 3 ] && [ ${k5[$varslp]} -eq 5 ] ; then
             if [ ${k6[$varslp]} -eq 2 ] || [ ${k6[$varslp]} -eq 3 ] ; then
               $wgrb/wgrib2 -match "HGT:surface" ${filefcst[0]} |$wgrb/wgrib2 -i ${filefcst[0]} -grib sfc.grib2
               varslp=$nvar
               echo  wgrib2ing ${filefcst[0]} for HGT:surface ........
             fi
         fi
         varslp=$((varslp + 1))
       done
    fi

  fi      # done for case 2

ls -l 


rm -f x y

  echo $cloud_base_from_sfc >>  g2g.ctl  #Header 10
  echo $lat_weight >> g2g.ctl            #Header 11 


   mv g2g.ctl g2g.ctl.$model
   mv fcst.grib fcst.grib.$model 
   mv obsv.grib obsv.grib.$model
 if [ $tnd03 = 'open' ] ; then
   mv fcst03.grib fcst03.grib.$model
   mv obsv03.grib obsv03.grib.$model
 fi
 if [ $tnd06 = 'open' ] ; then
   mv fcst06.grib fcst06.grib.$model
   mv obsv06.grib obsv06.grib.$model
 fi
 if [ $tnd12 = 'open' ] ; then
   mv fcst12.grib fcst12.grib.$model
   mv obsv12.grib obsv12.grib.$model
 fi
 if [ $tnd24 = 'open' ] ; then
   mv fcst24.grib fcst24.grib.$model
   mv obsv24.grib obsv24.grib.$model
 fi



if [ ${tfcst[1]} -gt 2000000000 ] ; then 
 if [ ${ens2p5:0:10} = 'NAEFS2P5/4' ] ; then
   echo naefs2p5 ${tfcst[1]} > temp
 elif [ ${ens2p5:0:10} = 'NAEFS2P5BC' ] ; then
   echo naefs2p5bc ${tfcst[1]} > temp
 else    
   echo $model ${tfcst[1]} > temp
 fi
fi
if [ ${tobsv[1]} -gt 2000000000 ] ; then
 if [ ${ens2p5:0:10} = 'NAEFS2P5/4' ] ; then
   echo naefs2p5 ${tobsv[1]} > temp
 elif [ ${ens2p5:0:10} = 'NAEFS2P5BC' ] ; then
   echo naefs2p5bc ${tobsv[1]} > temp
 else
   echo $model ${tobsv[1]} > temp
 fi
fi


exit
 

