#!/bin/bash

# calculate climatologies over periods 1989-2009 and 2080-2100

tmpmodels=`more allmodels.txt`
allmodels=`echo $tmpmodels |sed -e "s/\:\:\:\:\:\:\:\:\:\:\:\:\:\: allmodels\.txt \:\:\:\:\:\:\:\:\:\:\:\:\:\://g"`
echo $allmodels

vars[0]='ps'
vars[1]='psl'
vars[2]='uas'
vars[3]='vas'
vars[4]='hurs'
vars[5]='tas'
vars[6]='ts'

if [ ! -d CLIMO_ATM ]; then
  mkdir CLIMO_ATM
fi

for v in ${vars[@]}
do
  echo $v
  for m in $allmodels
  do
  echo $v  $m

        files=`ls ${m}/${v}_*.nc`
	n=0
        unset filename
	unset histstart
	unset histend
	unset rcpstart
	unset rcpend
	for f in $files
	do
		filename[n]=$f
                echo $f
		modeln=`echo $f | awk -F "_" '{print $3}'`
    		scen=`echo $f | awk -F "_" '{print $4}'`
    		ens=`echo $f | awk -F "_" '{print $5}'`
		years=`echo $f | awk -F "_" '{print $6}'`
		years=`echo $years | awk -F "." '{print $1}'`
		yearstart=`echo $years | awk -F "-" '{print $1}'`
		yearend=`echo $years | awk -F "-" '{print $2}'`
		monthstart=${yearstart:4:2}
		monthend=${yearend:4:2}
		yearstart=${yearstart:0:4}
		yearend=${yearend:0:4}
                echo "${modeln}  ${scen}  ${ens}  ${monthstart}/${yearstart}  ${monthend}/${yearend}"
		if [[ "$monthstart" -eq "01" ]] ;then
			starth=1989
			startr=2080
		else
			starth=1988
			startr=2079
		fi
	
		echo "starth=${starth} startr=${startr}"
		if [[ "$yearstart" -le "$starth" && "$yearend" -ge "$starth" ]] ;then
	 	  histstart=$n
		  file1989=$f
		  year1989=$yearstart
		  month1989=$monthstart
		fi
                if [[ "$yearstart" -le "startr" && "$yearend" -ge "startr" ]] ;then
                  rcpstart=$n
		  file2080=$f
                  year2080=$yearstart
                  month2080=$monthstart
                fi
                if [[ "$yearstart" -le 2009 && "$yearend" -ge 2009 ]] ;then
                  histend=$n
		  file2009=$f
		  year2009=$yearend
                  month2009=$monthend
                fi
                if [[ "$yearstart" -le 2099 && "$yearend" -ge 2099 ]] ;then
                  rcpend=$n
                  file2100=$f
                  year2100=$yearend
                  month2100=$monthend
                fi
                if [[ "$yearstart" -le 2100 && "$yearend" -ge 2100 ]] ;then
                  rcpend=$n
		  file2100=$f
		  year2100=$yearend
                  month2100=$monthend
                fi
		n=$((n+1))
	done
	if [ ! $rcpend ]; then
		rcpend=$rcpstart
	fi
        if [ ! $histend ]; then
                histend=$histstart
        fi

	if [ ! -f CLIMO_ATM/$v\_$m\_climo_1989_2009.nc ];then

	echo "Historical:"
        concatmodels=''
	blank='  '
	for i in $(seq $histstart $histend)
  	do
		#if [ "${filename[$i]/rcp85}" = "${filename[$i]}" ] ; then
  		#	concatmodels=$concatmodels$blank$dir_his/${filename[$i]}
		#else
  		#	concatmodels=$concatmodels$blank$dir_rcp/${filename[$i]}	
		#fi

                concatmodels=$concatmodels$blank${filename[$i]}

 	done

	echo ncrcat $concatmodels  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc
 	ncrcat $concatmodels  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc
	if [[ "$month1989" -eq "01" && "$month2009" -eq "12" ]] ;then
		a=$(( (1989 - $year1989)*12 ))
                b=$(( ($year2009-2009)*12 ))
                c=$(( ($year2009 - $year1989)*12 ))
		echo 1111  $a $b $c $year2009
		echo ncks -v time -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc $v\_$m\_TIME_CONCAT_1989.01_2009.12.nc
        	ncks -v time -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CLIMO_ATM/$v\_$m\_TIME_CONCAT_1989.01_2008.12.nc
        	echo ncks -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CONCAT_1989.01_2008.12.nc
        	ncks -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CONCAT_1989.01_2008.12.nc
	else
		a=$(( (1988 - $year1989)*12 ))
                b=$(( ($year2009-2009)*12 ))
                c=$(( ($year2009 - $year1989)*12 ))
                echo 2222 $a $b $c  $year1989 $year2009
        	echo ncks -v time -d time,$(( a+1 )),$(( $c-$b-12 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc $v\_$m\_TIME_CONCAT_1989.01_2009.12.nc
        	ncks -v time -d time,$(( a+1 )),$(( $c-$b-12 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CLIMO_ATM/$v\_$m\_TIME_CONCAT_1989.01_2008.12.nc
        	echo ncks -d time,$(( a+1 )),$(( $c-$b-12 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CONCAT_1989.01_2008.12.nc
        	ncks -d time,$(( a+1 )),$(( $c-$b-12 ))  CONCAT_$year1989.$month1989\_$year2009.$month2009.nc CONCAT_1989.01_2008.12.nc
        fi
	
	for month in 1 2 3 4 5 6 7 8 9 10 11 12
    	do
            if [ $month -lt 10 ]
            then
                    pre='0'
            else
                    pre=''
            fi
            echo ncra -F -d time,$month,,12  CONCAT_1989.01_2008.12.nc tmp_clim_$pre$month.nc
            ncra -F -d time,$month,,12 CONCAT_1989.01_2008.12.nc tmp_clim_$pre$month.nc
    	done
	echo ncrcat tmp_clim*  $v\_$m\_climo_1989_2009.nc
	ncrcat tmp_clim*  CLIMO_ATM/$v\_$m\_climo_1989_2009.nc
	rm tmp_clim_*
	rm CONCAT*
	fi

	if [ ! -f CLIMO_ATM/$v\_$m\_climo_2080_2100.nc ];then
 	echo "Future:"

	concatmodels=''
        blank='  '
	for i in $(seq $rcpstart $rcpend)
          do
                concatmodels=$concatmodels$blank${filename[$i]}
          done
        echo ncrcat $concatmodels  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc
	ncrcat $concatmodels  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc

        if [[ "$month2080" -eq "01" && "$month2100" -eq "12" ]] ;then
                a=$(( (2080 - $year2080)*12 ))
                b=$(( ($year2100-2100)*12 ))
                c=$(( ($year2100 - $year2080)*12 ))
                echo $a $b $c $year2100
		echo ncks -v time -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc $v\_$m\_TIME_CONCAT_2080.01_2100.12.nc
        	ncks -v time -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CLIMO_ATM/$v\_$m\_TIME_CONCAT_2080.01_2099.12.nc
        	echo ncks -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CONCAT_2080.01_2099.12.nc
        	ncks      -d time,$(( a-0 )),$(( $c-$b-1 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CONCAT_2080.01_2099.12.nc
	else
                a=$(( (2079 - $year2080)*12 ))
                b=$(( ($year2100-2100)*12 ))
                c=$(( ($year2100 - $year2080)*12 ))
                echo $a $b $c $year2100
                echo ncks -v time -d time,$(( a+1 )),$(( $c-$b-13 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc $v\_$m\_TIME_CONCAT_2080.01_2100.12.nc
                ncks -v time -d time,$(( a+1 )),$(( $c-$b-13 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CLIMO_ATM/$v\_$m\_TIME_CONCAT_2080.01_2099.12.nc
                echo ncks -d time,$(( a+1 )),$(( $c-$b-13 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CONCAT_2080.01_2099.12.nc
                ncks      -d time,$(( a+1 )),$(( $c-$b-13 ))  CONCAT_$year2080.$month2080\_$year2100.$month2100.nc CONCAT_2080.01_2099.12.nc
        fi
        for month in 1 2 3 4 5 6 7 8 9 10 11 12
        do
            if [ $month -lt 10 ]
            then
                    pre='0'
            else
                    pre=''
            fi
            echo ncra -F -d time,$month,,12  CONCAT_2080.01_2099.12.nc tmp_clim_$pre$month.nc
            ncra -F -d time,$month,,12 CONCAT_2080.01_2099.12.nc tmp_clim_$pre$month.nc
        done
        echo ncrcat tmp_clim*  $v\_$m\_climo_2080_2100.nc
        ncrcat tmp_clim*  CLIMO_ATM/$v\_$m\_climo_2080_2100.nc
        rm tmp_clim_*
 	rm CONCAT_2080.01_2099.12.nc
	fi
        rm CONCAT*
	
	filename=''
  done
done



