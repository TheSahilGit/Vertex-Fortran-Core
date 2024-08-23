#!/usr/bin/tcsh

set MY_DIR=/home/sahil/Work/Vertex_Model/NewCodes24/

 if (-e param_Space.dat) then
  echo "param_Space.dat exists: move"
  mv  param_Space.dat old_param_Space.dat
#  rm -f  
 endif

cd Batch0


#sh clear.sh
#sh compile.sh

# Need to set variables now. i.e  gamm, beta  etc. 
# These it will take from the 'para1.dat' file. 

set gamm0 = `cat para1.dat | grep gamm| awk '{print $1}'`
set beta0 = `cat para1.dat | grep beta| awk '{print $1}'`


echo "gamm0 = $gamm0"
echo "beta0 = $beta0"

#set gammIn = `printf "%.f" $gamm0`
#set gammIn = `printf "%.f" $beta0`

# This is to convert the fortran or matlab way to unix understandable way.
# i.e. 0.0d0 type data it can't understand. So we convert. 

set gammIn = `echo $gamm0 | sed 's/d0//'`
set betaIn = `echo $beta0 | sed 's/d0//'`


echo "gammIn = $gammIn"
echo "betaIn = $betaIn"





cd ../

#pwd 

#set gamm = `echo "$gammIn+0.1" | bc -l`
#set beta = `echo "$gammIn*0.0" | bc -l`

#echo "gamm= $gamm"
#echo "beta= $beta"

set jj = 1
while ($jj <= 11)

  set beta = `echo "$betaIn+($jj-1)*0.018" | bc -l`

  set kk = 1
  while ($kk <= 11)

    set gamm = `echo "$gammIn+($kk-1)*0.18" | bc -l`


    cp -r Batch0/ Batch$jj.$kk\_gamm\_$gamm\_beta\_$beta/
    cd Batch$jj.$kk\_gamm\_$gamm\_beta\_$beta

    pwd

    sed -i "/gamm/s/$gamm0/$gamm/g" para1.dat
    sed -i "/beta/s/$beta0/$beta/g" para1.dat

    sh clear.sh
    sh compile.sh
    ./vertexmain.exe


    cat ./final_data.dat >> ../param_Space.dat

    cd ../


    echo "Gamma= $gamm & Beta = $beta"




    @ kk++

  end


  @ jj++

  echo $kk
  echo $jj
end
