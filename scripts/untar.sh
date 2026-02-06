#!/bin/bash

for directory in `ls -1 -d *`; do
   echo ${directory}
   if [ -d  ${directory} ]; then
      echo change to ${directory}
      cd ${directory}
      tar -xvf *.tar
      rm *.tar
      cd ..
   fi
done
