#!/bin/bash

i=0

generate () {
  if [ $1 -lt 10 ] ; then
    echo 0000${1}dir
  else
    if [ $1 -lt 100 ] ; then
      echo 000${1}dir
    else
      if [ $1 -lt 1000 ] ; then
        echo 00${1}dir
      else
        if [ $1 -lt 10000 ] ; then
          echo 0${1}dir
        else
          echo ${1}dir
        fi
      fi
    fi
  fi
}

rm joblist
if [ -d data ] ; then
  rm -rf data
fi
mkdir data
while [ $i -lt 50 ] ; do
  let i=i+1
  dir=`generate $i`
  echo "data/$dir job.sh" >> joblist
  cp -pr dir data/$dir
done
