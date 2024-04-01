#!/bin/zsh

scala-cli package -o executable --native -f bf.scala
echo "start"
time ./executable "$(< testcase.txt)" ""

