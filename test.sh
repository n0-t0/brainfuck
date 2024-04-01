#!/bin/zsh

# Scalaファイルからネイティブ実行可能ファイルを作成
scala-cli package -o executable --native -f bf.scala

echo "start"
# `testcase.txt`の内容を引数としてexecutableを実行
# 空文字列を第二引数として渡す
time ./executable "$(< testcase.txt)" ""

