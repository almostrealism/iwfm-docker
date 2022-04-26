#!/bin/sh
PATH=$PATH:/build/iwfm

cd Preprocessor
dos2unix *.dat
awk -v RS='\r\n' 'END{print NR}' C2VSimFG_StreamsSpec.dat
PreProcessor C2VSimFG_Preprocessor.in
ls -la