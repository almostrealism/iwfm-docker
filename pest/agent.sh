#!/bin/sh
PATH=$PATH:/build/iwfm

/init.sh

cd /
echo "Running pest..."
/pestbin/pestpp-glm C2VSimFG_01.pst /h $PEST_HOST