#!/bin/sh
cd Simulation

cp CalcTypeHyd_All_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub1Sub2_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub3Sub4_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub5_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub6Sub7_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub19Sub20Sub21_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub14Sub15_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub16Sub17Sub18_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub11Sub12Sub13_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub10_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_Sub8Sub9_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd
cp CalcTypeHyd_CC_sim.in CalcTypeHyd.in
/build/cth/CalcTypeHyd 

ls -la
