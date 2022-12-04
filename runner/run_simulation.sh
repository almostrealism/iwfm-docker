#!/bin/sh

cd ${WORKING_PATH}/Simulation
cp Groundwater/C2VSimFG_Groundwater1974.dat Groundwater\\C2VSimFG_Groundwater1974_Orig.dat

cp Streams/C2VSimFG_BypassSpec.dat Streams\\C2VSimFG_BypassSpec.dat
cp Streams/C2VSimFG_Diversions.dat Streams\\C2VSimFG_Diversions.dat
cp Streams/C2VSimFG_DiversionSpec.dat Streams\\C2VSimFG_DiversionSpec.dat
cp Streams/C2VSimFG_StreamInflow.dat Streams\\C2VSimFG_StreamInflow.dat
cp Streams/C2VSimFG_Streams.dat Streams\\C2VSimFG_Streams.dat
cp Groundwater/C2VSimFG_BC.dat Groundwater\\C2VSimFG_BC.dat
cp Groundwater/C2VSimFG_ConstrainedHeadBC.dat Groundwater\\C2VSimFG_ConstrainedHeadBC.dat
cp Groundwater/C2VSimFG_ElemPump.dat Groundwater\\C2VSimFG_ElemPump.dat
# cp Groundwater/C2VSimFG_Groundwater1974.dat Groundwater\\C2VSimFG_Groundwater1974.dat
cp Groundwater/C2VSimFG_Pumping.dat Groundwater\\C2VSimFG_Pumping.dat
cp Groundwater/C2VSimFG_PumpRates.dat Groundwater\\C2VSimFG_PumpRates.dat
cp Groundwater/C2VSimFG_Subsidence.dat Groundwater\\C2VSimFG_Subsidence.dat
cp Groundwater/C2VSimFG_TileDrain.dat Groundwater\\C2VSimFG_TileDrain.dat
cp Groundwater/C2VSimFG_TimeSeriesBC.dat Groundwater\\C2VSimFG_TimeSeriesBC.dat
cp Groundwater/C2VSimFG_WellSpec.dat Groundwater\\C2VSimFG_WellSpec.dat
cp RootZone/C2VSimFG_IrrPeriod.dat RootZone\\C2VSimFG_IrrPeriod.dat
cp RootZone/C2VSimFG_NativeVeg_Area.dat RootZone\\C2VSimFG_NativeVeg_Area.dat
cp RootZone/C2VSimFG_NativeVeg.dat RootZone\\C2VSimFG_NativeVeg.dat
cp RootZone/C2VSimFG_NonPondedCrop_Area.dat RootZone\\C2VSimFG_NonPondedCrop_Area.dat
cp RootZone/C2VSimFG_NonPondedCrop.dat RootZone\\C2VSimFG_NonPondedCrop.dat
cp RootZone/C2VSimFG_NonPondedCrop_MinSoilMoisture.dat RootZone\\C2VSimFG_NonPondedCrop_MinSoilMoisture.dat
cp RootZone/C2VSimFG_NonPondedCrop_RootDepthFracs.dat RootZone\\C2VSimFG_NonPondedCrop_RootDepthFracs.dat
cp RootZone/C2VSimFG_NonPondedCrop_TargetSM.dat RootZone\\C2VSimFG_NonPondedCrop_TargetSM.dat
cp RootZone/C2VSimFG_PondedCrop_Area.dat RootZone\\C2VSimFG_PondedCrop_Area.dat
cp RootZone/C2VSimFG_PondedCrop.dat RootZone\\C2VSimFG_PondedCrop.dat
cp RootZone/C2VSimFG_PondedCrop_Depth.dat RootZone\\C2VSimFG_PondedCrop_Depth.dat
cp RootZone/C2VSimFG_PondedCrop_Operations.dat RootZone\\C2VSimFG_PondedCrop_Operations.dat
cp RootZone/C2VSimFG_ReturnFlowFrac.dat RootZone\\C2VSimFG_ReturnFlowFrac.dat
cp RootZone/C2VSimFG_ReuseFrac.dat RootZone\\C2VSimFG_ReuseFrac.dat
cp RootZone/C2VSimFG_RootZone.dat RootZone\\C2VSimFG_RootZone.dat
cp RootZone/C2VSimFG_Urban_Area.dat RootZone\\C2VSimFG_Urban_Area.dat
cp RootZone/C2VSimFG_Urban.dat RootZone\\C2VSimFG_Urban.dat
cp RootZone/C2VSimFG_Urban_PerCapWaterUse.dat RootZone\\C2VSimFG_Urban_PerCapWaterUse.dat
cp RootZone/C2VSimFG_Urban_Population.dat RootZone\\C2VSimFG_Urban_Population.dat
cp RootZone/C2VSimFG_Urban_WaterUseSpecs.dat RootZone\\C2VSimFG_Urban_WaterUseSpecs.dat

/build/t2p/Texture2Par Texture2Par.in

dos2unix *.dat

Simulation C2VSimFG.in

cp ../Preprocessor/C2VSimFG_Preprocessor.in ..\\Preprocessor\\C2VSimFG_Preprocessor.in
cp ../Preprocessor/C2VSimFG_Elements.dat ..\\Preprocessor\\C2VSimFG_Elements.dat
cp ../Preprocessor/C2VSimFG_Nodes.dat ..\\Preprocessor\\C2VSimFG_Nodes.dat
cp ../Preprocessor/C2VSimFG_Stratigraphy.dat ..\\Preprocessor\\C2VSimFG_Stratigraphy.dat
cp ../Preprocessor/C2VSimFG_StreamsSpec.dat ..\\Preprocessor\\C2VSimFG_StreamsSpec.dat

/build/iwfm2obs/iwfm2obs_2017 <iwfm2obs_2015.in
/build/mlt/MultiLayerTarget MultiLayerTarget.in
/build/srs/SquareRootStream

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