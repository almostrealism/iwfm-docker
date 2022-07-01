 _____          _                  ____    ___           
/__   \_____  _| |_ _   _ _ __ ___|___ \  / _ \__ _ _ __ 
  / /\/ _ \ \/ / __| | | | '__/ _ \ __) |/ /_)/ _` | '__|
 / / |  __/>  <| |_| |_| | | |  __// __// ___/ (_| | |   
 \/   \___/_/\_\\__|\__,_|_|  \___|_____\/    \__,_|_|   

Readme Folder: .\src\mfUSGLib
Readme Date:   6/1/2019

This contents of this folder, and the folders inside, are used to create
the MODFLOW-USG Fortran Library that Texture2Par uses to read MODFLOW
files. Below is a brief summary of the files and folders in this folder.

.\Jan_23_2018
  * Contains the source code of the January 23, 2018 source code for MODFLOW-USG.
.\Release
  * 32-bit compiled library.
.\x64
  * 64-bit compiled library (within the internal "Release" folder).
.\mf_t2p.f90
  * Non-MODFLOW Fortran file that calls the reading routines for Texture2Par.
.\mfUSGLib.sln
  * Visual Studio solution file for the library.
.\mfUSGLib.vfproj
  * Visual Studio project file for the library. Can be opened as a text file to see
    compilation settings.