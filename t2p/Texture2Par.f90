program Texture2Par
  use MakePar
  use fpath
  use IWFM
  use Krige
  use errorhandle
  use MFSupport
  implicit none

!-----------------------------------------------------------------------------!
!                             Texture2Par
!                         ********************
!                               V 1.0.0
!
! Converts well percent course with depth (1 or 0) data to aquifer parameters
! at specified nodes. Intended to work with input/output files of the
! Integrated Water Flow Model (IWFM) with added support for MODFLOW 2000/2005.
! Written by Leland Scantlebury and Marinko Karanovic of S.S. Papadopulos & 
! Associates, based on original method and VBA program created by Timothy J.
! Durbin.
!
! 
! Requires Input File: Texture2Par.in
!
!-----------------------------------------------------------------------------!

    integer                    :: mytpe, ioerr, ilay, ifail, ppn, atppn, ipp, &
                                  i, ierr,  twell, wpoint, zone, ppcount, &
                                  modflowflag, zoneflag, geoflag
    real                       :: a_hmin
    real, allocatable          :: ppx(:), ppy(:), ppKCMin(:), &
                                  ppdKC(:), ppAnisoC(:), ppAnisoF(:), &
                                  ppKFMin(:), ppdKF(:), ppSsC(:), ppSsF(:), &
                                  ppSyC(:), ppSyF(:), ppatx(:), ppaty(:), &
                                  ppatKCMin(:), ppatdKC(:), ppatKFMin(:), & 
                                  ppatdKF(:), ppatAnisoC(:), ppatAnisoF(:)
    integer,allocatable        :: ppZone(:), ppatZone(:)
    character(150)             :: jnk,fpath_line
    character(1000)            :: line
    logical                    :: writenodefiles
    
! Intel-Fortran specific way of writing to same command line line
!    open(6, carriagecontrol='fortran')  ! Uses removed by VB
    
    ! Write Flag
    write(*,'(/,8x,a)') '   Texture2Par v1.0.0'
    write(*,'(8x,a)')   '------------------------'

    ! Read input file
    ierr = 0
    writeNodeFiles = .false.
    write(*,'(/,2x,a)') 'Reading input file'
    open(10, file='Texture2Par.in', status='old', action='read', &
         iostat = ierr)
    call iostathandler(ierr, 'Texture2Par.in')
    
    ! Read in model type and well file
    call ReadToSymbol(10, '*')
    read(10,*) line
    read(10,*) wellfile
    read(10,*) geozonefile
    call ReadToSymbol(10, '*')
    
    ! Check if MODFLOW or IWFM
    if (index(line,'IWFM')) then
      modflowflag = 0
    else if (index(line, 'MODFLOW')) then
      modflowflag = 1
    else
      write(*,'(a)') 'Error - Invalid Model Type. Stopping.'
      write(*,'(a)') 'Valid Model Types: IWFM, MODFLOW'
      stop
    end if

    ! Different order/different files for each model
    if (modflowflag==0) then
      ! IWFM
      read(10,*) simFile
      read(10,*) fpath_line    !preSimFile
      call fpath_strip(fpath_line, prefolder, preSimFile)
      read(10,*) gwtemp
      read(10,*) zonefile
      ! MODFLOW Confining flag, set to 0 when IWFM
      ncbd = 0
    else if (modflowflag==1) then
      ! MODFLOW
      read(10,*) simFile
      read(10,*) layfiletemp
      read(10,*) mfzonename
      read(10,*) xoff
      read(10,*) yoff
      read(10,*) mfrot
    end if
    
    ! Node output setting
    call ReadToSymbol(10, '*')
    read(10,*) line
    if (trim(line)=='True') writeNodeFiles = .true.
    
    ! Variogram Settings
    call ReadToSymbol(10, '*')
    read(10,*) itype
    read(10,*) sill
    read(10,*) range
    read(10,*) a_hmin
    read(10,*) ang1
    read(10,*) nugget
    read(10,*) pckrige_nwells
    
    ! Soil Settings
    call ReadToSymbol(10, '*')
    read(10,*) KCk
    read(10,*) KFk
    read(10,*) KHp
    read(10,*) KVp
    read(10,*) Syp
    
    ! Aquifer Pilot Point section
    ! TODO: Add descriptive error when not all pilot point parameters are present
    call ReadToSymbol(10, '*')
    call FindSectionLength(10, ppn, '*')
    allocate(ppx(ppn), ppy(ppn), ppKCMin(ppn), ppdKC(ppn), ppKFMin(ppn), &
             ppdKF(ppn), ppSsC(ppn), ppSsF(ppn), ppSyC(ppn), ppSyF(ppn), &
             ppAnisoC(ppn), ppAnisoF(ppn), ppZone(ppn))
    do ipp = 1, ppn
      read(10,*) ppx(ipp), ppy(ipp), ppKCMin(ipp), ppdKC(ipp), ppKFMin(ipp), &
                 ppdKF(ipp), ppSsC(ipp), ppSsF(ipp), ppSyC(ipp),ppSyF(ipp), &
                 ppAnisoC(ipp), ppAnisoF(ipp), ppZone(ipp)
    end do
    
    ! Aquitard Pilot Point section
    !read(10,*)    !call ReadToSymbol(10, '*')
    call ReadToSymbol(10, '*')
    call FindSectionLength(10, atppn, '*')
    allocate(ppatx(atppn), ppaty(atppn), ppatKCMin(atppn), ppatdKC(atppn), &
             ppatKFMin(atppn), ppatdKF(atppn), ppatAnisoC(atppn), &
             ppatAnisoF(atppn), ppatZone(atppn))
    do ipp = 1, atppn
      read(10,*) ppatx(ipp), ppaty(ipp), ppatKCMin(ipp), ppatdKC(ipp), &
                 ppatKFMin(ipp), ppatdKF(ipp), ppatAnisoC(ipp), &
                 ppatAnisoF(ipp), ppatZone(ipp)
    end do
    close(10)

    ! MODEL SPECIFIC
    if (modflowflag == 0) then
      write(*,'(2x,a)') 'Reading IWFM Input Files'
      call readIWFM()
      call create_nodetree()
    else if (modflowflag == 1) then
      write(*,'(2x,a)') 'Reading MODFLOW Input Files'
      call read_modflow(simfile)
      call calcCellCenters_global()
      call create_nodetree()
    end if
    
    ! Pre-read Well file for file length, nwells, maxWPoints
    open(10, file = trim(wellfile), status='old', iostat = ierr)
    call iostathandler(ierr, trim(wellfile))
    read(10,*)                                    ! Header
    ioerr = 0
    wfilelen = 0
    nwell = 0
    maxWPoints = 0
    do while (ioerr == 0)
      read(10,*, iostat = ioerr) jnk, twell, wpoint
      wfilelen = wfilelen + 1
      if (twell > nwell) nwell = twell
      if (wpoint > maxWPoints) maxWPoints = wpoint
    end do
    wfilelen = wfilelen - 1
    close(10)

    ! Allocate now that many dimensions are known
    call WellAllocate()
    call NodeAllocate()
    
    ! Read Node Zone file, unless zonefile was set to NONE
    if (modflowflag == 0) then
      if (trim(zonefile)=='NONE'.or.trim(zonefile)=='none') then
        zoneflag = 0
      else
        ! External file
        zoneflag = 1
        call inputZones_IWFM()
      end if
    else if (modflowflag == 1) then
      if (trim(mfzonename)=='NONE'.or.trim(mfzonename)=='none') then
        zoneflag = 0
      else
        ! Translate MODFLOW zones file to node arrays
        zoneflag = 1
        call inputZones_modflow()
      end if
    end if
    
    ! Handle No Zones
    if (zoneflag == 0) then
      nzones   = 1
      zones    = 1
      nodezone = 1
      ppzone   = 1
      ppatzone = 1
    end if
    
    ! Read geologic units file
    if (trim(geozonefile)=='NONE'.or.trim(geozonefile)=='none') then
      ! No geologic zones
      ! perhaps a different subroutine should be used instead of dummy values?
      geoflag = 0
      ngeozones = 1
      ngeozones_lay(1:nlayers) = 1
      geozones(1:nnodes,1:nlayers) = '[NONE]'
      geonames_lay(1,1:nlayers) = '[NONE]'
    else
      geoflag = 1
      if (modflowflag == 0) then
        call readNodeGeoZones_IWFM()
      else if (modflowflag == 1) then
        call readNodeGeoZones_modflow()
      end if
    end if

    ! Write Read Summary
    write(*,'(2x,a)') 'Setup Summary:                          '
    write(*,'(4x,1a32,i)') 'Number of aquifer pilot points: ', ppn
    write(*,'(4x,1a32,i)') 'Number of aquitard pilot points ', atppn
    write(*, '(4x,1a32,i)') 'Number of Layers: ', nlayers
    if (modflowflag == 0) then
      write(*, '(4x,1a32,i)') 'Number of Nodes: ', nnodes
      write(*, '(4x,1a32,i)') 'Number of Elements: ', nelements
    else if (modflowflag == 1) then
      ! Perhaps "cells per layer"
      write(*, '(4x,1a32,i)') 'Number of Cells: ', nnodes
      write(*, '(4x,1a32,i)') 'Quasi-3D Confining Beds: ', ncbd
    end if
    !write(*,'(4x,1a32,i)') 'Well File Length: ', wfilelen
    write(*,'(4x,1a32,i)') 'Number of Wells: ', nwell
    write(*,'(4x,1a32,i)') 'Number of Pilot Point Zones: ', nzones
    write(*,'(4x,1a32,i,/)') 'Number of Geologic Zones: ', ngeozones

! Set anistropy and rotation matrix
    call setanis(a_hmin, 0.0)
    call setrot()
! MODEL SPECIFIC
! Call input wells subroutine
    if (modflowflag==0) then
      call inputwells_IWFM(geoflag)
    else if (modflowflag==1) then
      call inputwells_modflow(geoflag)
    end if

! Call Layer Elevation subroutine
    write(*,'(2x,a)') 'Calculating Layer Elevations at Wells'
    if (modflowflag == 0) then
      call layerelevation(TopElev, BotElev, wellelevtop, wellelevbot)
      ! For aquitard                        
      call layerelevation(AqTardTopElev, AqTardBotElev, &
                          wellaqtardelevtop, wellaqtardelevbot)
    end if
    ! Modflow Well elevations are assigned during well read
    
! Move co-located wells (if kriging still crashes, well locations need to be revised)
    call colocate_corrector(xwell, ywell, nwell)

! Call Layer Texture subroutine
    write(*,'(2x,a)') 'Calculating Percent Coarse at Wells      '
    outfile = 'PcWellElem.out'
    call layertexture(wellelevtop, wellelevbot, PcWellElem)
    ! If IWFM or if there are MODFLOW confining beds
    if ((modflowflag == 0).or.(ncbd > 0 )) then
      outfile = 'PcWellAqTardElem.out'
      call layertexture(wellaqtardelevtop, wellaqtardelevbot, PcWellAqtardElem)
    end if

    ! Krige from pilot points to nodes
    ! Aquifer Parameters - KCMin, DeltaKC, KFMin, DeltaKF, SsC, SsF, SyC, Aniso
    write(*,'(2x,a)') 'Kriging Aquifer Pilot Point Parameters     '
    call pp_parkrige(ppx, ppy, ppKCMin,  ppn, ppzone, nKCMin)
    call pp_parkrige(ppx, ppy, ppdKC,    ppn, ppzone, dKC)
    call pp_parkrige(ppx, ppy, ppKFMin,  ppn, ppzone, nKFMin)
    call pp_parkrige(ppx, ppy, ppdKF,    ppn, ppzone, dKF)
    call pp_parkrige(ppx, ppy, ppSsC,    ppn, ppzone, nSsC)
    call pp_parkrige(ppx, ppy, ppSsF,    ppn, ppzone, nSsF)
    call pp_parkrige(ppx, ppy, ppSyC,    ppn, ppzone, nSyC)
    call pp_parkrige(ppx, ppy, ppSyF,    ppn, ppzone, nSyF)
    call pp_parkrige(ppx, ppy, ppAnisoC, ppn, ppzone, nAnisoC)
    call pp_parkrige(ppx, ppy, ppAnisoF, ppn, ppzone, nAnisoF)
    
    if ((modflowflag == 0).or.(ncbd > 0 )) then
      if (atppn > 0) then
        ! Aquitard Parameters - KCMin, DeltaKC, KFMin, DeltaKF, Aniso
        write(*,'(2x,a)') 'Kriging Aquitard Pilot Point Parameters     '
        call pp_parkrige(ppatx, ppaty, ppatKCMin,  atppn, ppatzone, atnKCMin)
        call pp_parkrige(ppatx, ppaty, ppatdKC,    atppn, ppatzone, atdKC)
        call pp_parkrige(ppatx, ppaty, ppatKFMin,  atppn, ppatzone, atnKFMin)
        call pp_parkrige(ppatx, ppaty, ppatdKF,    atppn, ppatzone, atdKF)
        call pp_parkrige(ppatx, ppaty, ppatAnisoC, atppn, ppatzone, atnAnisoC)
        call pp_parkrige(ppatx, ppaty, ppatAnisoF, atppn, ppatzone, atnAnisoF)
      else
        write(*,'(2x,a,/)') 'No Aquitard Pilot Points - Skipping       '
        atnKCMin = -999; atdKC= -999; atnKFMin = -999; atdKF = -999
      end if
    end if
    
! Call interpolate subroutines (Well PC to Node Aquifer parameter)
    write(*,'(2x,a)') 'Calculating Node Aquifer Parameters           '
    call interpolate()
    if ((modflowflag == 0).or.(ncbd > 0 )) then
      call interpolate_aqtard()
    end if
    
! Write Output files, routines called depend on model type
    if (modflowflag == 0) then
      if (writeNodeFiles==.true.) then
        ! Only calculate if writing the file
        call calc_interbed_thickness()
        write(*,'(2x,a)') 'Writing Node Parameter Files'
        call writeWellFile_IWFM(PcWellElem,       't2p_WellPC.out')
        call writeWellFile_IWFM(PcWellAqtardElem, 't2p_WellAqTardPC.out')
        call writeNodeFile(PcNode, 't2p_NodePC.out')
        call writeNodeFile(KhB,    't2p_KhB.out'   )
        call writeNodeFile(KvB,    't2p_KvB.out'   )
        call writeNodeFile(SsB,    't2p_SsB.out'   )
        call writeNodeFile(SyB,    't2p_SyB.out'   )
        call writeNodeFile(interbed_thick,'t2p_InterbedThickness.out')
        call writeNodeFile(PcNode_aqtard, 't2p_NodeAqTardPC.out')
        call writeNodeFile(KvB_aqtard,    't2p_AqTardKvB.out'   )
      else
        write(*,'(2x,a)') 'WriteNodeFiles set to FALSE - no parameter files written'
      end if
        call writeIWFMgwfile()
        
    else if (modflowflag == 1) then
      if (writeNodeFiles==.true.) then
        ! Only calculate if writing the file
        call calc_interbed_thickness()
        write(*,'(2x,a)') 'Writing Cell Parameter Files'
        call writeWellFile_modflow(PcWellElem,       't2p_WellPC.out')
        call writeCellFile(PcNode, 't2p_NodePC.out')
        call writeCellFile(KhB,    't2p_KhB.out'   )
        call writeCellFile(KvB,    't2p_KvB.out'   )
        call writeCellFile(SsB,    't2p_SSB.out'   )
        call writeCellFile(SyB,    't2p_SyB.out'   )
        call writeCellFile(interbed_thick,'t2p_InterbedThickness.out')
        if (ncbd > 0 ) then
          call writeWellFile_modflow(PcWellAqtardElem, 't2p_WellAqTardPC.out')
          call writeCellFile(PcNode_aqtard, 't2p_AqTardNodePC.out')
          call writeCellFile(KvB_aqtard,    't2p_AqTardKvB.out'   )
        end if
      else
        write(*,'(2x,a)') 'WriteCellFiles set to FALSE - no parameter files written'
      end if
      call readwriteFlowPackage()
      call clear_modflow_memory()
    end if

    write(*,'(/,2x,a)') 'Program finished!'

  end program Texture2Par
!-----------------------------------------------------------------------------!