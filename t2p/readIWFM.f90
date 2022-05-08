MODULE IWFM
  use MakePar
  use errorhandle
  implicit none

!-----------------------------------------------------------------------------!
! IWFM Module for Texture2Par
! Combination of subroutines written by SSP&A and John Doherty
! used for the reading/writing of IWFM files
!-----------------------------------------------------------------------------!
  
  SAVE
  integer, parameter                  :: NUM_WORD_DIM=100
  integer, dimension(NUM_WORD_DIM)    :: left_word, right_word
  character(300)                      :: cline
  character(200)                      :: nodefile, elementfile, prefolder, &
                                         preSimFile, simFile, stratfile, &
                                         gwfile, gwtemp
  
  CONTAINS
  
!-----------------------------------------------------------------------------!
  
subroutine readIWFM()
  use fpath
  implicit none
  
  integer                  :: i, k, ifail, ioerr, ierr, ipos, nreg, intd, &
                              idum
  integer                  :: ls(100), rs(100)
  real                     :: fact
  character(150)           :: jnk,currfile
  
  ! Read pre-processor file
!    write(*,'(2x,a)') 'Reading IWFM pre-processor file...'
    !currfile = adjustl(trim(prefolder)) // adjustl(trim(preSimFile))
    call fpath_join(prefolder, presimfile, currfile)
    open(12, file=trim(currfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, currfile)
    do i = 1,4
      call readOneLine(preSimFile, 12, cline)
    end do
    read(12, *, iostat=ierr) elementfile
    read(12, *, iostat=ierr) nodefile
    read(12, *, iostat=ierr) stratfile
    close(12)
    
! Read node file
!    write(*,'(2x,a)') 'Reading IWFM node file...'
    !currfile = adjustl(trim(prefolder)) // adjustl(trim(nodefile))
    call fpath_join(prefolder, nodefile, currfile)
    open(12, file=trim(currfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, currfile)
    call readOneLine(nodefile,12,cline)
    call multisplit(ifail,1,ls,rs,cline)
    call ifailhandler(ifail, nodefile, cline)
    call intread(ifail,cline(ls(1):rs(1)),nnodes)
    call ifailhandler(ifail, nodefile, cline)
    
    ! TODO : ALLOCATE NODE ROUTINE
    
    allocate(nodex(nnodes),nodey(nnodes), &
              nodeelem(nnodes, max_node_elem_members), &
              nnodeelem(nnodes),nodexy(nnodes,2))
    read(12,*) fact
    call readOneLine(elementfile,12,cline)
    BACKSPACE(12)
      
    do i=1,nnodes
      read(12,*)intd,nodex(i),nodey(i)
      nodex(i) = nodex(i) * fact
      nodey(i) = nodey(i) * fact
      ! Initialize node-element membership array to 0
      nnodeelem(i) = 0
    end do
    close(12)
    
! Read elements file
!    write(*,'(2x,a)') 'Reading IWFM elements file...'
    !currfile = adjustl(trim(prefolder)) // adjustl(trim(elementfile))
    call fpath_join(prefolder, elementfile, currfile)
    open(12, file=trim(currfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, currfile)
    do
      call readOneLine(elementfile,12,cline)
      !cline= adjustl(trim(cline))
      IPOS=SCAN(cline,'NE')
      if (IPOS.GT.0) THEN
        EXIT
      end if
    end do
     
    call multisplit(ierr,1,ls,rs,cline)
    call intread(ierr,cline(ls(1):rs(1)),nelements)
    
    ! TODO : ALLOCATE ELEMENTS ROUTINE
    
    allocate(elements(nelements,4))
     
    call readOneLine(elementfile,12,cline)       
    call multisplit(ifail,1,ls,rs,cline)
    call ifailhandler(ifail, elementfile, cline)
    call intread(ifail,cline(ls(1):rs(1)),nreg)
    call ifailhandler(ifail, elementfile, cline)
    call readOneLine(elementfile,12,cline)         
     
    do i=1,nreg-1
      call readOneLine(elementfile,12,cline)
    end do

    call readOneLine(elementfile,12,cline)
    BACKSPACE(12)     
    do i=1,nelements
      READ(12,*)intd,(elements(intd,k),k=1,4)
      ! Add to node-element relationship array
      ! So we can identify by node what element we're in
      call add_elem2nodeelem(intd)
    end do
    close(12)
     
! Read Stratigraphy file
!    write(*,'(2x,a)') 'Reading IWFM stratigraphy file'
    !currfile = adjustl(trim(prefolder)) // adjustl(trim(stratfile))
    call fpath_join(prefolder, stratfile, currfile)
    open(12, file=trim(currfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, currfile)
    call readOneLine(nodefile,12,cline)
    call multisplit(ifail,1,ls,rs,cline)
    call ifailhandler(ifail, stratfile, cline)
    call intread(ifail,cline(ls(1):rs(1)),nlayers)
    call ifailhandler(ifail, stratfile, cline)
    allocate(Elevation    (nnodes),         &
             TopElev      (nnodes,nlayers), &
             BotElev      (nnodes,nlayers), &
             Thick        (nnodes,nlayers), &
             AqTardThick  (nnodes,nlayers), &
             AqTardTopElev(nnodes,nlayers), &
             AqTardBotElev(nnodes,nlayers)  )
    topelev = 0.0
    botelev = 0.0
    AqTardTopElev = 0.0
    AqTardBotElev = 0.0
    !call readOneLine(nodefile,12,cline)
    read(12, *) fact
    call readOneLine(nodefile,12,cline)
    BACKSPACE(12)
    do i=1,nnodes
      call readOneLine(nodefile,12,cline)
      read(cline,*) idum,Elevation(i),((AqTardThick(i,k),Thick(i,k)),k=1,nlayers)
      ! Adjust for Factor
      Elevation(i) = Elevation(i) * fact
      AqTardThick(i,1:nlayers) = AqTardThick(i,1:nlayers) * fact
      Thick(i,1:nlayers)       = Thick(i,1:nlayers)       * fact
      do k = 1,nlayers
        ! Aquitard layer is above aquifer layer
        if (k.eq.1) then
          AqTardTopElev(i,k) = Elevation(i)
          AqTardBotElev(i,k) = AqTardTopElev(i,k)-AqTardThick(i,k)
          TopElev(i,k) = Elevation(i)-AqTardThick(i,k)
          BotElev(i,k) = TopElev(i,k)-Thick(i,k)
        else
          AqTardTopElev(i,k) = BotElev(i,k-1)
          AqTardBotElev(i,k) = AqTardTopElev(i,k) - AqTardThick(i,k)
          TopElev(i,k) = BotElev(i,k-1)-AqTardThick(i,k)
          BotElev(i,k) = TopElev(i,k)-Thick(i,k)
        endif
      end do
    end do  
    close(12)

! Read simulation file
!    write(*,'(2x,a)') 'Reading simulation file'
    currfile = adjustl(trim(simfile))
    open(12, file=trim(currfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, currfile)
    do i= 1,4
      call readOneLine(simfile,12,cline)
    end do
    read(12,*,iostat=ierr) GWfile
    close(12)
    
end subroutine readIWFM

!-----------------------------------------------------------------------------!
SUBROUTINE inputwells_IWFM(readzones)
    use MakePar
    use kd_tree
    use errorhandle
    implicit none

!-----------------------------------------------------------------------------!
! Inputs well information.
! Uses NodeTree to locate closest nodes and pass this information to
! subroutine that locates cell/element of well.
!
! Well GeoZones added 7/1/2019 -LS
!   Argument "readzones" is a flag for whether GeoZones exist (1 = yes, 0 = no)
!-----------------------------------------------------------------------------!
    
    integer, parameter       :: nnear=5
    integer                  :: i, iwell, izone, igeo, ilay, tpc, readzones, &
                                currwellelem, node, nwellgeozones, ierr
    integer                  :: closenodes(nnear)
    real                     :: depth, tx, ty, tz, td
    real                     :: nodedists(nnear), queryxy(2)
    character(30)            :: wellname, geobylayer(nlayers), &
                                wellgeonames(max_zones)
    logical                  :: newzone
    
    ! Initialize (to prevent issues where compiler does not initilize to 0)
    nwellgeozones = 0

    ! Read well file
    write(*,'(2x,2a)') 'Reading Well File ', trim(wellfile)
    open(10, file =trim(wellfile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, trim(wellfile))
    read(10,*)                        ! Header
    
    if (readzones == 0) then
      ! No GeoZones
      nwellgeozones = 1
      wellgeozones(1:nwell,1:nlayers) = '[NONE]'
      do i=1, wfilelen
        read(10,*) wellname, iwell, izone, tpc, tx, ty, tz, td
        ! Well "zone" is discrete depth interval counter and should not be 
        ! confused with other uses of zone (e.g. pilot points, geologic)
        PcWellZone(iwell, izone) = tpc
        numZone(iwell) = izone
        if (izone == 1) then
          Xwell(iwell) = tx
          Ywell(iwell) = ty
          Zland(iwell) = tz
          ! Use node tree to find closest node
          queryxy(1) = tx
          queryxy(2) = ty
          call n_nearest_to(nodetree, queryxy, nnear, closenodes, nodedists)
          wellelem(iwell) = get_well_element(tx, ty, iwell, closenodes, nnear)
        end if
        Zwell(iwell, izone) = tz - td
      end do
    else
      ! GeoZones
      do i=1, wfilelen
        read(10,*) wellname, iwell, izone, tpc, tx, ty, tz, td, geobylayer
        ! Well "zone" is discrete depth interval counter and should not be 
        ! confused with other uses of zone (e.g. pilot points, geologic)
        PcWellZone(iwell, izone) = tpc
        numZone(iwell) = izone
        if (izone == 1) then
          Xwell(iwell) = tx
          Ywell(iwell) = ty
          Zland(iwell) = tz
        
          ! Well geologic zones
          wellgeozones(iwell,:) = geobylayer(:)
          ! Counts geozones for reporting and comparing
          do ilay=1, nlayers
            call strtracker(nwellgeozones, geobylayer(ilay), &
                  wellgeonames, newzone)
          end do
        
          ! Use node tree to find closest node
          queryxy(1) = tx
          queryxy(2) = ty
          call n_nearest_to(nodetree, queryxy, nnear, closenodes, nodedists)
          wellelem(iwell) = get_well_element(tx, ty, iwell, closenodes, nnear)
        
        end if
        Zwell(iwell, izone) = tz - td
      end do
    end if
    close(10)
    
    ! Report if mismatch in model vs well geo zone count
    if (nwellgeozones > ngeozones) then
      ! Just a warning - the extra wells will not be used
      write(*,'(4x,a)') 'Warning - more well geologic zones than node geologic zones'
      write(*,'(4x,i3,a,i3)') nwellgeozones, ' vs ', ngeozones
    else if (nwellgeozones < ngeozones) then
      ! All node geologic zones MUST have wells - otherwise can't calc PC
      write(*,'(4x,a)') 'Error - more node geologic zones than well geologic zones'
      write(*,'(4x,i3,a,i3)') ngeozones, ' vs ', nwellgeozones
      stop
    end if
    
    !write(*,'(4x,a)') 'Well file read successfully.'

end subroutine inputwells_IWFM
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine readNodeGeoZones_IWFM()
  use MakePar
  implicit none
!-----------------------------------------------------------------------------!
! Added 7/1/2019 to facilitate limiting percent coarse interpolation to wells
! and nodes of the same assigned geologic unit. File is assumed to have
! a heading and the node number followed by the geologic zones of each
! corresponding layer.
!
! Requires: strtracker (see Tools.f90) to increment zone arrays & counters
!
! Author: Leland Scantlebury of S.S. Papadopulos & Associates
!-----------------------------------------------------------------------------!
  
  integer                  :: node, i, ilay, izone, ierr
  character(30)            :: geobylayer(nlayers)
  logical                  :: newzone, newzone_lay
  
  ! Initialize
  ngeozones = 0
  ngeozones_lay = 0
  
  write(*,'(2x,2a)') 'Reading Model Geologic Zone File ', trim(geozonefile)
  open(10, file =trim(geozonefile), status='old', action='read', iostat=ierr)
  call iostathandler(ierr, trim(geozonefile))
  read(10, *) ! Header
  
  do i=1, nnodes
    read(10, *) node, geobylayer
    geozones(node,:) = geobylayer
    ! Track geozones for looping, reporting and comparing
    do ilay=1, nlayers
      ! Is this zone new to the layer?
      call strtracker(ngeozones_lay(ilay), geobylayer(ilay), &
                      geonames_lay(:,ilay), newzone_lay)
      ! Handle if new zone for layer
      if (newzone_lay) then
        ! Is it a zone we haven't seen in any layer?
        call strtracker(ngeozones, geobylayer(ilay), &
                        geonames, newzone)
      end if
    end do
  end do
  
  close(10)

end subroutine readNodeGeoZones_IWFM
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine inputZones_IWFM()
    use MakePar
    use errorhandle
    implicit none
!-----------------------------------------------------------------------------!
! Reads zone file consisting of node number and corresponding pilot point zone
!-----------------------------------------------------------------------------!    
    integer                :: i, node, zone, ierr
    logical                :: newzone
    
    ! Set
    nzones = 0
    
    open(10, file=trim(zonefile), status='old', action='read', iostat=ierr)
    call iostathandler(ierr, trim(zonefile))
    read(10,*)   ! Header
    do while (ierr == 0)
      read(10,*,iostat=ierr) node, zone
      nodezone(node) = zone
      ! Is this a new zone?
      newzone = .true.
      do i=1, nzones
        if (zone == zones(i)) newzone = .false.
      end do
      if (newzone) then
        nzones = nzones + 1
        ! Handle parameter exceedence if necessary
        if (nzones > max_zones) then
          call parexceed(max_zones, 'max_zones', &
                          'Failed during list creation of node zones', &
                          finalint=zone)
        end if
        zones(nzones) = zone
      end if
    end do
    
    close(10)

end subroutine inputZones_IWFM
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine writeWellFile_IWFM(v, filename)
  use MakePar
  implicit none

  real,intent(in)          :: v(nwell,nlayers)
  character(*),intent(in)  :: filename
  integer                  :: iwell, ilay
  character(60)            :: fmt(2)
  
  write(*,'(4x,a,a35)') 'Writing:', filename

  write(fmt(1),'("(2a10,",i5,"i14)")') nlayers
  write(fmt(2),'("(2i10,",i5,"es14.5)")') nlayers

  open(20, file=trim(filename), status='replace', action='write', buffered='YES')
  ! Header
  write(20,fmt(1)) 'Well', 'Element', (ilay, ilay=1, nlayers)
  ! Values
  do iwell = 1, nwell
    write(20,fmt(2)) iwell, wellelem(iwell), (v(iwell,ilay), ilay=1, nlayers)
  end do
  
  close(20)

end subroutine writeWellFile_IWFM
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine writeNodeFile(v, filename)
  use MakePar
  implicit none

  real,intent(in)          :: v(nlayers,nnodes)
  character(*),intent(in)  :: filename
  integer                  :: inode, ilay
  character(60)            :: fmt(2)
  
  write(*,'(4x,a,a35)') 'Writing:', filename

  write(fmt(1),'("(2a8,2a18,",i5,"i14)")') nlayers
  write(fmt(2),'("(2i8,2f18.5,",i5,"e14.5)")') nlayers

  open(20, file=trim(filename), status='replace', action='write', buffered='YES')
  ! Header
  write(20,fmt(1)) 'Node', 'Zone', 'X', 'Y', (ilay, ilay=1, nlayers)
  ! Values
  do inode = 1, nnodes
    write(20,fmt(2)) inode, nodezone(inode), nodex(inode), nodey(inode), &
                     (v(ilay, inode), ilay=1,nlayers)
  end do
  
  close(20)

end subroutine writeNodeFile
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine writeIWFMgwfile()
  implicit none
    
  !integer, intent(in)      :: nskip
  integer                  :: i, ilay, ierr, nouth, noutf, ngroup
  character(1000)          :: line
  
  write(*,'(2x,a)') 'Reading gw temp file, writing new gw file'
    
  open(13, file=gwtemp, status='old')   ! Temp File
  open(12, file=GWFile, status='replace')   ! New File
    
  ! Copy Section Number
  call CopySettingsSection(13, 12, 1)
  ! Copy over the file header
  call CopyCommentSection(13, 12)
  ! Copy over main groundwater file settings (19 lines in V4.0)
  call CopySettingsSection(13, 12)
  ! Copy Debugging
  call CopyCommentSection(13, 12)
  call CopySettingsSection(13, 12)
  ! Copy Groundwater Hydrograph Output Settings
  call CopyCommentSection(13, 12)
  read(13, '(a1000)', iostat = ierr) line
  read(line, *) nouth
  write(12,'(a)') trim(line)
  call CopySettingsSection(13, 12)
  ! Copy Hydrogrph data
  call CopyCommentSection(13, 12)
  call CopySettingsSection(13, 12, nouth)
  ! Copy Element Face Flow Output Settings
  call CopyCommentSection(13, 12)
  read(13, '(a1000)', iostat = ierr) line
  read(line, *) noutf
  write(12,'(a)') trim(line)
  call CopySettingsSection(13, 12)
  ! Copy Element Face Data
  call CopyCommentSection(13, 12)
  call CopySettingsSection(13, 12, noutf)
  ! Copy Aquifer Parameters, Handle NGROUP setting
  call CopyCommentSection(13, 12)
  read(13, '(a1000)', iostat = ierr) line
  write(12,'(a)') trim(line)
  read(line, *) ngroup
  if (ngroup > 0) then
    write(*,'(a)') 'ERROR - NGROUP > 0 in Groundwater file (parametric grid)'
    write(*,'(a)') 'Only NGROUP = 0 is supported (node parameters)'
    stop
  end if
  ! Copy all the way down to aquifer parameter section
  call CopyCommentSection(13, 12)
  call CopySettingsSection(13, 12) ! Conversion Factors
  call CopyCommentSection(13, 12)
  call CopySettingsSection(13, 12) ! Units
  call CopyCommentSection(13, 12)
    
  ! Finally ready to write the aquifer parameters!
  do i=1, nnodes
    do ilay=1, nlayers
      if (ilay.eq.1) then
        write(12,'(i13,f14.8,e12.3,f14.8,f14.8,f14.8)') i, KhB(ilay,i), SsB(ilay,i), SyB(ilay,i), KvB_aqtard(ilay,i), KvB(ilay,i)
      else
        write(12,'(13x,f14.8,e12.3,f14.8,f14.8,f14.8)') KhB(ilay,i), SsB(ilay,i), SyB(ilay,i), KvB_aqtard(ilay,i), KvB(ilay,i)
      end if
      read(13,*)
    end do
  end do
    
  ! Copy the rest of the file
  do
    read(13, '(a1000)', iostat = ierr) line
    if (ierr /= 0) exit
    write(12,'(a)') adjustl(trim(line))
  end do
    
  close(12)
  close(13) 
  
end subroutine writeIWFMgwfile

!-----------------------------------------------------------------------------!
! IWFM File copying subroutines
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine CopyCommentSection(copyunit, newunit)
  implicit none
  
  integer,intent(in)       :: copyunit, newunit
  character(1)             :: compare
  character(1000)          :: line
  
  ! Copy lines until there are no more comment lines
  do
    read(copyunit, '(a1000)') line
    compare = ADJUSTL(line)
    if ((compare == 'C').or.(compare == 'c').or.&
        (compare == '*').or.(compare == '#')) then
      write(newunit,'(a)') trim(line)
    else
      ! Non comment found, back one line and exit
      backspace(copyunit)
      exit
    end if
  end do

end subroutine
!-----------------------------------------------------------------------------!
subroutine CopySettingsSection(copyunit, newunit, nlines)
  implicit none
  
  integer,intent(in)       :: copyunit, newunit
  integer                  :: i
  character(1)             :: compare
  character(1000)          :: line
  integer,intent(in),optional  :: nlines
  
  ! If nlines is passed, just copy over that many lines
  if (present(nlines)) then
    do i=1, nlines
      read(copyunit, '(a1000)') line
      write(newunit,'(a)') trim(line)
    end do
  else
    ! Copy lines until a comment line is found
    do
      read(copyunit, '(a1000)') line
      compare = adjustl(line)
      if ((compare == 'C').or.(compare == 'c').or.&
          (compare == '*').or.(compare == '#')) then
        ! Non comment found, back one line and exit
        backspace(copyunit)
        exit
      else
        write(newunit,'(a)') trim(line)
      end if
    end do
  end if
  
end subroutine
  
!-----------------------------------------------------------------------------!

subroutine add_elem2nodeelem(intd)
  use MakePar
  use errorhandle
  implicit none
  
  integer                  :: i, j, k, node
  integer, intent(in)      :: intd
  logical                  :: stored

  do j=1, 4
    stored = .false.
    node = elements(intd, j)
    if (node > 0) then
      ! Increment node-element membership array
      nnodeelem(node) = nnodeelem(node) + 1
      ! Ensure we're not going over our max
      if (nnodeelem(node) > max_node_elem_members) then
        write(*,*) 'ERROR - max_node_elem_members exceeded!'
        write(*,'(a,i7,a,i3)') 'node ', intd, 'is in >', max_node_elem_members
        stop
      end if
      ! Otherwise, add to the array
      nodeelem(node, nnodeelem(node)) = intd
    end if
  end do

  end subroutine add_elem2nodeelem

!-----------------------------------------------------------------------------!
    
subroutine intread(IFAIL,CLINE,iTEMP)
      integer, intent(out)            ::IFAIL
      character (len=*), intent(in)   ::cline
      integer, intent(out)            ::iTEMP
 
! -- Subroutine intREAD reads an integer number from a string.


! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       cline:    character string
!       itemp:    return integer

       CHARACTER*6 AFMT
 
       IFAIL=0
       AFMT='(i   )'
       WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
       READ(CLINE,AFMT,ERR=100) iTEMP

       RETURN
 
100    IFAIL=1
       RETURN
end subroutine intread
  
!-----------------------------------------------------------------------------!
  
subroutine readOneLine(infile,iinfile,cline)
!   read and return one line from a file, skipping lines that are commented out

    implicit none

    character (len=*), intent(in)  :: infile
    integer,           intent(in)  :: iinfile
    character (len=*), intent(out) :: cline

100  read(iinfile,'(a)',end=900) cline
        if((cline(1:1).eq.'c').or.(cline(1:1).eq.'C').or.&
            (cline(1:1).eq.'*').or.(cline(1:1).eq.'#')) go to 100    ! skip comment line

    go to 999 ! skip past error statements to end of subroutine
    
    !================================================================================
    ! error statements
900 write(*,901) trim(infile)
901 format(/,' *** Unexpected end to file ',a,' ***',/)
      stop
999 end subroutine readOneLine

!-----------------------------------------------------------------------------!
    
    SUBROUTINE multisplit(IFAIL,NUM,LW,RW,CLINE)
 
! -- Subroutine multisplit splits a string into blank-delimited fragments.

! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       num:      number of ...
!       lw:       number of ...
!       rw:       number of ...
!       cline:    character string

! -- Author:-
!       John Doherty
 
       INTEGER IFAIL,NW,NBLC,J,I
       INTEGER NUM,NBLNK
       INTEGER LW(NUM),RW(NUM)
       CHARACTER*(*) CLINE
       IFAIL=0
       NW=0
       NBLC=LEN_TRIM(CLINE)
       IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
         CALL TABREM(CLINE)
         NBLC=LEN_TRIM(CLINE)
       ENDIF
       IF(NBLC.EQ.0) THEN
         IFAIL=-1
         RETURN
       END IF
       J=0
5      IF(NW.EQ.NUM) RETURN
       DO 10 I=J+1,NBLC
         IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.&
         (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10     CONTINUE
       IFAIL=1
       RETURN
20     NW=NW+1
       LW(NW)=I
       DO 30 I=LW(NW)+1,NBLC
         IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.&
         (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30     CONTINUE
       RW(NW)=NBLC
       IF(NW.LT.NUM) IFAIL=1
       RETURN
40     RW(NW)=I-1
       J=RW(NW)
       GO TO 5
 
    END subroutine multisplit

!-----------------------------------------------------------------------------!

  subroutine TABREM(CLINE)
 
! -- Subroutine TABREM removes tabs from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string


       INTEGER I
       CHARACTER*(*) CLINE
 
       DO 10 I=1,LEN(CLINE)
10     IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
 
       RETURN
  end subroutine tabrem

!-----------------------------------------------------------------------------!
  
end module IWFM