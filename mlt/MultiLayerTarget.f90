  Program MultiLayerTarget
   use defn
   implicit none
   character (len=250) fname,infile,outfile,stratfile,elementsdeffile, nodesfile, obsfile,interpofile,preprocfile,outfracfile
   character (len=250)preprocfolder,simfile,GWfile, interpfile,dummystring,outinsfile,psthdrtargetfile
  
   character (len=50),allocatable :: obname(:),obslayer(:),obsDate(:),obtime(:)
   real, allocatable              :: obx(:),oby(:),InterpThick(:,:),InterpElev(:,:),InterpHK(:,:),InterpT(:,:),TOS(:),BOS(:),fscrobs(:)
   real, allocatable              :: nodex(:),nodey(:),Elevation(:,:),Thick(:,:),HK(:,:),obsvallayer(:),obsTrans(:),dum(:)
   integer                        :: nobs,nnodes,nlayers,nelements,ifail,nreg,NOUTF,NGROUP,NEBK,cnt,cntstart
   integer                        :: i,j,k,m,n,intd,lenght,ipos,skiplines,NOUTH,nodechange,nrecords,idum
   integer,allocatable            :: elements(:,:),ObsElem(:),recperobs(:),ObsOverwriteLayer(:)
   integer                        ::ls(100),rs(100)
   real                           ::char2double,FX,FACT,TUNITH,tempthick,tope,bote,weightedWL,obsscrthk,totf
  
    call getarg(1,infile)
    if(infile.eq.' ') then
      Write(*,*)'Enter input parametar file'
      read(*,*)infile
    endif
    Open(11,File=infile)
    
    read(11,*)obsfile
    read(11,*)preprocfolder
    read(11,*)preprocfile
    read(11,*)simfile
    read(11,*)interpfile
    read(11,*)outfile
    preprocfile= adjustl(trim(preprocfile))
    
    ipos = scan(outfile,'.')
    outinsfile = outfile(1:ipos-1) // '.ins'
    outfracfile=outfile(1:ipos-1) // '_LayFrac.dat'
  
    psthdrtargetfile=outfile(1:ipos-1) // '._pstTargets.txt'
    
     open(12,file=adjustl(trim(preprocfolder)) // adjustl(trim(preprocfile)))
     do i= 1,4
       call readOneLine(preprocfile,12,cline)
     end do
     read(12,*,end=100)elementsdeffile
     read(12,*,end=100)nodesfile
     read(12,*,end=100)stratfile
     close(12)
     
     ! read elements file
     write(*,*)'Reading elements file'
     open(12,file=adjustl(trim(preprocfolder)) // adjustl(trim(elementsdeffile)))
     
     do
       call readOneLine(elementsdeffile,12,cline)
       !cline= adjustl(trim(cline))
        IPOS=SCAN(cline,'NE')
        if (IPOS.GT.0) THEN
          EXIT
        end if
     end do
     
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nelements)
    if(ifail.ne.0) go to 9600
    
    allocate(elements(nelements,4))
     
    call readOneLine(elementsdeffile,12,cline)       
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nreg)
    if(ifail.ne.0) go to 9600
    
   
    call readOneLine(elementsdeffile,12,cline)         
   
    do i=1,nreg-1
      call readOneLine(elementsdeffile,12,cline)
   
    end do
    call readOneLine(elementsdeffile,12,cline)
    BACKSPACE(12)     
    do i=1,nelements
      READ(12,*)intd,(elements(intd,k),k=1,4)
    end do
    close(12)
    
    ! read node file
    write(*,*)'Reading node file'
     open(12,file=adjustl(trim(preprocfolder)) // adjustl(trim(nodesfile)))
     
     call readOneLine(nodesfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),nnodes)
     allocate(nodex(nnodes),nodey(nnodes))
      
      
     if(ifail.ne.0) go to 9600
     call readOneLine(elementsdeffile,12,cline)
     call readOneLine(elementsdeffile,12,cline)
     BACKSPACE(12)     
     do i=1,nnodes
       read(12,*)intd,nodex(i),nodey(i)
     end do
     close(12)
     
     
     ! read layers geometry file
     write(*,*)'Reading layers geometry file'
     open(12,file=adjustl(trim(preprocfolder)) // adjustl(trim(stratfile)))
     call readOneLine(nodesfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),nlayers)
     allocate(Elevation(nnodes,nlayers+1),Thick(nnodes,nlayers),HK(nnodes,nlayers),dum(nlayers))
     
     call readOneLine(nodesfile,12,cline)
     call readOneLine(nodesfile,12,cline)
     BACKSPACE(12)  
     do i=1,nnodes
      call readOneLine(nodesfile,12,cline)
!      call multisplit(ifail,nlayers*2+2,ls,rs,cline) 
      read(cline,*) idum,Elevation(i,1),((dum(k),Thick(i,k)),k=1,nlayers)
!      Elevation(i,1) = char2double(ifail,2,cline(ls(2):rs(2)))
      do k = 1,nlayers
!       Thick(i,k)= char2double(ifail,2,cline(ls((k+1)*2):rs((k+1)*2)))
!       if(Thick(i,k).lt.0.01) Thick(i,k)=0.01
       Elevation(i,k+1)=Elevation(i,k)-Thick(i,k)
      end do
      
     end do  
     close(12)
       
     
     !read  simulation file
     write(*,*)'Reading simulation file'
     open(12,file=adjustl(trim(simfile)))
     do i= 1,4
       call readOneLine(preprocfile,12,cline)
     end do
     read(12,*,end=100)GWfile
     close(12)
        
    open(12,file=adjustl(trim(GWfile)))
    ! skip list of the files
     do i= 1,21
       call readOneLine(preprocfile,12,cline)
     end do
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),NOUTH)
     
     ! read FACTXY
     call readOneLine(preprocfile,12,cline)
     ! read GWHYDOUTFL
     call readOneLine(preprocfile,12,cline)
     
     ! skip to observationslist 
     call readOneLine(preprocfile,12,cline)
     BACKSPACE(12)    
     ! read observations
     do i= 1,NOUTH
       call readOneLine(preprocfile,12,cline)
     end do

     !read NOUTF
     call readOneLine(preprocfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),NOUTF)
     
     
     !read FCHYDOUTFL
     call readOneLine(preprocfile,12,cline)
     
     do i= 1,NOUTF
       call readOneLine(preprocfile,12,cline)
     end do
     
       !read NGROUP
     call readOneLine(preprocfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),NGROUP)
     
     if (NGROUP.gt.0) then
      Write(*,*) 'Parametric grid is not suported'
      stop
     end if
     
     !read parametric fx
     call readOneLine(preprocfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     fx= char2double(ifail,2,cline(ls(1):rs(1)))
     
     ! Read time units
     do i= 1,3
       call readOneLine(preprocfile,12,cline)
     end do

     
     ! Read Hydraulic conductivity per node
     
     do i=1,nnodes
      do k = 1,nlayers
        call readOneLine(nodesfile,12,cline)
!        call multisplit(ifail,10,ls,rs,cline) 
        if (k.eq.1) then
          !HK(i,k)= char2double(ifail,2,cline(ls(2):rs(2)))      
          read(cline,*) idum,HK(i,k)
        else
          !HK(i,k)= char2double(ifail,2,cline(ls(1):rs(1)))      
          read(cline,*) HK(i,k)
        end if
      end do
     end do 
     
     
     ! read HK anomaly
     call readOneLine(nodesfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     call intread(ifail,cline(ls(1):rs(1)),NEBK)
     
     call readOneLine(nodesfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     FACT= char2double(ifail,2,cline(ls(1):rs(1)))  
     
     call readOneLine(nodesfile,12,cline)
     call multisplit(ifail,1,ls,rs,cline)
     if(ifail.ne.0) go to 9600
     TUNITH= char2double(ifail,2,cline(ls(1):rs(1)))      
     
     if(NEBK.gt.0) then
!       write(*,*) 'NEBK > 0, check MultiLayerTarget program!'
!       read(*,*)
!       stop
     endif
     do i=1,NEBK
       call readOneLine(nodesfile,12,cline)
       call multisplit(ifail,10,ls,rs,cline) 
      call intread(ifail,cline(ls(2):rs(2)),nodechange)
       
      do k = 1,nlayers
!commented for now. these are list of elements so first associated node numbers would be 
!needed and then K calculated. also need to find out how IWFM handles this internally.
!          HK(nodechange,k)= char2double(ifail,2,cline(ls(k+2):rs(k+2)))      
      end do
     end do 
     close(12)
     
     
   
    ! read observation file with coordinates and screen information    
     write(*,*)'Reading observation file with coordinates and screen information'
    nobs=0
    open(12,file=obsfile)
    read(12,*)
    do 
     read(12,*,end=100)  
     nobs=nobs+1
    end do
100 rewind(12)  
    allocate(obname(nobs),obx(nobs),oby(nobs),tos(nobs),bos(nobs),ObsElem(nobs),recperobs(nobs),obsTrans(nobs),ObsOverwriteLayer(nobs))
    allocate(fscrobs(nlayers))
    allocate(InterpElev(nobs,nlayers+1),InterpThick(nobs,nlayers),InterpT(nobs,nlayers),InterpHK(nobs,nlayers))
    read(12,*)
    do i=1,nobs
      read(12,*)obname(i),obx(i),oby(i),ObsElem(i),bos(i),tos(i),ObsOverwriteLayer(i)
    end do
    close(12)
    
  
  
     ! interpolate layer elevation and HK to the observation
      write(*,*)'Interpolate layer elevation and HK to the observation'
     !call IDW(nobs,obx,oby,ObsElem, InterpThick,nnodes,nlayers, nodex,nodey,Thick,nelements,elements)
     call IDW(nobs,obx,oby,ObsElem, InterpHK,nnodes,nlayers, nodex,nodey,HK,nelements,elements)
     call IDW(nobs,obx,oby,ObsElem, InterpElev,nnodes,nlayers+1, nodex,nodey,Elevation,nelements,elements)
     !call krige(nobs,obx,oby,ObsElem, KrigThick,nnodes,nodex,nodey,Thick,nlayers)
     
     open(21,file=outfracfile)
     write(21,'(a,50(a,i1))') ' obsname x y tos bos totalF ',(' F',k,k=1,nlayers),(' Elev',k,k=0,nlayers)
     InterpT=0
     obsTrans=0
      do i=1,nobs
       fscrobs=0.
       if (ObsOverwriteLayer(i).eq.-1) then 
           obsscrthk=TOS(i)-BOS(i)
         do k=1,nlayers
           if (TOS(i).lt.InterpElev(i,k)) then
             tope=TOS(i)
           else
             tope=InterpElev(i,k)
           end if
            if (BOS(i).gt.InterpElev(i,k+1)) then
             bote=BOS(i)
           else
             bote=InterpElev(i,k+1)
           end if
           tempthick = tope-bote
         
           if (tempthick.gt.0) then
            InterpT(i,k)=tempthick*InterpHK(i,k)
            fscrobs(k)=tempthick/obsscrthk
           end if
         
           obsTrans(i) = obsTrans(i) + InterpT(i,k)
         end do
       
         if (obsTrans(i).eq.0) then
           InterpT(i,nlayers)=1.0
           obsTrans(i)=1.0
         end if
       else
         ! overide screen elevation layer 
          tope=InterpElev(i,ObsOverwriteLayer(i))
          bote=InterpElev(i,ObsOverwriteLayer(i)+1)
          tempthick = tope-bote
          TOS(i)=tope
          BOS(i)=bote
           fscrobs(ObsOverwriteLayer(i))=1.
          
          InterpT(i,ObsOverwriteLayer(i))=tempthick*InterpHK(i,ObsOverwriteLayer(i))
          
         obsTrans(i) = obsTrans(i) + InterpT(i,ObsOverwriteLayer(i))
         
       end if
       totf=0.
       do k=1,nlayers
        totf=totf+fscrobs(k)
       enddo
       write(21,'(1x,a25,2(f15.3),50f15.6)')obname(i),obx(i),oby(i),TOS(i),BOS(i),totf,(fscrobs(k),k=1,nlayers),(InterpElev(i,k),k=1,nlayers+1)
       
         
       
      end do
     close(21)      
      
!   read spatial and temporal interpolated observation values for all layer calculated by iwfm2015obs      
      write(*,*)'Reading spatial and temporal interpolated observation values for all layer calculated by iwfm2015obs'
     nrecords=0
     open(12,file=interpfile)
     do 
      read(12,*,end=110)  
      nrecords=nrecords+1
     end do
110  rewind(12)  
     allocate (obslayer(nrecords),obsvallayer(nrecords),obsDate(nrecords),obtime(nrecords))
     do i=1,nrecords
      call readOneLine(interpfile,12,cline)
      call tabrem(cline)
      call multisplit(ifail,4,ls,rs,cline)
      if(ifail.ne.0) go to 9600
      obslayer(i) = cline(ls(1):rs(1))
      obsDate(i)= cline(ls(2):rs(2))
      obtime(i)= cline(ls(3):rs(3))
      obsvallayer(i)= char2double(ifail,2,cline(ls(4):rs(4)))   
      
      !read(12,*)obslayer(i),obsDate(i),obtime(i),obsvallayer(i)
     end do
     
     !count how many time records per each observation
     write(*,*)'Interpolating head from multiple layers'
     k=1
     do i=1,nobs
       cnt=0
       do j=k,nrecords
         IPOS=SCAN(obslayer(j),'%')
         dummystring = adjustl(trim(obslayer(j)))
         if (adjustl(trim(dummystring(1:IPOS-1))).eq.adjustl(trim(obname(i)))) then
           cnt=cnt+1
         else
           k=j
           exit
         end if
       end do
        recperobs(i)=cnt/nlayers
        !write(*,*)recperobs(i)
     end do

     cnt=1
     cntstart=1
     open(13,file=outfile)
     open(14,file=outinsfile)
     open(15,file=psthdrtargetfile)
     write(13,'(a)')'Name Date Time Simulated T1 T2 T3 T4 NewTOS NewBOS'
     write(14,*)'pif #'
     write(14,*)'l1'
     
     do i=1,nobs
       do j=1,recperobs(i)
        weightedWL=0
         cnt=cntstart
         do k = 1,nlayers
           weightedWL=weightedWL+obsvallayer(cnt)*InterpT(i,k)
           cnt=cnt+recperobs(i)
         end do
         weightedWL=weightedWL/obsTrans(i)
         write(13,'(a25,2a12,10F11.2,6f11.2)')obname(i),obsDate(cntstart), obtime(cntstart),weightedWL,(InterpT(i,k),k=1,nlayers),tos(i),bos(i)
         write(14,450)trim(obname(i)),j
         cntstart=cntstart+1
       end do
       cntstart=cnt-recperobs(i)+1
       
     end do
     
     close(13)
     close(14)
     
9600 stop      
     
     
450  format('l1  [',a,'_',I4.4,']50:60')
     
  end program
  
  subroutine IDW(nobs,x,y,ObsElem, InterpValues, nnodes,nlayers,nodex,nodey,NodeVal,nelements,elements)
  implicit none
  
  integer         :: i,j,k,n,m,nelements,nodeID
  integer         :: nnodes,nlayers,nobs
  integer         :: elements(nelements,4),ObsElem(nobs)
  real            :: interpV, totalDistance, Distance
  real            :: x(nobs),y(nobs),InterpValues(nobs,nlayers)
  real            :: nodex(nnodes),nodey(nnodes),NodeVal(nnodes,nlayers)
  real            :: CalcDistance,wgt_tmp,wgt
  
  do i=1,nobs
    wgt=0
    do j=1,4
      nodeID = elements(ObsElem(i),j)
      if (nodeID.gt.0) then
       Distance = CalcDistance(x(i),y(i),nodex(NodeID),nodey(nodeID))
       wgt_tmp = 1.0 / Distance;
	     wgt =wgt+ wgt_tmp;
       do k=1,nlayers
        InterpValues(i,k) =InterpValues(i,k) + wgt_tmp *  NodeVal(nodeID,k);
       end do
      end if
    
    end do
    do k=1,nlayers
        InterpValues(i,k) =InterpValues(i,k) / wgt
    end do
  end do
  
  
  
  end subroutine IDW
  
  
  logical function leap(year)

! -- Function LEAP returns .true. if a year is a leap year.

! -- Revision history:-
!       June-November, 1995: version 1.

    integer, intent(in)     :: year

        leap = (mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or. &
               (mod(year,400).eq.0 .and. year.ne.0 )

    return
  end function leap
  
  
      subroutine krige(kpoints,sx,sy,krigv,nobs,tx,ty,tv,nlayers)
      implicit none
!     SSPA Kriging Program modified by LS
!
!     FIND:   TOS/BOS for all wells in CVSIM model
!     GIVEN:  Aquifer Thickness & BOS for some wells
!     METHOD: Simple Kriging for Thickness, Ordinary Kriging for BOS.
!             TOS = BOS + Thickness
!             Use CVSIM GSE and Layer4 Bot to limit values

      integer,parameter         :: itype=2,MAXROT=1,IND=1
      integer                   :: nrow,nlay,j,i,k,ierr,nobs, kpoints,&
                                  n,ntot,maxpt,nlayers
      integer,allocatable       :: id(:),id2(:),lak(:,:),node(:)
      real*8,parameter          :: xoff=0.0d0, yoff=0.0d0
      real*8,parameter          :: ang1=0.0d0, &
                                   ang2=0.0d0, &
                                   ang3=0.0d0, &
                                   anis1=1.0d0, &
                                   anis2=0.0d0, &
                                   slope=0.0d0, &
                                   sill=1., &
                                   a=8.5d4, &
                                   nugget= 0.0d0, &
                                   median=0.0
      real*8                    :: rotmat(1,3,3),x,y,value,zkrig
      real                      :: sx(kpoints),sy(kpoints),krigv(kpoints,nlayers)
      real                      :: tx(nobs),ty(nobs),tv(nobs,nlayers)
      real*8,allocatable        :: delr(:),delc(:), &
                                   p(:),r(:),w(:), &
                                   p2(:),r2(:),w2(:), &
                                   kriged_thk(:),kriged_bos(:)
     
      character(18),allocatable :: name(:)


!------------Set rotation matrix-----------
      call setrot(ang1,ang2,ang3,anis1,anis2,ind,MAXROT,rotmat)


!------------Krige Thickness---------------

!     read KNOWN LOCS and VALUE, subtract MEAN from VALUE
   
      !do n=1,nobs
      !  read(12,*) x,y,value
      !  tx(n)=x-xoff
      !  ty(n)=y-yoff
      !  tv(n)=value - median                                            ! Subtracting Median
      !end do

!     setup LHS
      write(*,'(/,3x,a)') ' Setting up LHS...'
      ntot=nobs !+1
      maxpt=ntot*(ntot+1)/2
      allocate(p(maxpt),r(ntot),w(ntot),id(ntot))
      p=0.d0
      id=0
      write(*,'(/,3x,a)') ' Calling PSET...'
      call PSET(p,tx,ty,id,nobs,sill,ang1,ang2,ang3,anis1,anis2,ind, &
                MAXROT,rotmat,itype,a,nugget,ntot)
      write(*,'(/,3x,a)') ' Calling DECOMP...'
      call decomp(p,id,ntot)

!     loop through Grid Locs
      allocate(kriged_thk(nrow))
      write(*,'(/,3x,a)') ' Kriging Thickness...'
      do i=1,kpoints
!        write(*,'(3x,a,i3)') '    Working on ROW: ',i
        r=0.d0
        w=0.d0
        !r(ntot)=1.0d0                                                  ! commented = simple, uncommented = ordinary
        do n=1,nobs
          call variogram(itype,sx(i),sy(i),0.0d0,tx(n),ty(n),0.0d0, &
                        ind,MAXROT,rotmat,value,sill,a,nugget)
          r(n)=value
        end do
        call doolittle(r,p,id,w,ntot)
        value=0.0d0
      
        do j=1,nlayers
          zkrig=0.0d0
          do n=1,nobs
            zkrig=zkrig+w(n)*tv(n,j)
          end do
          zkrig=zkrig+value
          krigv(i,j)=zkrig + median                                    ! add back in your Median
        end do
      end do

!      Thickness output
!     open(10, file='KrigedThick.out')
!     write(10, '(4(1a,3x))') 'NAME','X','Y','kTHICKNESS'
!     do i=1,nrow
!        write(10,'(1a,3f14.4)') name(i),sx(i),sy(i),kriged_thk(i)
!     end do
!     close(10)


!     All Output
      open(10, file='Kriged.out')
      write(10, '(5(1a,3x))') 'NAME','X','Y','kBOS','kTHICKNESS'      
      do i=1,nrow
        write(10,'(1a,4f14.4)') name(i),sx(i),sy(i),kriged_BOS(i),kriged_thk(i)
      end do
      close(10)

      end subroutine

      SUBROUTINE PSET (P,X,Y,ID,N,SILL,ang1,ang2,ang3,anis1,anis2, &
           ind,MAXROT,rotmat,itype,a,nugget,ntot)
      implicit none
!***  FROM SUBROUTINE WRITTEN BY SKRIVAN AND KARLINGER AND SLIGHTLY
!***  MODIFIED BY W.D. GRUNDY
!***  CALCULATE COEFFICIENTS IN THE SYMMETRIC P MATRIX WHICH WILL
!***  CONSIST OF THE LOWER TRIANGULAR PORTION STORED BY COLUMNS.
!***  ID IS THE POINTER VECTOR GIVING THE LOCATION OF THE DIAGONAL
!***  ELEMENTS IN P.
      integer id,ntot,np1,N,ind,MAXROT,MAXPT,i,j,js,itype
      real*8 p,x,y,rotmat,SILL,ang1,ang2,ang3,anis1,anis2,x1,x2,y1,y2, &
            value,a,nugget
      DIMENSION P(1),X(1),Y(1),ID(1)
      dimension rotmat(1,3,3)
!      SAVE
!      NTOT=N+1
      NP1=N+1
!***  ZERO OUT ELEMENTS OF P-MATRIX NEEDED FOR KRIGING
      MAXPT=NTOT*(NTOT+1)/2
      DO 10 I=1,MAXPT
      P(I)=0.
   10 CONTINUE
!***  CALCULATE THE POINTERS FOR ID
      ID(1)=1
      DO 20 I=2,NTOT
   20 ID(I)=ID(I-1)+NTOT+2-I
!***  CALCULATE THE COEFFICIENTS OF THE COVARIANCE OF POINTS I AND J
      DO 30 J=1,N
        JS=ID(J)-J
      DO 30 I=J,N
        x1=x(i)
        y1=y(i)
        x2=x(j)
        y2=y(j)
        call variogram(itype,x1,y1,0.0d0,x2,y2,0.0d0,ind,MAXROT, &
                      rotmat,value,sill,a,nugget)
        P(JS+I)=value
   30 CONTINUE
      DO 40 I=1,N
   40 P(ID(I)+NP1-I)=1
      RETURN
  END subroutine
  

!-----------------------------------------------------------------------
    	subroutine decomp(p,id,ntot)
      implicit none
!*** DECOMPOSITION TO GET LOWER TRIANGULAR S MATRIX
          integer id,ier,ks,jm1,k,js,is,is1,j,i,i1,ntot
          real*8 p,sum,p1,p2
    	dimension p(1),id(1)
          IER=0
          P(1)=1./P(1)
          DO 65 J=2,NTOT
          DO 65 I=J,NTOT
            KS=ID(I)
            JM1=J-1
            SUM=0.
            DO 55 K=1,JM1
              JS=ID(K)
              IS=JS+I-K
              IS1=JS+J-K
              P1=P(IS)
              P2=P(IS1)
              IF (P1) 39,55,39
   39         IF (P2) 50,55,50
   50         SUM=SUM+P1*P2*P(JS)
   55         CONTINUE
            I1=ID(J)+I-J
            P(I1)=P(I1)-SUM
            IF (I-J) 65,70,65
   70       IF(P(KS)) 90,91,90
   90       P(KS)=1./P(KS)
   65      CONTINUE
          GOTO 92
   91     IER=1
   92     CONTINUE
      return
      end

!-----------------------------------------------------------------------
      subroutine doolittle(r,p,id,w,ntot)
          implicit none
          integer id,ntot,i,is,js,im1,j,ii,iip1,iis
          real*8 r,p,w,sum
      dimension r(1),p(1),w(1),id(1)
!***  START OF DOOLITTLE ***
!***  FORWARD SUBSTITUTION FOR SYSTEM S*W = R
          W(1)=R(1)*P(1)
          DO 130 I=2,NTOT
            IS=ID(I)
            SUM=0.
            IM1=I-1
            DO 120 J=1,IM1
              JS=ID(J)+I-J
  120       SUM=SUM+W(J)*P(JS)
  130     W(I)=(R(I)-SUM)*P(IS)
!***  BACKWARD SUBSTITUTION FOR SYSTEM T*V = W
!***  WHERE V IS STORED IN W
          DO 150 I=2,NTOT
            SUM=0.
            II=NTOT-I+1
            JS=ID(II)
            IIP1=II+1
            DO 140 J=IIP1,NTOT
              IIS=JS+J-II
  140       SUM=SUM+W(J)*P(IIS)
  150     W(II)=W(II)-SUM*P(JS)
!*** END OF DOOLITTLE ****
      return
    	end

!-----------------------------------------------------------------------
      real*8 function sqdist(x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat)
      implicit none
!
!    Squared Anisotropic Distance Calculation Given Matrix Indicator
!    ***************************************************************
!
! This routine calculates the anisotropic distance between two points
!  given the coordinates of each point and a definition of the
!  anisotropy.
!
!
! INPUT VARIABLES:
!
!   x1,y1,z1         Coordinates of first point
!   x2,y2,z2         Coordinates of second point
!   ind              The matrix indicator to initialize
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!                  
!
!
! OUTPUT VARIABLES:
!
!   sqdist           The squared distance accounting for the anisotropy
!                      and the rotation of coordinates (if any).
!
!
! Author: C. Deutsch                                Date: September 1989
      integer   i,ind,MAXROT
      real*8    rotmat(MAXROT,3,3),dx,dy,dz,x1,y1,x2,y2,z1,z2,cont
!
! Compute component distance vectors and the squared distance:
!
      dx = x1 - x2
      dy = y1 - y2
      dz = z1 - z2
          sqdist = 0.0d0
      do 1 i=1,3
        cont   = rotmat(ind,i,1) * dx &
                      + rotmat(ind,i,2) * dy &
                      + rotmat(ind,i,3) * dz 
        sqdist = sqdist + cont*cont
    1   continue
      return
      end


!-----------------------------------------------------------------------
      subroutine variogram(itype,x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat, &
                          value,sill,a,nugget)
      implicit none
!      common block/vario/slope,sill,a
      integer ind,MAXROT,itype
      real*8 rotmat,x1,y1,z1,x2,y2,z2,value,h,sqdist,sill,tmp,a,nugget
      dimension rotmat(1,3,3)
      h=sqdist(x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat)
! linear variogram
      if(itype.eq.1)  H=sqrt(h)
! uniform variogram
      if(itype.eq.0)  h=sill
! spherical variogram
      if(itype.eq.2) then
        H=dSQRT(h)
        tmp=h/a
        if(tmp.ge.1.) then
          h=sill
        else
          h=sill*(1.5d0*tmp-0.5d0*tmp*tmp*tmp)
        endif
      endif
      value=sill-h+nugget
      return
      end

!-----------------------------------------------------------------------
      subroutine setrot(ang1,ang2,ang3,anis1,anis2,ind,MAXROT,rotmat)
      implicit none
!
!              Sets up an Anisotropic Rotation Matrix
!              **************************************
!
! Sets up the matrix to transform cartesian coordinates to coordinates
! accounting for angles and anisotropy (see manual for a detailed
! definition):
!
!
! INPUT PARAMETERS:
!
!   ang1             Azimuth angle for principal direction
!   ang2             Dip angle for principal direction
!   ang3             Third rotation angle
!   anis1            First anisotropy ratio
!   anis2            Second anisotropy ratio
!   ind              The matrix indicator to initialize
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!
!
!
! Author: C. Deutsch                                Date: September 1989
!-----------------------------------------------------------------------
      integer                   :: MAXROT,ind
      real*8, parameter         :: DEG2RAD=3.14159265d0/180.0d0, &
                                   EPSLON=0.000001d0
      real*8                    :: rotmat(MAXROT,3,3),ang1,ang2,ang3, &
                                  anis1,anis2,alpha,beta,theta, &
                                  sina,sinb,sint,cosa,cosb,cost, &
                                  afac1,afac2
!
! Converts the input angles to three angles which make more
!  mathematical sense:
!
!         alpha   angle between the major axis of anisotropy and the
!                 E-W axis. Note: Counter clockwise is positive.
!         beta    angle between major axis and the horizontal plane.
!                 (The dip of the ellipsoid measured positive down)
!         theta   Angle of rotation of minor axis about the major axis
!                 of the ellipsoid.
!
      if(ang1.ge.0.0d0.and.ang1.lt.270.0d0) then
          alpha = (90.0d0   - ang1) * DEG2RAD
      else
          alpha = (450.0d0  - ang1) * DEG2RAD
      endif
      beta  = -1.0d0 * ang2 * DEG2RAD
      theta =          ang3 * DEG2RAD
!
! Get the required sines and cosines:
!
      sina  = sin(alpha)
      sinb  = sin(beta)
      sint  = sin(theta)
      cosa  = cos(alpha)
      cosb  = cos(beta)
      cost  = cos(theta)
!
! Construct the rotation matrix in the required memory:
!
      afac1 = 1.0d0 / max(anis1,EPSLON)
      afac2 = 1.0d0 / max(anis2,EPSLON)
      rotmat(ind,1,1) =       (cosb * cosa)
      rotmat(ind,1,2) =       (cosb * sina)
      rotmat(ind,1,3) =       (-sinb)
      rotmat(ind,2,1) = afac1*(-cost*sina + sint*sinb*cosa)
      rotmat(ind,2,2) = afac1*(cost*cosa + sint*sinb*sina)
      rotmat(ind,2,3) = afac1*( sint * cosb)
      rotmat(ind,3,1) = afac2*(sint*sina + cost*sinb*cosa)
      rotmat(ind,3,2) = afac2*(-sint*cosa + cost*sinb*sina)
      rotmat(ind,3,3) = afac2*(cost * cosb)
!
! Return to calling program:
!
      return
      end
