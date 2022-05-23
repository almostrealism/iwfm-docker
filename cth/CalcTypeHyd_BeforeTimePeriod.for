      program CalcTypeHyd
      implicit none
            
      character                       :: fobs*50,fwts*50,fwl*50,
     +                                   fout*50,fins*50,
     +                                   subreg_str*18,wl_str*11,
     +                                   dumstr1*12,dumstr2*18,
     +                                   clusid_str*2,
     +                                   mm_str*2,yyyy_str*4,
     +                                   refdate_str*10,
     +                                   startdate_str*10,
     +                                   enddate_str*10,date2str*10,
     +                                   wellnm_obs*25,wellnm*25,
     +                                   lay_str*1,datestr*12,
     +                                   subregnm*18,header*50,pbase*50,
     +                                   pstnam*20,id_str*4,pifstr*30,
     +                                   c_st_str*2,c_end_str*2,hdr1*14
      character,allocatable           :: wellnm_ary(:)*25,date_str(:)*10
      integer                         :: nclus,ncluswells,nhydro,
     +                                   startm,endm,date,
     +                                   i,j,k,nval,mm,yyyy,dumint,
     +                                   monthrange,year,month,strmatch,
     +                                   count,n,c_st,c_end,
     +                                   c1,c2,c3,c4
      integer,allocatable             :: clusid(:),clus(:),wid(:),
     +                                   date_id(:)
      real                          :: dumvar,wl,wlsum,nzwtsum
      real,allocatable              :: cluswt(:,:),mo_avg(:,:),
     +                                   non_zero_wts(:,:),
     +                                   mean(:),demeaned(:,:),
     +                                   sumproduct(:),typehydro(:)
      
c------------------------------------------------------      
c     open main input file      
      open(10,file='CalcTypeHyd.in')
      read(10,*) fwl                 
      read(10,*) fwts      
      read(10,*) nclus
      read(10,*) ncluswells
      read(10,*) nhydro
      allocate(clusid(nhydro))
      do i=1,nhydro
          read(10,*) clusid(i)          
      end do
      read(10,'(a10)') startdate_str
      read(10,'(a10)') enddate_str
      read(10,*)  pbase
      close(10)

c------------------------------------------------------      
c     read cluster weights file
      write(*,*) 'reading cluster weights...'
      allocate(cluswt(ncluswells,nclus),
     +         wellnm_ary(ncluswells),
     +         clus(ncluswells),
     +         wid(ncluswells))
      
      open(10,file=fwts)
c      open(20,file='test1.out')      
      read(10,*)
      do i=1,ncluswells
          read(10,*) wellnm_ary(i),(cluswt(i,j),j=1,nclus),
     +               dumint,clus(i),subreg_str
          wid(i) = i
c          write(20,'(a20,<nclus>f12.4,i4,a18)') 
c     +          wellnm_ary(i),(cluswt(i,j),j=1,nclus),
c     +          clus(i),adjustr(subreg_str)
      end do
      close(10)
c      close(20)

c------------------------------------------------------      
c     SET UP DATE ARRAYS
      write(*,*) 'setting up date arrays...'      
c     get cut off date id's and allocate date id array (for indexing)
      refdate_str='01/01/1900'
      startm = monthrange(refdate_str,startdate_str,'/')
      endm = monthrange(refdate_str,enddate_str,'/')
      allocate(date_id(endm))
      date_id = 0
      
c     get number of output dates           
      nval = monthrange(startdate_str,enddate_str,'/')+1
      
c     allocate date string array (for output)
      allocate(date_str(nval))
      
c     fill date arrays      
c      open(10,file='test2.out')
      k = startm
      do i=1,nval
          if(i.eq.1) then
              mm = month(startdate_str,'/')
              yyyy = year(startdate_str,'/')
          else
              if(mm.eq.13) then
                  mm = 1
                  yyyy = yyyy + 1
              end if              
          end if
          date_str(i) = date2str(mm,15,yyyy,'/')
          date_id(k) = i
c          write(10,'(a12,2i8)') date_str(i),date_id(k),k
          mm = mm + 1
          k = k + 1
      end do
c      close(10)
      
c------------------------------------------------------      
c     READ WL DATA
      write(*,*) 'reading WL data...'            
      allocate(mo_avg(nval,ncluswells))
      mo_avg = -9999.
      open(10,file=fwl)
c     FORMAT FOR READING OBSERVED DATA, COMMENT OUT NEXT LINE IF READING SIMULATED DATA
!5     format(a22,a11,a8,a)      
      
c     FORMAT FOR READING SIMULATED DATA, COMMENT OUT NEXT TWO LINES IF READING OBSERVED DATA
      read(10,*)      
5     format(a25,a12,a12,a11)              

      do       
        read(10,5,end=100) wellnm_obs,datestr,dumstr1,wl_str
        
c     UNCOMMENT NEXT THREE LINES IF READING OBSERVED DATA/COMMENT OUT IF READING SIMULATED DATA
        !k = index(wellnm_obs,'%1')
        !if(k.eq.0) cycle
        !wellnm = wellnm_obs(1:k-1)
        
c     UNCOMMENT NEXT LINE IF READING SIMULATED DATA/COMMENT OUT IF READING OBSERVED DATA
        wellnm = wellnm_obs
        
        date = monthrange(refdate_str,datestr,'/')
        if(date.ge.startm .and. date.le.endm) then
            k = strmatch(wellnm_ary,ncluswells,wellnm)
            if(k.gt.0) then
                read(wl_str,*) wl            
                mo_avg(date_id(date),k) = wl
            end if
        end if
      end do
100   continue
      close(10)
c      open(10,file='test3.out')       
c      do i=1,nval
c          write(10,'(<ncluswells>f10.2)') 
c     +         (mo_avg(i,j),j=1,ncluswells)
c      end do
c      close(10)

c------------------------------------------------------      
c     CALCULATE MEANS
      write(*,*) 'calculating means...'      
      allocate(mean(ncluswells))
c      open(10,file='test4.out')
      do j=1,ncluswells
          wlsum = 0.
          count = 0          
          do i=1,nval
              if(mo_avg(i,j).gt.-9000.) then
                  wlsum = wlsum + mo_avg(i,j)
                  count = count + 1
              end if
          end do
          mean(j) = wlsum/count
c          write(10,'(a20,f16.4,i6,f16.6)') 
c     +          wellnm_ary(j),wlsum,count,mean(j)
      end do
      close(10)

c------------------------------------------------------      
c     GENERATE TYPE HYDROGRAPH(S) / WRITE PEST INSTRUCTION FILE(S)
      write(*,*) 'generating type hydrographs...'
      
      allocate(non_zero_wts(nval,ncluswells))      
      allocate(demeaned(nval,ncluswells))      
      allocate(sumproduct(nval))
      allocate(typehydro(nval))      
      
      do n=1,nhydro
        write(*,'(a13,i2,a1,i2)') '--hydrograph ',n,'/',nhydro
c       set up non-zero weight matrix
        non_zero_wts=0.
c        open(10,file='test5.out')
        do i=1,nval
            do j=1,ncluswells
                if(mo_avg(i,j).gt.-9000.)
     +          non_zero_wts(i,j) = cluswt(j,clusid(n))
            end do
c            write(10,'(<ncluswells>f12.4)')
c     +      (non_zero_wts(i,j),j=1,ncluswells)
        end do
c        close(10)

c       set up de-meaned WL matrix
        demeaned=0.
c        open(10,file='test6.out')
        do i=1,nval
            do j=1,ncluswells
                if(mo_avg(i,j).gt.-9000.)
     +          demeaned(i,j) = mo_avg(i,j)-mean(j)
            end do
c            write(10,'(<ncluswells>f12.4)')
c     +      (demeaned(i,j),j=1,ncluswells)
        end do
c        close(10)
        
c       set up sumproduct vector
c        open(10,file='test7.out')
        do i=1,nval
            sumproduct(i)=
     +      dot_product(cluswt(:,clusid(n)),demeaned(i,:))
c            write(10,'(f20.6)') sumproduct(i)
        end do
c        close(10)
        
c       open and set up output files
c       -set up output file names
        clusid_str='00'
        write(clusid_str,'(i2)') clusid(n)
        k = index(fwts,'.')
        subregnm = fwts(1:k-1)

C       COMMENT/UNCOMMENT NEXT TWO LINES BASED ON READING OBS/SIM DATA
        !header = 'obs_'//
        header = 'sim_'//        
     +       trim(adjustl(subregnm))//
     +       '_cls'//
     +       trim(adjustl(clusid_str))
        fout = trim(adjustl(header))//'.out'
        fins = trim(adjustl(header))//'.ins'

c       -set up output file headers
        open(10,file=fout)
        c1=14
        c2=12
        c3=20
        c4=40
        hdr1='PEST_NAME'
        write(10,'(a<c1>,a<c2>,a<c4>)')
     +       adjustl(hdr1),'DATE',trim(header)
        
c       COMMENT OUT NEXT THREE LINES FOR OBSERVED DATA / KEEP FOR SIMULATED
        open(20,file=fins)
        write(20,'(a5)') 'pif #'
        write(20,'(a2)') 'l1'        
      
c       -calculate type hydrograph
        typehydro=-9999.
        do i=1,nval
            nzwtsum = sum(non_zero_wts(i,:))
            if(nzwtsum.ne.0.)
     +      typehydro(i) = sumproduct(i) / nzwtsum
            write(id_str,'(i0)') i
            pstnam = trim(adjustl(pbase))//
     +               trim(adjustl(clusid_str))//'_'//
     +               trim(adjustl(id_str))
c       -write to files
c       ----main output file
            if(typehydro(i).gt.-9000.)
     +          write(10,'(a<c1>,a<c2>,f<c3>.6)')
     +          pstnam,date_str(i),typehydro(i)

c       COMMENT OUT NEXT SECTION FOR OBSERVED DATA / KEEP FOR SIMULATED            
c       ----pest instruction file
            if(typehydro(i).gt.-9000.) then
                c_st = c1 + c2 + 8
                c_end = c1 + c2 + c3
                write(c_st_str,'(i0)') c_st   
                write(c_end_str,'(i0)') c_end
                pifstr='l1 ['//
     +          trim(adjustl(pstnam))//
     +          ']'//
     +          c_st_str//':'//c_end_str
                k=len_trim(pifstr)
                write(20,'(a<k>)') pifstr                
            end if
        end do
        close(10)
        
c       COMMENT OUT NEXT SECTION FOR OBSERVED DATA / KEEP FOR SIMULATED         
        close(20)
      end do      

      end program
c------------------------------------------------------------------------       
      integer function monthrange(datemin,datemax,delim)
      implicit none      
      character                   :: datemin*10,datemax*10,delim*1
      integer                     :: startmo,intermo,endmo,year,month 
      
      startmo = 12 - month(datemin,delim)
      intermo = ((year(datemax,delim) - year(datemin,delim)) - 1) * 12
      endmo = month(datemax,delim)
      
      monthrange = startmo + intermo + endmo
      
      end function            
c------------------------------------------------------------------------            
      integer function year(date_str,delim)
      implicit none
      character                   :: date_str*10,delim*1,yr_str*4
      integer                     :: indx
      
      indx=index(date_str,delim,back=.true.)+1
      yr_str = date_str(indx:)
      read(yr_str,*) year
      
      end function
c------------------------------------------------------------------------            
      integer function month(date_str,delim)
      implicit none      
      character                   :: date_str*10,delim*1,mo_str*2
      
      mo_str = date_str(1:2)
      read(mo_str,*) month
      
      end function
c------------------------------------------------------------------------           
      function date2str(mm,dd,yyyy,delim)
      implicit none      
      character                   :: date2str*10,delim*1,
     +                               mm_str*4,dd_str*2,yyyy_str*4 
      integer                     :: mm,dd,yyyy
      
      write(mm_str,'(i2)') mm
      if(mm.lt.10) mm_str = '0'//trim(adjustl(mm_str))
      write(dd_str,'(i2)') dd      
      write(yyyy_str,'(i4)') yyyy          
      date2str = trim(adjustl(mm_str)) // delim // 
     +           trim(adjustl(dd_str)) // delim //
     +           yyyy_str
      
      end function
c------------------------------------------------------------------------
      integer function strmatch(list,n,str)
      implicit none      
      integer,intent(in)          :: n
      integer                     :: i
      character                   :: list(n)*25
      character(*)                :: str
      
      strmatch=0
      do i=1,n
          if(str.eq.list(i)) then
              strmatch=i
              exit
          end if
      end do
      
      end function
c------------------------------------------------------------------------      