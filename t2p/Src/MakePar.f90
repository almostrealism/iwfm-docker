MODULE MakePar
  use kd_tree
    implicit none

!-----------------------------------------------------------------------------!
! MakePar Module for Texture2Par
! Some subroutines were originally VBA subroutines written by Tim Durbin
! Author: Leland Scantlebury of S.S. Papadopulos & Associates
!-----------------------------------------------------------------------------!
    
    SAVE
    ! Parameters
    integer, parameter        :: max_node_elem_members = 15, &                 ! Maximum number of elements a node is a part of (will stop w/ error if too low)
                                 max_zones = 100, &                            ! Maximum number of zones (used for both pilot point and geologic)
                                 USE_IDW = 0
    real, parameter           :: NODATA=-9999.0
                              
    real,allocatable          :: wellelevbot(:,:), ssb(:,:), pcwellelem(:,:), &
                                 xwell(:), wellaqtardelevtop(:,:), &
                                 wellelevtop(:,:), nkcmin(:), nsyc(:), &
                                 atnkcmin(:), atdkf(:), nkfmin(:), ywell(:), &
                                 zland(:), aqtardbotelev(:,:), atdkc(:), &
                                 nodex(:), nodey(:), nsyf(:), dkc(:), &
                                 zwell(:,:), nanisoC(:), nanisoF(:), khb(:,:), &
                                 pcwellaqtardelem(:,:), atnkfmin(:), nssf(:), &
                                 aqtardtopelev(:,:), nssc(:), kvb(:,:), &
                                 syb(:,:), elevation(:), botelev(:,:), &
                                 wellaqtardelevbot(:,:), topelev(:,:), &
                                 pcnode(:,:), pcnode_aqtard(:,:), &
                                 kvb_aqtard(:,:), dkf(:), &
                                 Thick(:,:), AqTardThick(:,:), &
                                 nodexy(:,:), atnAnisoC(:), atnAnisoF(:), &
                                 interbed_thick(:,:)
    integer,allocatable       :: wellelem(:), pcwellzone(:,:), numzone(:), &
                                 elements(:,:), nodeelem(:,:), nnodeelem(:), &
                                 nodezone(:), ngeozones_lay(:)
    integer                   :: nlayers, nnodes, nwell, wfilelen, &
                                 nelements, maxWPoints, pckrige_nwells, &
                                 nzones, zones(max_zones), ngeozones
    real                      :: kfk, syp, khp, kvp, kck
    character(30),allocatable :: geozones(:,:), wellgeozones(:,:), &
                                 geonames_lay(:,:)
    character(30)             :: geonames(max_zones)
    character(60)             :: outfile
    character(150)            :: wellfile, zonefile, geozonefile
    logical                   :: colocated=.false.
    
    ! Global Trees
    type(tree_master_record), pointer :: nodetree

    CONTAINS

!-----------------------------------------------------------------------------!
SUBROUTINE layerelevation(top_elev, bot_elev, result_top, result_bot)
    implicit none
!-----------------------------------------------------------------------------!
! Find layer elevations at each well using node elevation data.
! Inverse Distance Weighting is used to determine the elevations.
!
! Arguments:
!    - topelev    - Top elevation at nodes
!    - botelev    - Bottom elevation at nodes
!    - result_top - Layer top elevations at each well
!    - result_bot - Layer bottom elevations at each well
!-----------------------------------------------------------------------------!
    real,intent(in)          :: top_elev(nnodes, nlayers), &
                                bot_elev(nnodes, nlayers)
    real,intent(inout)       :: result_top(nwell, nlayers), &
                                result_bot(nwell, nlayers)

    result_top = 0.0
    result_bot = 0.0
    
    call IDW(nwell, xwell, ywell, wellelem, result_top, nnodes, nlayers, &
             nodex, nodey, top_elev, nelements, elements)
    call IDW(nwell, xwell, ywell, wellelem, result_bot, nnodes, nlayers, &
             nodex, nodey, bot_elev, nelements, elements)

end subroutine layerelevation
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
SUBROUTINE layertexture(well_topelev, well_botelev, pcwellout)
    implicit none
!-----------------------------------------------------------------------------!
! This subprogram calculates the proportion of coarse-grained material at each
! well within an element layer, where the well at least partially penetrates 
! the element layer.
!
! Arguments:
!    - well_topelev - Top elevation at every well
!    - well_botelev - Bottom elevation at every well
!    - pcwellout    - Percent coarse at each well element
!-----------------------------------------------------------------------------!
    
    real,intent(in)          :: well_topelev(nwell, nlayers), &
                                well_botelev(nwell, nlayers)
    real,intent(inout)       :: pcwellout(nwell, nlayers)
    real                     :: elemtop, elembot, zonetop, zonebot, &
                                ztop, zbot, lengthin, sumlengthin, &
                                sumpcwellzone
    integer                  :: i, iwell, ilay, izone, irow
    character(60)            :: fmt(2)

    !write(*,'(4x, a)') 'Calculating coarse-grained % for each well in each layer'
    do iwell = 1, nwell
      do ilay = 1, nlayers

        elemTop = well_topelev(iwell, ilay)
        elemBot = well_botelev(iwell, ilay)

!     Interval within layer
        SumPcWellZone = 0
        SumLengthIn = 0
        do izone = 1, numZone(iwell)
          if (izone == 1) then
            zoneBot = Zwell(iwell, izone)
            zoneTop = Zland(iwell)
          else
            zoneBot = Zwell(iwell, izone)
            zoneTop = Zwell(iwell, izone - 1)
          end if
!       Calculate vertical weight for screen interval within element
          if ((zoneTop >= elemBot).and.(zoneBot <= elemTop).and.&
              (PcWellZone(iwell, izone) >= 0)) then

            ! Find min top/bot values
            Ztop = min(elemTop, zoneTop)
            Zbot = max(elemBot, zoneBot)
            LengthIn = Ztop - Zbot
            SumLengthIn = SumLengthIn + LengthIn
            SumPcWellZone = SumPcWellZone + LengthIn * PcWellZone(iwell, izone)
          end if
        end do
        pcwellout(iwell, ilay) = NODATA
        if (SumLengthIn > 0.0) then
          pcwellout(iwell, ilay) = SumPcWellZone / SumLengthIn
        end if
      end do
    end do

end subroutine layertexture
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine pp_parkrige(ppx, ppy, ppv, npp, ppz, kriged)
  use Krige
  implicit none
!-----------------------------------------------------------------------------!
! Loops over zones kriging pilot point parameter values to nodes
!-----------------------------------------------------------------------------!
  
  real,intent(in)          :: ppx(npp), ppy(npp), ppv(npp)
  real,intent(inout)       :: kriged(nnodes)
  integer,intent(in)       :: ppz(npp), npp
  
  integer                  :: i, ipp, ppcount, zone
  real                     :: pzx(npp), pzy(npp), pzv(npp)
  
  ! Loop over zones
  do i=1, nzones
    zone = zones(i)    ! Zones do not necessarily have to be in any order
    ! Round up pilot points in zone
    ppcount = 0
    do ipp=1, npp
      if (ppz(ipp) == zone) then
        ppcount = ppcount + 1
        pzx(ppcount) = ppx(ipp)
        pzy(ppcount) = ppy(ipp)
        pzv(ppcount) = ppv(ipp)
      end if
    end do
    if (ppcount > 0) then
      ! Krige
      call spkrige_group(ppcount, nnodes, nodex, nodey, pzx, pzy, pzv, &
                         kriged, nodezone, zone)
    else
      write(*,*) 'ERROR - no pilot points for zone ', zones(i)
      stop
    end if
  end do
  
end subroutine pp_parkrige
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
SUBROUTINE interpolate()
    use Krige
    implicit none
!-----------------------------------------------------------------------------!
! Interpolates from wells to nodes using inverse-distance method
!
! Well GeoZones Loop added 7/1/2019
! Utilizes arrays geonames_lay and ngeozones_lay which list geologic zones
! ("Units") present in each model layer, based upon geologic node zones
! -LS
!-----------------------------------------------------------------------------!

    real                     :: sum1, sum2, weight, dcol
    real,allocatable         :: khc(:,:), khf(:,:), kvc(:,:), kvf(:,:), pc(:)
    integer                  :: iwell, ilay, inode, igeo, wcount, count
    real,allocatable         :: wellxy(:,:), wellv(:),temppc(:)
    type(tree_master_record),pointer :: welltree
    character(30),allocatable:: tempzones(:)
    character(60),allocatable:: fmt(:)

!  Avoiding stack overflow by allocating after declaration
    allocate(KhC(nlayers, nnodes), &
             KhF(nlayers, nnodes), &
             KvC(nlayers, nnodes), &
             KvF(nlayers, nnodes), &
             pc(nnodes))
    allocate(wellxy(nwell,2), wellv(nwell),temppc(nnodes))
    allocate(tempzones(nnodes),fmt(2))
    
    if (USE_IDW == 0) then
      ! Calculate percent coarse at each node by kriging well data by layer
      do ilay=1, nlayers
        tempzones = geozones(1:nnodes,ilay)
        ! Compile wells (x,y,val) that have PC for each layer/geo zone
        ! Use to build kd-tree for kriging subroutine
        do igeo=1, ngeozones_lay(ilay)
          wcount = 0
          do iwell=1, nwell
            if ((pcwellelem(iwell, ilay) >= 0.0d0).and.&
                (wellgeozones(iwell,ilay) == geonames_lay(igeo,ilay))) then
              wcount = wcount + 1
              wellv(wcount) = pcwellelem(iwell, ilay)
              wellxy(wcount,1) = xwell(iwell)
              wellxy(wcount,2) = ywell(iwell)
            end if
          end do
          if (wcount >= pckrige_nwells) then
            welltree => create_tree(wellxy(1:wcount,:))
            ! Pass well tree to kriging routine
            ! Will ignore nodes not in the target group
            call spkrige_tree_group(nnodes, nodex, nodey, welltree, nwell, &
                              wellv, temppc, pckrige_nwells, &
                              tempzones, geonames_lay(igeo,ilay))
          else if (wcount > 0) then
            ! If here, wcount was less than pckrige_nwells but greater than 0
            write(*,'(4x,a,i8,3a,i3,a)') 'Warning - Layer', ilay, ' - ', &
                     trim(geonames_lay(igeo,ilay)), ', only ', wcount, ' wells'
            ! (warns even through it calls the same subroutine)
            welltree => create_tree(wellxy(1:wcount,:))
            call spkrige_tree_group(nnodes, nodex, nodey, welltree, nwell, &
                              wellv, temppc, pckrige_nwells, &
                              tempzones, geonames_lay(igeo,ilay)) 
          else
            write(*,'(4x,a,i8,2a)') 'Warning - No well data in layer ', ilay, &
                                   'for geologic zone', geonames_lay(igeo,ilay)
            ! Only affect one zone
            where (tempzones == geonames_lay(igeo,ilay)) temppc = NODATA
          end if
        end do
        ! Copy over
        PcNode(ilay,1:nnodes) = temppc
      end do
    else  ! i.e. USE_IDW == 1
      ! Calculate percent coarse for each node based on normalized inverse-distance
      write(*,'(4x,a)') 'Calculating inverse-distance well-node weights'
      do ilay = 1, nlayers
        write(*,'(4x,a,i2)') 'Processing layer ', ilay
        do inode = 1, nnodes
          sum1 = 0.0
          sum2 = 0.0
          count = 0            ! LS Added to avoid "no well data" when PC is 0.0 from data
          do iwell = 1, nwell
            if ((PcWellElem(iwell, ilay) >= 0).and.&
                       (geozones(inode,ilay) == wellgeozones(iwell,ilay))) then
              count = count + 1
              weight = (((Xwell(Iwell) - nodex(inode)) ** 2 &
                       + (Ywell(Iwell) - nodey(inode)) ** 2) ** 0.5) ** 2
              weight = 1 / weight
              sum1 = sum1 + weight * PcWellElem(iwell, ilay)
              sum2 = sum2 + weight
            end if
          end do
          if (count > 0) then                               ! LS sum2 > 0 
            PcNode(ilay, inode) = sum1 / sum2
          else
            PcNode(ilay, inode) = -9999.0
            write(*,'(4x,a,i8)') 'No well data for node ', inode
          end if
        end do
      end do
    end if
    
! Use input values to calculate aquifer parameters for each node
    !write(*,'(4x,a)') 'Calculating element aquifer parameters'
    do ilay = 1, nlayers
      do inode = 1, nnodes
        Dcol = elevation(inode) - ((topelev(inode,ilay) + botelev(inode, ilay)) / 2.0)
        if (PcNode(ilay, inode) >= 0.0d0) then
            KhC(ilay, inode) = nKCMin(inode) + (dKC(inode) * &
                               exp(-1.0 * KCk * Dcol))
            KhF(ilay, inode) = nKFMin(inode) + (dKF(inode) * &
                               exp(-1.0 * KFk * Dcol))
            KvC(ilay, inode) = KhC(ilay, inode) / nAnisoC(inode)
            KvF(ilay, inode) = KhF(ilay, inode) / nAnisoF(inode)
            KhB(ilay, inode) = ((PcNode(ilay, inode) * &
                                (KhC(ilay, inode) ** KHp)) + &
                                ((1 - PcNode(ilay, inode)) * &
                                 (KhF(ilay, inode) ** KHp))) ** (1.0 / KHp)
            KvB(ilay, inode) = ((PcNode(ilay, inode) * &
                                (KvC(ilay, inode) ** KVp)) + &
                                ((1 - PcNode(ilay, inode)) * &
                                 (KvF(ilay, inode) ** KVp))) ** (1.0 / KVp)
! SsB has been modified so that
! SsC = SskeC + SskvC
! SsF = SskeF + SskvF
! -LS 9/7/2017
            SsB(ilay, inode) = (PcNode(ilay, inode) * (nSsC(inode))) + &
                               ((1 - PcNode(ilay, inode)) * (nSsF(inode)))
            SyB(ilay, inode) = ((PcNode(ilay, inode) * (nSyC(inode) ** Syp)) + &
                                ((1 - PcNode(ilay, inode)) * (nSyF(inode) ** &
                                  Syp))) ** (1.0 / Syp)

! Removing these, not used in model - LS 9/7/2017
!            SskeB(ilay, inode) = (PcNode(ilay, inode) * SskeC) + &
!                                 ((1 - PcNode(ilay, inode)) * SskeF)
!            SskvB(ilay, inode) = (PcNode(ilay, inode) * SskvC) + &
!                                 ((1 - PcNode(ilay, inode)) * SskvF)
        else
            KhB(ilay, inode) = NODATA
            KvB(ilay, inode) = NODATA
            SsB(ilay, inode) = NODATA
            SyB(ilay, inode) = NODATA
!            SskeB(ilay, inode) = NODATA
!            SskvB(ilay, inode) = NODATA
        end if
      end do
    end do
    
    ! Deallocate
    deallocate(KhC, KhF, KvC, KvF, pc)
    deallocate(wellxy, wellv, temppc)
    deallocate(tempzones,fmt)

end subroutine interpolate
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
SUBROUTINE interpolate_aqtard()
  use Krige
  use kd_tree
    implicit none
!-----------------------------------------------------------------------------!
! Interpolates from wells to nodes using inverse-distance method
!-----------------------------------------------------------------------------!

    real                     :: sum1, sum2, weight, dcol
    real,allocatable         :: khc(:,:), khf(:,:), kvc(:,:), kvf(:,:), pc(:)
    integer                  :: iwell, ilay, inode, wcount, count
    real,allocatable         :: wellxy(:,:), wellv(:)
    character(60),allocatable:: fmt(:)
    type(tree_master_record),pointer :: welltree
    
    allocate(KhC(nlayers, nnodes), &
             KhF(nlayers, nnodes), &
             KvC(nlayers, nnodes), &
             KvF(nlayers, nnodes), &
             pc(nnodes))
    allocate(wellxy(nwell,2), wellv(nwell))
    allocate(fmt(2))
    if (USE_IDW == 0) then
      ! Calculate percent coarse at each node by kriging well data by layer
      do ilay=1, nlayers
        ! Get wells (x,y,val) that have PC for this layer to build kd-tree
        wcount = 0
        do iwell=1, nwell
          if (PcWellAqtardElem(iwell, ilay) >= 0.0d0) then
            wcount = wcount + 1
            wellv(wcount) = PcWellAqtardElem(iwell, ilay)
            wellxy(wcount,1) = xwell(iwell)
            wellxy(wcount,2) = ywell(iwell)
          end if
        end do
      
        ! Only build tree & krige if there's data for the layer
        if (wcount > 0) then
          welltree => create_tree(wellxy(1:wcount,:))

          call spkrige_tree(nnodes, nodex, nodey, welltree, nwell,  wellv, &
                            pc, pckrige_nwells)
          PcNode_aqtard(ilay,1:nnodes) = pc(1:nnodes)
        else
          ! Check for layer thickness
          do inode = 1, nnodes
            if ((aqtardtopelev(inode, ilay) - aqtardbotelev(inode, ilay)) > 1e-6) then
              ! No wells in layer, despite having thickness
              pcnode_aqtard(ilay, inode) = NODATA
            else
              ! No thickness, no problem. PC is 0.0
              pcnode_aqtard(ilay, inode) = 0.0d0
            end if
          end do
          if (minval(pcnode_aqtard(ilay,:))<0.0d0) then
            ! Need to warn if NODATA values were written 
            write(*,'(4x,a,i8)') 'No well data for aquitard layer ', ilay
          end if
        end if
      end do
    else
      ! Calculate percent course for each node based on normalized inverse-distance
      write(*,'(4x,a)') 'Calculating inverse-distance well-node weights'
      do ilay = 1, nlayers
  !      write(*,'(4x,a,i2)') 'Processing layer ', ilay
        do inode = 1, nnodes
          ! Check to make sure there is aquifer thickness
          if ((aqtardtopelev(inode, ilay) - aqtardbotelev(inode, ilay)) > 1e-5) then
            sum1 = 0.0
            sum2 = 0.0
            count = 0
            do iwell = 1, nwell
              if (PcWellAqtardElem(iwell, ilay) > -1e-5) then                   ! PcWellAqtardElem is -9999 if blank
                count = count + 1
                weight = (((Xwell(Iwell) - nodex(inode)) ** 2 &
                         + (Ywell(Iwell) - nodey(inode)) ** 2) ** 0.5) ** 2
                weight = 1 / weight
                sum1 = sum1 + weight * PcWellAqtardElem(iwell, ilay)
                sum2 = sum2 + weight
              end if
            end do
            if (count > 0) then
              pcnode_aqtard(ilay, inode) = sum1 / sum2
            else
              pcnode_aqtard(ilay, inode) = -9999.0
              write(*,'(4x,a,i8)') 'No well data for node ', inode
            end if
          else
            ! No thickness - no error, just zero
            pcnode_aqtard(ilay, inode) = 0.0
          end if
        end do
      end do
    end if

! Use input values to calculate aquifer parameters for each node
    !write(*,'(4x,a)') 'Calculating element aquitard parameters'
    do ilay = 1, nlayers
      do inode = 1, nnodes
        Dcol = elevation(inode) - ((aqtardtopelev(inode,ilay) + aqtardbotelev(inode, ilay)) / 2.0)
        if (pcnode_aqtard(ilay, inode) >= 0.0d0) then
            KhC(ilay, inode) = atnKCMin(inode) + (atdKC(inode) * &
                               exp(-1.0 * KCk * Dcol))
            KhF(ilay, inode) = atnKFMin(inode) + (atdKF(inode) * &
                               exp(-1.0 * KFk * Dcol))
            KvC(ilay, inode) = KhC(ilay, inode) / atnAnisoC(inode)
            KvF(ilay, inode) = KhF(ilay, inode) / atnAnisoF(inode)
            if ((aqtardtopelev(inode, ilay) - aqtardbotelev(inode, ilay)) > 1e-5) then
              ! Has thickness
              KvB_aqtard(ilay, inode) = ((pcnode_aqtard(ilay, inode) * &
                                  (KvC(ilay, inode) ** KVp)) + &
                                  ((1 - pcnode_aqtard(ilay, inode)) * &
                                   (KvF(ilay, inode) ** KVp))) ** (1.0 / KVp)
            else
              ! No Thickness
              KvB_aqtard(ilay, inode) = 0.0d0
            end if
        else
            KvB_aqtard(ilay, inode) = NODATA
        end if
      end do
    end do
    
    ! Deallocate
    deallocate(KhC, KhF, KvC, KvF)
    deallocate(wellxy, wellv)
    deallocate(fmt)

end subroutine interpolate_aqtard
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! - IWFM KD-tree related subroutines
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!

subroutine create_nodetree()
  use kd_tree
  implicit none
  
  integer                  :: i
  
  ! Reassignment into a single xy array
  ! memory usage could be improved by modifying KDTree
  do i=1, nnodes
    nodexy(i,1) = nodex(i)
    nodexy(i,2) = nodey(i)
  end do
  
  nodetree => create_tree(nodexy)
  
end subroutine create_nodetree

!-----------------------------------------------------------------------------!

integer function get_well_element(x, y, iwell, closenodes, nnear) result(element)
  implicit none
  
  integer                  :: i, j, inode, elem, points, count, node
  integer, intent(in)      :: nnear, iwell, closenodes(nnear)
  real, intent(in)         :: x, y
  real                     :: elemx(4), elemy(4), inelem, pinpol
  
  element = 0
  
  ! Loop over elements of closest node
outerloop:  do inode=1, nnear
    node = closenodes(inode)
    do i=1, nnodeelem(node)
      elem = nodeelem(node,i)
      count = 0
      ! Get Element node x,y coordinates
      do j=1, 4
        if (elements(elem,j) > 0) then
          elemx(j) = nodex(elements(elem,j))
          elemy(j) = nodey(elements(elem,j))
          count = count + 1
        end if
      end do
      ! Check if the point is in this polygon
      inelem = pinpol(x, y, elemx, elemy, count)
      if (inelem >= 0.0d0) then             ! Greedy, treats on line as in elem
        element = elem
        exit outerloop
      end if
    end do
  end do outerloop
  
  if (element == 0) then
    write(*,'(4x,a)') 'ERROR - Well not inside element. Likely located out of model.'
    write(*,'(4x,a8,2a18)') 'WellNum', 'X', 'Y'
    write(*,'(4x,i8,2f18.5)') iwell, x, y
    stop
  end if
  
  return
    
end function get_well_element

!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! - Extra Subroutines
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine calc_interbed_thickness()
  implicit none
  
  ! Estimates interbed thickness as percent fine * layer thickness
  ! Added by Leland Scantlebury July 2019
  !
  ! For IWFM, layer thickness is directly read in in variable thick
  ! However, for MODFLOW support we're just going to use the layer top/bot
  
  integer                  :: ilay, inode
  real                     :: laythick
  
  allocate(interbed_thick(nlayers,nnodes))
  
  do ilay=1, nlayers
    do inode=1, nnodes
      laythick = topelev(inode,ilay) - botelev(inode, ilay)
      interbed_thick(ilay,inode) = (1 - pcnode(ilay,inode)) * laythick 
    end do
  end do
  
end subroutine calc_interbed_thickness
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine colocate_corrector(xcoords, ycoords, ncoords)
implicit none
! Checks for duplicate X,Y values and moves them away from each other.
!
! Added by Leland Scantlebury July 2019
  integer, intent(in)     :: ncoords
  real, intent(inout)     :: xcoords(ncoords), ycoords(ncoords)
  integer                 :: i, j, k, ncoloc, max
  real, parameter         :: move=0.01
  
  ! Setup
  max = 1
  
  ! Outer Loop over coordinates
  do i=1, ncoords
    ncoloc = 0
    ! Inner Loop over coordinates to search for pairs
    do j=1, ncoords
      ! Skip the outer loop coordinate
      if (i==j) cycle
      ! Check for x-y match
      if (xcoords(i) == xcoords(j).and.ycoords(i) == ycoords(j)) then
        ! Warn if this is our first match
        if (colocated == .false.) then
          colocated = .true.
          write(*,'(4x,a)') 'Warning - co-located wells. Attempting to move apart for kriging'
        end if
        ! Add to count
        ncoloc = ncoloc + 1
        ! See if greater than max
        if (ncoloc > max) max = ncoloc
        ! Move
        xcoords(j) = xcoords(j) + move * ncoloc
        ycoords(j) = ycoords(j) + move * ncoloc
      end if
    end do
  end do
  
  if (colocated) then
    write(*,'(4x,a,i3,a,f6.3)') 'Max co-located wells:',max,' | Max dist moved:', sqrt(2*(max*move)**2)
  end if
  
end subroutine colocate_corrector
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! Allocation Subroutine(s)
!-----------------------------------------------------------------------------!

subroutine WellAllocate()
  implicit none
  
    allocate(Xwell(nwell), &
             Ywell(nwell), &
             Zland(nwell), &
             Zwell(nwell, maxWPoints), &
             wellelem(nwell), &
             numZone(nwell), &
             PcWellZone(nwell, maxWPoints), &
             PcWellElem(nwell, nlayers), &
             PcWellAqtardElem(nwell,nlayers), &
             wellelevtop(nwell, nlayers), &
             wellelevbot(nwell, nlayers), &
             wellaqtardelevtop(nwell,nlayers), &
             wellaqtardelevbot(nwell,nlayers), &
             wellgeozones(nwell, nlayers))

end subroutine
!-----------------------------------------------------------------------------!
subroutine NodeAllocate()
  implicit none
  
  allocate(PcNode(nlayers, nnodes), &
           pcnode_aqtard(nlayers, nnodes), &
           nKCMin(nnodes), &
           dKC(nnodes), &
           nKFMin(nnodes), &
           dKF(nnodes), &
           nSsC(nnodes), &
           nSsF(nnodes), &
           nSyC(nnodes), &
           nSyF(nnodes), &
           nAnisoC(nnodes), &
           nAnisoF(nnodes), &
           atnKCMin(nnodes), &
           atdKC(nnodes), &
           atnKFMin(nnodes), &
           atdKF(nnodes), &
           atnAnisoC(nnodes), &
           atnAnisoF(nnodes), &
           KhB(nlayers, nnodes), &
           KvB(nlayers, nnodes), &
           SsB(nlayers, nnodes), &
           SyB(nlayers, nnodes), &
           KvB_aqtard(nlayers, nnodes), &
           nodezone(nnodes), &
           geozones(nnodes, nlayers), &
           ngeozones_lay(nlayers), &
           geonames_lay(max_zones,nlayers))

end subroutine 
!-----------------------------------------------------------------------------!

END MODULE MakePar