!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2021  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov 
!***********************************************************************
MODULE Class_TecplotOutput
  USE MessageLogger           , ONLY: EchoProgress
  USE TimeSeriesUtilities     , ONLY: TimeStepType       , &
                                      f_iTimeStampLength
  USE GeneralUtilities        , ONLY: IntToText
  USE IOInterface             , ONLY: GenericFileType
  USE Package_Discretization  , ONLY: AppGridType
  IMPLICIT NONE
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: TecplotOutputType                 
  
  
  ! -------------------------------------------------------------
  ! --- TECPLOT OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE TecplotOutputType
      PRIVATE
      TYPE(GenericFileType) :: OutFile
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: PrintInitialValues  
      PROCEDURE,PASS :: PrintResults
  END TYPE TecplotOutputType
  
  
  
  
CONTAINS
    
    

    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE TECPLOT OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(TecplotOut,IsForInquiry,cFileName,cDescriptor,iStat)
    CLASS(TecplotOutputType)    :: TecplotOut
    LOGICAL,INTENT(IN)          :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cDescriptor
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('   Instantiating '//TRIM(cDescriptor))
    
    !Open file
    IF (IsForInquiry) THEN
        CALL TecplotOut%OutFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor=TRIM(cDescriptor),FileType='TXT',iStat=iStat)
    ELSE
        CALL TecplotOut%OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor=TRIM(cDescriptor),FileType='TXT',iStat=iStat)
    END IF

  END SUBROUTINE New
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CLOSE TECPLOT OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill(TPOutput)
    CLASS(TecplotOutputType) :: TPOutput
    
    CALL TPOutput%OutFile%Kill()
    
  END SUBROUTINE Kill
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT-OUT INITIAL VALUES
  ! -------------------------------------------------------------
  SUBROUTINE PrintInitialValues(TPOutput,AppGrid,StrmConnectivity,rIniValues,Factors,cFormat,cVarNames,TimeStep)
    CLASS(TecplotOutputType)      :: TPOutput
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    COMPLEX,INTENT(IN)            :: StrmConnectivity(:)
    REAL(8),INTENT(IN)            :: rIniValues(:,:),Factors(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cFormat,cVarNames(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    
    !Local variables
    INTEGER   :: indxNode,NStrmNodes,NNodes,NElements,NVertex,Nodes(4),indxElem,indx,indxVar,iDimVar
    CHARACTER :: Header(3)*3000,Text*3000,cZoneName*f_iTimeStampLength
    
    !Initialize
    NNodes     = AppGrid%NNodes
    NElements  = AppGrid%NElements
    NStrmNodes = SIZE(StrmConnectivity)
    iDimVar    = SIZE(cVarNames)
    
    !Prepare zone name
    cZoneName = ZoneName(TimeStep)

    !Prepare and print-out headers
    Header(1) = 'TITLE="IWFM RESULTS VISUALIZATION"'
    Header(2) = 'VARIABLES="X","Y"'
    DO indx=1,SIZE(cVarNames)
      Header(2)=TRIM(Header(2))//',"'//TRIM(cVarNames(indx))//'"'
    END DO
    Header(3)='ZONE T="'//TRIM(cZoneName)//'",N='//TRIM(IntToText(NNodes))//',E='//TRIM(IntToText(NElements+NStrmNodes))//',DATAPACKING=POINT,ZONETYPE=FEQUADRILATERAL,STRANDID=1,SOLUTIONTIME=0.0'
    CALL TPOutput%OutFile%WriteData(Header)
    
    !x-y coordinates and initial conditions
    DO indxNode=1,NNodes
      WRITE (Text,cFormat) AppGrid%X(indxNode),AppGrid%Y(indxNode),(rIniValues(indxNode,indxVar)*Factors(indxVar),indxVar=1,iDimVar)
      CALL TPOutput%OutFile%WriteData(TRIM(Text))
    END DO
    CALL TPOutput%OutFile%WriteData(' ')
    
    !Groundwater node connectivity
    DO indxElem=1,NElements
      NVertex          = AppGrid%NVertex(indxElem)
      Nodes(1:NVertex) = AppGrid%Vertex(1:NVertex,indxElem)
      IF (NVertex .EQ. 3) Nodes(4) = Nodes(3)
      WRITE (Text,'(4(I8,2X))') Nodes
      CALL TPOutput%OutFile%WriteData(Text)
    END DO  
    
    !Stream node connectivity
    DO indx=1,SIZE(StrmConnectivity)
        Nodes(1)   = REAL(StrmConnectivity(indx))
        Nodes(2)   = AIMAG(StrmConnectivity(indx))
        Nodes(3:4) = Nodes(2)
        WRITE (Text,'(4(I8,2X))') Nodes
        CALL TPOutput%OutFile%WriteData(Text)
    END DO
    Text = 'TEXT X=35, Y=70, CS=FRAME, T="'//TRIM(cZoneName)//'", ZN=1'
    CALL TPOutput%OutFile%WriteData(TRIM(Text))
    
  END SUBROUTINE PrintInitialValues
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(TPOutput,rValues,rFactor,TimeStep)
    CLASS(TecplotOutputType)      :: TPOutput
    REAL(8),INTENT(IN)            :: rValues(:,:),rFactor
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    
    !Local variables
    CHARACTER :: Text*3000,cZoneName*f_iTimeStampLength
    INTEGER   :: indx,indx1
    REAL(8)   :: rValuesTemp(SIZE(rValues,DIM=1)*SIZE(rValues,DIM=2))
    
    !Initialize
    cZoneName   = ZoneName(TimeStep)
    rValuesTemp = PACK(rValues , MASK=.TRUE.)

    Text = 'ZONE T="'//TRIM(cZoneName)//'",DATAPACKING=BLOCK,ZONETYPE=FEQUADRILATERAL,VARSHARELIST=([1,2]=1),CONNECTIVITYSHAREZONE=1,STRANDID=1,SOLUTIONTIME='//TRIM(IntToText(TimeStep%CurrentTimeStep+1))//'.0'
    CALL TPOutput%OutFile%WriteData(Text) 
    DO indx=1,SIZE(rValues)/100+1 
      WRITE (Text,'(100(F12.3,2X))') (rValuesTemp(indx1)*rFactor,indx1=(indx-1)*100+1,MIN(indx*100,SIZE(rValues)))
      CALL TPOutput%OutFile%WriteData(Text)
    END DO
    Text = 'TEXT X=35, Y=70, CS=FRAME, T="'//TRIM(cZoneName)//'", ZN='//TRIM(IntToText(TimeStep%CurrentTimeStep+1))
    CALL TPOutput%OutFile%WriteData(TRIM(Text))
   
  END SUBROUTINE PrintResults


  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PREPARE ZONE NAME
  ! -------------------------------------------------------------
  FUNCTION ZoneName(TimeStep) RESULT(cName)
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    CHARACTER(LEN=f_iTimeStampLength) :: cName
    
    !Initialize
    cName = ''
    
    !Prepare zone name
    IF (TimeStep%TrackTime) THEN
        cName = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
        cName = ADJUSTL(IntToText(TimeStep%CurrentTimeStep))
    END IF
    
  END FUNCTION ZoneName
  
END MODULE