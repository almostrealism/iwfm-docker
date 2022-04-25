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
MODULE Class_AsciiFileType
  USE MessageLogger        , ONLY: SetLastMessage                  , &
                                   LogMessage                      , &
                                   f_iWarn                         , &
                                   f_iFatal
  USE GeneralUtilities     , ONLY: GenericString                   , &
                                   GenericString_To_String         , &
                                   f_cLineFeed                     , &
                                   IntToText                       , &
                                   LocateInList                    , &
                                   CleanSpecialCharacters          , &
                                   UpperCase
  USE TimeSeriesUtilities  , ONLY: LeapYearCorrection              , &
                                   StripTimeStamp                  , &
                                   TimeStampToJulian               , &
                                   TimeStampToJulianDateAndMinutes , &
                                   JulianToTimeStamp               , &
                                   IncrementTimeStamp              , &
                                   IsTimeStampValid                , &
                                   AdjustRateTypeData              , &
                                   AdjustTimeStampWithYear4000     , &
                                   SetTSDCacheSize                 , &
                                   f_iRecognizedIntervals_InMinutes, &
                                   f_iTimeStampLength              , &
                                   OPERATOR(.TSGT.)
  USE Class_BaseFileType   , ONLY: BaseFileType                    , &
                                   IsFileOpen
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
  PUBLIC :: AsciiInFileType         , &
            AsciiOutFileType        , &
            AsciiTSDInFileType      , &
            AsciiTSDOutFileType


  ! -------------------------------------------------------------
  ! --- ASCII INPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: AsciiInFileType
      PRIVATE
      INTEGER :: AtLine = 1    !Pointer to the lines in the file
  CONTAINS
      PROCEDURE,PASS :: New                                    => New_AsciiInFile
      PROCEDURE,PASS :: Kill                                   => Kill_AsciiInFile
      PROCEDURE,PASS :: ReadSingleData_AsciiInFile
      PROCEDURE,PASS :: ReadArrayData_AsciiInFile
      PROCEDURE,PASS :: ReadMatrixData_AsciiInFile
      PROCEDURE,PASS :: ReadCharacterArray                     => ReadCharacterUntilComment_AsciiInFile
      PROCEDURE,PASS :: Backspace                              => Backspace_AsciiInFile
      PROCEDURE,PASS :: Rewind                                 => Rewind_AsciiInFile
      GENERIC        :: ReadData                               => ReadSingleData_AsciiInFile             , &
                                                                  ReadArrayData_AsciiInFile              , &
                                                                  ReadMatrixData_AsciiInFile
  END TYPE AsciiInFileType


  ! -------------------------------------------------------------
  ! --- ASCII OUTPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: AsciiOutFileType
      PRIVATE
  CONTAINS
      PROCEDURE,PASS :: New                               => New_AsciiOutFile
      PROCEDURE,PASS :: Kill                              => Kill_AsciiOutFile
      PROCEDURE,PASS :: WriteSingleData_AsciiOutFile
      PROCEDURE,PASS :: WriteArrayData_AsciiOutFile
      PROCEDURE,PASS :: Rewind         => Rewind_AsciiOutFile
      GENERIC        :: WriteData      => WriteSingleData_AsciiOutFile                    , &
                                          WriteArrayData_AsciiOutFile
  END TYPE AsciiOutFileType


  ! -------------------------------------------------------------
  ! --- ASCII TIME SERIES DATA INPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AsciiInFileType) :: AsciiTSDInFileType
      PRIVATE
      INTEGER             :: NSP                       = 1            !Number of time steps to update the time series data
      INTEGER             :: NFQ                       = 0            !Repitition frequency of the time series data; after NFQ times of read actions the file is rewound
      INTEGER             :: TimeStampedData           = 0            !Flag to distinguish if the time series data is time stamped or not (0 = not known; 1 = time stamped ; -1 = non-time stamped)
      INTEGER             :: NumberOfBlocksToSkip      = 1            !Number of data blocks (groups of data lines between comment lines) to skip after rewinding the file to reach the time series data
      INTEGER             :: NumberOfDataLines         = -1           !Number of data lines over which a single time series entry is distributed
      INTEGER             :: NumberOfReads             = 0            !Total number of read actions performed on the file since it was opened or last rewound
      INTEGER             :: NumberOfTimeStepsPassed   = 0            !Number of time steps that passed since the last read action
      INTEGER             :: Interval_InMinutes        = -99          !Data time interval in minutes
      REAL(8)             :: JulianDate1               = -HUGE(0d0)   !Julian time of the time series data that is smaller than the simulation time (used only for time-tracing simulations)
      REAL(8)             :: JulianDate2               = -HUGE(0d0)   !Julian time of the time series data that is greater than the simulation time (used only for time-tracing simulations)
      LOGICAL             :: FileRewound               = .FALSE.      !Flag used when time series data file includes recycling data (e.g. monthly ET used for all years) to avoid stepping into infinite loop
      LOGICAL,ALLOCATABLE :: RateTypeData(:)                          !Flag to specify if the data being read is a rate type data; this information is used to convert the time-series data based on the time step of the simulation)
  CONTAINS
      PROCEDURE,PASS :: Kill => Kill_AsciiTSDInFile
      PROCEDURE,PASS :: SetNSP
      PROCEDURE,PASS :: SetNFQ
      PROCEDURE,PASS :: SetNumberOfBlocksToSkip
      PROCEDURE,PASS :: SetRateTypeData
      PROCEDURE,PASS :: SetDataInterval
      PROCEDURE,PASS :: SetParameters
      PROCEDURE,PASS :: Rewind_To_BeginningOfTSData
      PROCEDURE,PASS :: ReadSingleData_AsciiTSDInFile
      PROCEDURE,PASS :: ReadArrayData_AsciiTSDInFile
      PROCEDURE,PASS :: ReadMatrixData_AsciiTSDInFile
      PROCEDURE,PASS :: ReadTimeSeries_ForTimeRange_AsciiTSDInFile
      GENERIC        :: ReadData                                    => ReadSingleData_AsciiTSDInFile               , &
                                                                       ReadArrayData_AsciiTSDInFile                , &
                                                                       ReadMatrixData_AsciiTSDInFile               , &
                                                                       ReadTimeSeries_ForTimeRange_AsciiTSDInFile
  END TYPE AsciiTSDInFileType


  ! -------------------------------------------------------------
  ! --- ASCII TIME SERIES DATA OUTPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AsciiOutFileType) :: AsciiTSDOutFileType
      PRIVATE
      INTEGER                       :: NumberOfDataBatch      = 0   !Number of batch of data to be stored
      INTEGER                       :: NumberOfDataRows       = 0   !Number of data rows to be stored
      INTEGER                       :: NumberOfDataColumns    = 0   !Number of data columns to be stored
      INTEGER                       :: DataPointer            = 0   !Pointer to keep track of the row number to store values before flushing to the hard drive
      CHARACTER(LEN=500)            :: FormatStatement        = ''  !The format statement that will be used for printing the results
      REAL(8),ALLOCATABLE           :: ValuesForOutput(:,:,:)       !Array that stores the values to be flushed to the hard drive (will have dimensions NumberOfDataBatch x NumberOfDataRows x NumberOfDataColumns)
      CHARACTER(LEN=21),ALLOCATABLE :: TimeArray(:)                 !Array that stores the simulation time of values to be flushed to the hard drive
  CONTAINS
      PROCEDURE,PASS :: Kill                         => Kill_AsciiTSDOutFile
      PROCEDURE,PASS :: SetCacheSize                 => SetTSDCacheSize_AsciiOutFile
      PROCEDURE,PASS :: SetFormatStatement
      PROCEDURE,PASS :: WriteArrayData_AsciiTSDOutFile
      PROCEDURE,PASS :: WriteMatrixData_AsciiTSDOutFile
      GENERIC        :: WriteData                    => WriteArrayData_AsciiTSDOutFile                  , &
                                                        WriteMatrixData_AsciiTSDOutFile
  END TYPE AsciiTSDOutFileType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  CHARACTER(LEN=3),PARAMETER          :: f_cCommentIndicators = 'Cc*'
  INTEGER,PARAMETER                   :: ModNameLen               = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName                  = 'Class_AsciiFileType::'




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMMON ASCII FILE CONSTRUCTOR
  ! -------------------------------------------------------------
  SUBROUTINE New_AsciiFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(BaseFileType)          :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)  :: FileName
    LOGICAL,INTENT(IN)           :: lInputFile
    CHARACTER(LEN=*),INTENT(IN)  :: AccessType
    INTEGER,OPTIONAL,INTENT(OUT) :: FileOpenCode
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'New_AsciiFile'
    CHARACTER                              :: StatusJargon*7,ActionJargon*9,AccessJargon*10,cErrMessage*500
    INTEGER                                :: ErrorCode

    !Initialize
    iStat = 0
    IF (PRESENT(FileOpenCode)) FileOpenCode = 0

    !Instantiate the base file
    CALL ThisFile%NewBaseFile(FileName)

    !Check if the file is already open
    IF (IsFileOpen(ThisFile%Name)) THEN
        IF (PRESENT(FileOpenCode)) THEN
            FileOpenCode = -1
            RETURN
        ELSE
            CALL SetLastMessage('Error in opening file! File '//TRIM(ThisFile%Name)//' is already open!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

    !Open file
    SELECT CASE (lInputFile)
      CASE (.TRUE.)
        StatusJargon = 'OLD'
        ActionJargon = 'READ'
        AccessJargon = 'SEQUENTIAL'
      CASE (.FALSE.)
        StatusJargon = 'UNKNOWN'
        ActionJargon = 'READWRITE'
        AccessJargon = AccessType
    END SELECT
    OPEN (UNIT=ThisFile%UnitN,FILE=TRIM(ThisFile%Name),FORM='FORMATTED',STATUS=StatusJargon,ACTION=ActionJargon,ACCESS=TRIM(AccessJargon),IOSTAT=ErrorCode,IOMSG=cErrMessage)

    !Implement actions based on error code
    IF (ErrorCode .NE. 0) THEN
        IF (PRESENT(FileOpenCode)) THEN
            FileOpenCode = ErrorCode
            RETURN
        ELSE
            CALL SetLastMessage('Error in opening file '//TRIM(ThisFile%Name)//'!'//f_cLineFeed//TRIM(cErrMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

  END SUBROUTINE New_AsciiFile


  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR FOR ASCII INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_AsciiInFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(AsciiInFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Not used
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Instantiate the file
    IF (PRESENT(FileOpenCode)) THEN
        CALL New_AsciiFile(ThisFile,FileName,lInputFile,'SEQUENTIAL',FileOpenCode=FileOpenCode,iStat=iStat)
    ELSE
        CALL New_AsciiFile(ThisFile,FileName,lInputFile,'SEQUENTIAL',iStat=iStat)
    END IF

  END SUBROUTINE New_AsciiInFile


  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR FOR ASCII OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_AsciiOutFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
    CLASS(AsciiOutFileType)              :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Must be either 'SEQUENTIAL' or 'APPEND'
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'New_AsciiOutFile'
    CHARACTER                              :: LocalAccessType*10

    !Initialize
    IF (PRESENT(AccessType)) THEN
        LocalAccessType = AccessType
    ELSE
        LocalAccessType = 'SEQUENTIAL'
    END IF

    !Instantiate the file
    IF (PRESENT(FileOpenCode)) THEN
        CALL New_AsciiFile(ThisFile,FileName,lInputFile,LocalAccessType,FileOpenCode=FileOpenCode,iStat=iStat)
    ELSE
        CALL New_AsciiFile(ThisFile,FileName,lInputFile,LocalAccessType,iStat=iStat)
    END IF

  END SUBROUTINE New_AsciiOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMMON ASCII FILE DESTRUCTOR
  ! -------------------------------------------------------------
  SUBROUTINE Kill_AsciiFile(ThisFile,Status)
    CLASS(BaseFileType)                  :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'Kill_AsciiFile'
    INTEGER   :: iIteration,ErrorCode
    CHARACTER :: LocalStatus*6,cErrMessage*500

    IF (PRESENT(Status)) THEN
        LocalStatus = Status
    ELSE
        LocalStatus = 'KEEP'
    END IF

    !Try to close file 3 times (in certain cases the file is not closed with one try)
    DO iIteration=1,3
        !Close file
        CLOSE(ThisFile%UnitN,IOSTAT=ErrorCode,STATUS=LocalStatus,IOMSG=cErrMessage)

        !Exit if succesful
        IF (ErrorCode .EQ. 0) EXIT

    END DO

  END SUBROUTINE Kill_AsciiFile


  ! -------------------------------------------------------------
  ! --- KILL ASCII INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_AsciiInFile(ThisFile,Status)
    CLASS(AsciiInFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    TYPE(AsciiInFileType) :: Dummy

    IF (PRESENT(Status)) THEN
        CALL Kill_AsciiFile(ThisFile,Status)
    ELSE
        CALL Kill_AsciiFile(ThisFile)
    END IF

    ThisFile%AtLine = Dummy%AtLine

  END SUBROUTINE Kill_AsciiInFile


  ! -------------------------------------------------------------
  ! --- KILL ASCII OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_AsciiOutFile(ThisFile,Status)
    CLASS(AsciiOutFileType)              :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    IF (PRESENT(Status)) THEN
        CALL Kill_AsciiFile(ThisFile,Status)
    ELSE
        CALL Kill_AsciiFile(ThisFile)
    END IF

  END SUBROUTINE Kill_AsciiOutFile


  ! -------------------------------------------------------------
  ! --- KILL ASCII TIME-SERIES INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_AsciiTSDInFile(ThisFile,Status)
    CLASS(AsciiTSDInFileType)            :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(AsciiTSDInFileType) :: Dummy

    !Kill the parent class
    IF (PRESENT(Status)) THEN
        CALL Kill_AsciiInFile(ThisFile,Status)
    ELSE
        CALL Kill_AsciiInFile(ThisFile)
    END IF

    !Set the attributes to their default values
    ThisFile%NSP                     = Dummy%NSP
    ThisFile%NFQ                     = Dummy%NFQ
    ThisFile%TimeStampedData         = Dummy%TimeStampedData
    ThisFile%NumberOfBlocksToSkip    = Dummy%NumberOfBlocksToSkip
    ThisFile%NumberOfDataLines       = Dummy%NumberOfDataLines
    ThisFile%NumberOfReads           = Dummy%NumberOfReads
    ThisFile%NumberOfTimeStepsPassed = Dummy%NumberOfTimeStepsPassed
    ThisFile%Interval_InMinutes      = Dummy%Interval_InMinutes
    ThisFile%JulianDate1             = Dummy%JulianDate1
    ThisFile%JulianDate2             = Dummy%JulianDate2
    ThisFile%FileRewound             = Dummy%FileRewound

    !Deallocate array
    DEALLOCATE (ThisFile%RateTypeData ,STAT=ErrorCode)

  END SUBROUTINE Kill_AsciiTSDInFile


  ! -------------------------------------------------------------
  ! --- KILL ASCII TIME-SERIES OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_AsciiTSDOutFile(ThisFile,Status)
    CLASS(AsciiTSDOutFileType)           :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    INTEGER                   :: ErrorCode
    TYPE(AsciiTSDOutFileType) :: Dummy

    !Kill the parent class
    IF (PRESENT(Status)) THEN
        CALL Kill_AsciiOutFile(ThisFile,Status)
    ELSE
        CALL Kill_AsciiOutFile(ThisFile)
    END IF

    !Set the attributes to their default values
    ThisFile%NumberOfDataBatch   = Dummy%NumberOfDataBatch
    ThisFile%NumberOfDataRows    = Dummy%NumberOfDataRows
    ThisFile%NumberOfDataColumns = Dummy%NumberOfDataColumns
    ThisFile%DataPointer         = Dummy%DataPointer
    ThisFile%FormatStatement     = Dummy%FormatStatement

    !Deallocate arrays
    DEALLOCATE (ThisFile%ValuesForOutput , ThisFile%TimeArray , STAT=ErrorCode)

  END SUBROUTINE Kill_AsciiTSDOutFile




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET THE NSP VARIABLE FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetNSP(ThisFile,ValueOfNSP)
    CLASS(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(IN)        :: ValueOfNSP

    !Set the value of NSP variable
    ThisFile%NSP = ValueOfNSP

    !Also set the NumberOfTimeStepsPassed equal to NSP so that a read action will be taken at the first time step
    ThisFile%NumberOfTimeStepsPassed = ValueOfNSP

  END SUBROUTINE SetNSP


  ! -------------------------------------------------------------
  ! --- SET THE NFQ VARIABLE FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetNFQ(ThisFile,ValueOfNFQ)
    CLASS(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(IN)        :: ValueOfNFQ

    !Set the value of NFQ variable
    ThisFile%NFQ = ValueOfNFQ

  END SUBROUTINE SetNFQ


  ! -------------------------------------------------------------
  ! --- SET THE NUMBER OF DATA BLOCKS TO SKIP AT EVERY REWIND FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetNumberOfBlocksToSkip(ThisFile,BlocksToSkip)
    CLASS(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(IN)        :: BlocksToSkip

    !Set the number of blocks to skip
    ThisFile%NumberOfBlocksToSkip = BlocksToSkip

  END SUBROUTINE SetNumberOfBlocksToSkip


  ! -------------------------------------------------------------
  ! --- SET THE RateTypeData FLAG FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetRateTypeData(ThisFile,RateTypeData)
    CLASS(AsciiTSDInFileType) :: ThisFile
    LOGICAL,INTENT(IN)        :: RateTypeData(:)

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'SetRateTypeData'
    INTEGER                                :: ErrorCode

    !Set the flag
    ALLOCATE(ThisFile%RateTypeData(SIZE(RateTypeData)),STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL LogMessage('Error in allocating memory for the rate-type-data array for file '//TRIM(ThisFile%Name)//'!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    ThisFile%RateTypeData = RateTypeData

  END SUBROUTINE SetRateTypeData


  ! -------------------------------------------------------------
  ! --- SET THE Interval_InMinutes VARIABLE FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetDataInterval(ThisFile,Interval)
    CLASS(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(IN)        :: Interval

    !Set the data interval
    ThisFile%Interval_InMinutes = Interval

  END SUBROUTINE SetDataInterval


  ! -------------------------------------------------------------
  ! --- SET ALL THE DATA FIELDS FOR ASCII TSD INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetParameters(ThisFile,BlocksToSkip,RateTypeData,ValueOfNSP,ValueOfNFQ)
    CLASS(AsciiTSDInFileType)   :: ThisFile
    INTEGER,INTENT(IN)          :: BlocksToSkip
    LOGICAL,INTENT(IN)          :: RateTypeData(:)
    INTEGER,OPTIONAL,INTENT(IN) :: ValueOfNSP,ValueOfNFQ

    !Set the NumberOfBlocksToSkip
    CALL SetNumberOfBlocksToSkip(ThisFile,BlocksToSkip)

    !Set the rate type data flag
    CALL SetRateTypeData(ThisFile,RateTypeData)

    !Set NSP variable
    IF (PRESENT(ValueOfNSP)) CALL SetNSP(ThisFile,ValueOfNSP)

    !Set NFQ variable
    IF (PRESENT(ValueOfNFQ)) CALL SetNFQ(ThisFile,ValueOfNFQ)

  END SUBROUTINE SetParameters


  ! -------------------------------------------------------------
  ! --- SET THE NumberOfDataBatch, NumberOfDataRows AND NumberOfDataColumns FOR ASCII TSD OUTPUT FILES
  ! -------------------------------------------------------------
  SUBROUTINE SetTSDCacheSize_AsciiOutFile(ThisFile,NColumnsOfData,NRowsOfData)
    CLASS(AsciiTSDOutFileType)  :: ThisFile
    INTEGER,INTENT(IN)          :: NColumnsOfData
    INTEGER,OPTIONAL,INTENT(IN) :: NRowsOfData

    !Local variables
    INTEGER :: nrows

    !Initialize variables
    IF (PRESENT(NRowsOfData)) THEN
        nrows = NRowsOfData
    ELSE
        nrows = 1
    END IF

    !Allocate memory for the storage of time series results
    CALL SetTSDCacheSize(ThisFile%Name                , &
                         ThisFile%ValuesForOutput     , &
                         ThisFile%NumberOfDataBatch   , &
                         ThisFile%NumberOfDataRows    , &
                         ThisFile%NumberOfDataColumns , &
                         NColumnsOfData               , &
                         nrows                        , &
                         ThisFile%TimeArray           )

  END SUBROUTINE SetTSDCacheSize_AsciiOutFile


  ! -------------------------------------------------------------
  ! --- SET THE FormatStatement FOR ASCII TSD OUTPUT FILES
  ! -------------------------------------------------------------
  SUBROUTINE SetFormatStatement(ThisFile,FormatSpec,iStat)
    CLASS(AsciiTSDOutFileType)  :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: FormatSpec
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'SetFormatStatement'

    !Initialize
    iStat = 0

    !Make sure that FormatSpec is not longer than the size of FormatStatement field
    IF (LEN(FormatSpec) .GT. LEN(ThisFile%FormatStatement)) THEN
        CALL SetLastMessage('The format statement specified for file '//TRIM(ThisFile%Name)//' is too long!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set the data field
    ThisFile%FormatStatement = FormatSpec

  END SUBROUTINE SetFormatStatement




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ SINGLE DATA FROM ASCII FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadSingleData_AsciiInFile(ThisFile,Data,Status,Skip,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CLASS(*),INTENT(OUT)         :: Data
    INTEGER,INTENT(OUT),OPTIONAL :: Status
    LOGICAL,INTENT(IN),OPTIONAL  :: Skip
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = ModName // 'ReadSingleData_AsciiInFile'
    INTEGER                                :: ErrorCode

    !Initialize
    iStat = 0

    !Skip comment lines, if asked for
    IF (PRESENT(Skip)) THEN
        IF (Skip) CALL SkipComment(ThisFile,iStat)
    ELSE
        CALL SkipComment(ThisFile,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    SELECT TYPE(Data)
        TYPE IS (CHARACTER(LEN=*))
            READ (ThisFile%UnitN,'(A)',IOSTAT=ErrorCode) Data
            IF (ErrorCode .NE. 0) THEN
                IF (.NOT. PRESENT(Status)) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
            END IF


        TYPE IS (REAL(8))
            READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) Data
            IF (ErrorCode .NE. 0) THEN
                IF (.NOT. PRESENT(Status)) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
            END IF


        TYPE IS (INTEGER)
            READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) Data
            IF (ErrorCode .NE. 0) THEN
                IF (.NOT. PRESENT(Status)) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
            END IF


        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

    IF (PRESENT(Status)) Status = ErrorCode
    IF (iStat .EQ. -1) RETURN

    ThisFile%AtLine = ThisFile%AtLine + 1

  END SUBROUTINE ReadSingleData_AsciiInFile


  ! -------------------------------------------------------------
  ! --- READ ARRAY DATA FROM ASCII FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadArrayData_AsciiInFile(ThisFile,Data,Status,Skip,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CLASS(*),INTENT(OUT)         :: Data(:)
    INTEGER,INTENT(OUT),OPTIONAL :: Status
    LOGICAL,INTENT(IN),OPTIONAL  :: Skip
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ReadArrayData_AsciiInFile'
    CHARACTER                              :: InitialDataLine*30000
    INTEGER                                :: ErrorCode

    !Initialize
    iStat = 0

    !Return if the size of the array is zero
    IF (SIZE(Data) .EQ. 0) RETURN

    !Initialize
    IF (PRESENT(Skip)) THEN
        IF (Skip) CALL SkipComment(ThisFile,iStat)
    ELSE
        CALL SkipComment(ThisFile,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Mark the initial data line for reference to count the number of data lines used
    CALL MarkInitialDataLine(ThisFile,InitialDataLine,iStat)
    IF (iStat .EQ. -1) RETURN

    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) Data


        TYPE IS (INTEGER)
            READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) Data


        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

    !Process error code
    IF (PRESENT(Status)) Status = ErrorCode
    IF (ErrorCode .NE. 0) THEN
        IF (.NOT. PRESENT(Status)) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Adjust the line counter
    CALL FindNumberOfDataLines(ThisFile,InitialDataLine,1,iStat=iStat)

  END SUBROUTINE ReadArrayData_AsciiInFile


  ! -------------------------------------------------------------
  ! --- READ MATRIX DATA FROM ASCII FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadMatrixData_AsciiInFile(ThisFile,Data,Status,Skip,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CLASS(*),INTENT(OUT)         :: Data(:,:)
    INTEGER,INTENT(OUT),OPTIONAL :: Status
    LOGICAL,INTENT(IN),OPTIONAL  :: Skip
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = 'ReadMatrixData_AsciiInFile'
    INTEGER                                :: nRow,rowIndx,ErrorCode,nCol
    CHARACTER                              :: InitialDataLine*30000
    REAL(8),ALLOCATABLE                    :: rData(:)
    INTEGER,ALLOCATABLE                    :: iData(:)

    !Initialize
    iStat = 0
    nRow  = SIZE(Data,DIM=1)
    nCol  = SIZE(Data,DIM=2)

    !Return if any of the dimensions is zero
    IF (nRow .EQ. 0) RETURN
    IF (SIZE(Data,DIM=2) .EQ. 0) RETURN

    !Skip comments
    IF (PRESENT(Skip)) THEN
        IF (Skip) CALL SkipComment(ThisFile,iStat)
    ELSE
        CALL SkipComment(ThisFile,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN

    !Mark the initial data line for reference to count the number of data lines used
    CALL MarkInitialDataLine(ThisFile,InitialDataLine,iStat)
    IF (iStat .EQ. -1) RETURN

    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            ALLOCATE (rData(nCol))
            DO rowIndx=1,nRow
                READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) rData
                Data(rowIndx,:) = rData
                IF (ErrorCode .NE. 0) THEN
                    IF (PRESENT(Status)) THEN
                        Status = ErrorCode
                        RETURN
                    ELSE
                        CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
                    END IF
                END IF
            END DO
            DEALLOCATE (rData)


        TYPE IS (INTEGER)
            ALLOCATE (iData(nCol))
            DO rowIndx=1,nRow
                READ (ThisFile%UnitN,*,IOSTAT=ErrorCode) iData
                Data(rowIndx,:) = iData
                IF (ErrorCode .NE. 0) THEN
                    IF (PRESENT(Status)) THEN
                        Status = ErrorCode
                        RETURN
                    ELSE
                        CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
                    END IF
                END IF
            END DO
            DEALLOCATE (iData)


        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

    IF (PRESENT(Status)) Status = ErrorCode
    IF (iStat .EQ. -1) RETURN

    !Adjust the line counter
    CALL FindNumberOfDataLines(ThisFile,InitialDataLine,nRow,iStat=iStat)

  END SUBROUTINE ReadMatrixData_AsciiInFile


  ! -------------------------------------------------------------
  ! --- READ CHARACTER DATA FROM A FILE UNTIL COMMENT LINE IS REACHED
  ! -------------------------------------------------------------
  SUBROUTINE ReadCharacterUntilComment_AsciiInFile(ThisFile,CharacterData,iStat)
    CLASS(AsciiInFileType)                   :: ThisFile
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: CharacterData(:)
    INTEGER,INTENT(OUT)                      :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'ReadCharacterUntilComment_AsciiInFile'
    INTEGER                                :: NumberOfLinesToRead,indx,ErrorCode
    CHARACTER(LEN=1)                       :: ACharacter

    !Initialize variables
    iStat               = 0
    NumberOfLinesToRead = 0
    IF (ALLOCATED(CharacterData)) DEALLOCATE(CharacterData)

    !First skip comments to reach the beginning of the data lines
    CALL SkipComment(ThisFile,iStat)
    IF (iStat .EQ. -1) RETURN

    !Determine how many lines of data lines there is between comment lines
    DO
      READ (ThisFile%UnitN,'(A)',IOSTAT=ErrorCode) ACharacter
      IF (SCAN(f_cCommentIndicators,ACharacter).NE.0 .OR. ErrorCode.EQ.-1) THEN  !Exit DO loop if a comment line or en-of-file is reached
        SELECT CASE (NumberOfLinesToRead)
            CASE (0)      !Case when no data lines are found inbetween comment lines
                BACKSPACE(ThisFile%UnitN)

            CASE DEFAULT  !Case when multiple data lines are found inbetween comment lines
                DO indx=1,NumberOfLinesToRead+1
                    BACKSPACE(ThisFile%UnitN)
                END DO
        END SELECT
        EXIT
      END IF
      NumberOfLinesToRead = NumberOfLinesToRead + 1
    END DO

    !Allocate character array to read in lines of character data
    IF (NumberOfLinesToRead .EQ. 0) RETURN
    ALLOCATE (CharacterData(NumberOfLinesToRead),STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for a set of character data',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    DO indx=1,NumberOfLinesToRead
      CALL ReadSingleData_AsciiInFile(ThisFile,CharacterData(indx),iStat=iStat)
      IF (iStat .EQ. -1) RETURN
    END DO

  END SUBROUTINE ReadCharacterUntilComment_AsciiInFile


  ! -------------------------------------------------------------
  ! --- READ THE FIRST CHARACTER OF THE CURRENT LINE FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadFirstCharacter(ThisFile,ACharacter,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CHARACTER(LEN=1),INTENT(OUT) :: ACharacter
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    INTEGER   :: ErrorCode
    CHARACTER :: FiveChars*5

    !Initialize
    iStat = 0

    FiveChars = ''
    READ (ThisFile%UnitN,'(A)',IOSTAT=ErrorCode) FiveChars
    ACharacter = FiveChars(1:1)
    IF (ErrorCode .NE. 0) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    ThisFile%AtLine = ThisFile%AtLine + 1

  END SUBROUTINE ReadFirstCharacter


  ! -------------------------------------------------------------
  ! --- READ NON-COMMENT LINES UNTIL A COMMENT LINE IS REACHED
  ! -------------------------------------------------------------
  SUBROUTINE ReadToComment(ThisFile,iStat)
    CLASS(AsciiInFileType) :: ThisFile
    INTEGER,INTENT(OUT)    :: iStat

    !Local variables
    INTEGER   :: ErrorCode
    CHARACTER :: ACharacter*1

    !INitialize
    iStat = 0

    CALL SkipComment(ThisFile,iStat)
    IF (iStat .EQ. -1) RETURN
    DO
        READ (ThisFile%UnitN,'(A)',IOSTAT=ErrorCode) ACharacter
        IF (ErrorCode .NE. 0) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        ThisFile%AtLine = ThisFile%AtLine + 1
        IF (SCAN(f_cCommentIndicators,ACharacter) .NE. 0) THEN
            CALL ThisFile%Backspace()
            EXIT
        END IF
    END DO

  END SUBROUTINE ReadToComment


  ! -------------------------------------------------------------
  ! --- READ SINGLE TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadSingleData_AsciiTSDInFile(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(AsciiTSDInFileType)                    :: ThisFile
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)                         :: Data
    INTEGER,INTENT(OUT)                          :: FileReadCode,iStat
    LOGICAL,OPTIONAL,INTENT(IN)                  :: TraceTime

    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'ReadSingleData_AsciiTSDInFile'
    INTEGER                      :: iData(1,1)
    REAL(8)                      :: rData(1,1)

    !Initialize
    iStat = 0

    !Transfer read method to matrix time series data reader
    SELECT TYPE (Data)
        TYPE IS (INTEGER)
            IF (PRESENT(TraceTime)) THEN
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,iData,FileReadCode,iStat,TraceTime)
            ELSE
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,iData,FileReadCode,iStat)
            END IF
            IF (FileReadCode .EQ. 0) Data = iData(1,1) !If any value is read, transfer that value to tyhe return variable

        TYPE IS (REAL(8))
            IF (PRESENT(TraceTime)) THEN
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,rData,FileReadCode,iStat,TraceTime)
            ELSE
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,rData,FileReadCode,iStat)
            END IF
            IF (FileReadCode .EQ. 0) Data = rData(1,1) !If any value is read, transfer that value to tyhe return variable

        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

  END SUBROUTINE ReadSingleData_AsciiTSDInFile


  ! -------------------------------------------------------------
  ! --- READ ARRAY TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadArrayData_AsciiTSDInFile(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(AsciiTSDInFileType)                    :: ThisFile
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)                         :: Data(:)
    INTEGER,INTENT(OUT)                          :: FileReadCode,iStat
    LOGICAL,OPTIONAL,INTENT(IN)                  :: TraceTime

    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'ReadArrayData_AsciiTSDInFile'
    INTEGER                      :: iData(1,SIZE(Data))
    REAL(8)                      :: rData(1,SIZE(Data))

    !Initialize
    iStat = -1

    !Transfer read method to matrix integer time series data reader
    SELECT TYPE (Data)
        TYPE IS (INTEGER)
            IF (PRESENT(TraceTime)) THEN
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,iData,FileReadCode,iStat,TraceTime)
            ELSE
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,iData,FileReadCode,iStat)
            END IF
            IF (FileReadCode .EQ. 0) Data = iData(1,:) !If any value is read, transfer that value to tyhe return variable

        TYPE IS (REAL(8))
            IF (PRESENT(TraceTime)) THEN
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,rData,FileReadCode,iStat,TraceTime)
            ELSE
                CALL ThisFile%ReadMatrixData_AsciiTSDInFile(Time,rData,FileReadCode,iStat)
            END IF
            IF (FileReadCode .EQ. 0) Data = rData(1,:) !If any value is read, transfer that value to tyhe return variable

        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1

    END SELECT

  END SUBROUTINE ReadArrayData_AsciiTSDInFile


  ! -------------------------------------------------------------
  ! --- READ MATRIX TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadMatrixData_AsciiTSDInFile(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(AsciiTSDInFileType)                    :: ThisFile
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)                         :: Data(:,:)
    INTEGER,INTENT(OUT)                          :: FileReadCode,iStat
    LOGICAL,INTENT(IN),OPTIONAL                  :: TraceTime

    !Local variables
    LOGICAL :: IsTimeTracingRequired,lDataTimeStamped

    !Initialize
    iStat = 0
    IF (PRESENT(TraceTime)) THEN
        IsTimeTracingRequired = TraceTime
    ELSE
        IsTimeTracingRequired = .TRUE.
    END IF
    FileReadCode = -99  !Default; problem in assigning a status code to Stat

    !Check if a non time-stamped or a time-stamped data will be read
    IF (IsTimeTracingRequired) THEN
        CALL IsDataTimeStamped(ThisFile,lDataTimeStamped,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (lDataTimeStamped) THEN
            CALL ReadTimeStampedMatrixData(iStat)
        ELSE
            FileReadCode = 1  !Data is not properly time stamped
            RETURN
        END IF
    ELSE
        CALL ReadNonTimeStampedMatrixData(iStat)
    END IF

  CONTAINS

    !####################################################
    ! --- READ NON TIME-STAMPED MATRIX TIME SERIES DATA
    !####################################################
    SUBROUTINE ReadNonTimeStampedMatrixData(iStat)
      INTEGER,INTENT(OUT) :: iStat

      !If it is time for read action do so and reinitialize NumberOfTimeStepsPassed variable
      IF (IsItTimeToRead(ThisFile)) THEN
          CALL PlacePointerBehindTimeColumn(ThisFile,iStat)  ;  IF (iStat .EQ. -1) RETURN !Place the file pointer behind the time column as an initialization for time series data reading
          CALL ThisFile%ReadMatrixData_AsciiInFile(Data,Skip=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL ReinitializeNumberOfTimeStepsPassed(ThisFile)
          CALL IncrementNumberOfReads(ThisFile)
          CALL CheckRewindStatus(ThisFile,iStat)  ;  IF (iStat .EQ. -1) RETURN

          SELECT TYPE (Data)
              TYPE IS (REAL(8))
                  !Convert the rate-type data so that the time unit matches the time step of simulation
                  IF (ALLOCATED(ThisFile%RateTypeData)) &
                      CALL AdjustRateTypeData(Data,ThisFile%RateTypeData,ConversionFactor=REAL(ThisFile%NSP,8))
          END SELECT
          FileReadCode = 0
      ELSE
          CALL IncrementNumberOfTimeStepsPassed(ThisFile)
          FileReadCode = -1  !Data was not read because it wasn't time to read data
      END IF
    END SUBROUTINE ReadNonTimeStampedMatrixData

    !####################################################
    ! --- READ TIME-STAMPED MATRIX TIME SERIES DATA
    !####################################################
    SUBROUTINE ReadTimeStampedMatrixData(iStat)
      INTEGER,INTENT(OUT) :: iStat

      !Local variables
      CHARACTER(LEN=30000)              :: InitialDataLine
      INTEGER                           :: NDataLines,LineNumberToGo
      CHARACTER(LEN=f_iTimeStampLength) :: LastDataDate

      !Check if the NumberOfDataLines is identified; if not identify
      IF (ThisFile%NumberOfDataLines .LT. 0) THEN
          NDataLines = ThisFile%AtLine
          CALL PlacePointerBehindTimeColumn(ThisFile,iStat)                        ;  IF (iStat .EQ. -1) RETURN
          CALL MarkInitialDataLine(ThisFile,InitialDataLine,iStat)                 ;  IF (iStat .EQ. -1) RETURN
          CALL ThisFile%ReadMatrixData_AsciiInFile(Data,Skip=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
          NDataLines                 = ThisFile%AtLine - NDataLines
          ThisFile%NumberOfDataLines = NDataLines
          CALL ThisFile%Backspace(NDataLines)
      END IF

      !Check if it is time read data; if so read it
      IF (IsItTimeToRead(ThisFile,Time)) THEN
          !Locate the Julian date in the data file
          NDataLines = ThisFile%NumberOfDataLines
          CALL LocateDate(ThisFile,Time,NDataLines,LineNumberToGo,LastDataDate,iStat=iStat)
          IF (iStat .EQ. -1) RETURN

          !Position the file pointer behind the time stamp as an initialization for the time series data reading
          CALL PlacePointerBehindTimeColumn(ThisFile,iStat)
          IF (iStat .EQ. -1) RETURN

          !Then, read the desired data
          CALL ThisFile%ReadMatrixData_AsciiInFile(Data,Skip=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL GoToLine(ThisFile,LineNumberToGo,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  !Rewind the file to the beginning of data set before the one that was just read; this allows for error-free data reading when there are multiple time steps in-between two measured data

          !Convert the rate-type data so that the time unit matches the time step of simulation
          SELECT TYPE (Data)
              TYPE IS (REAL(8))
                  IF (ALLOCATED(ThisFile%RateTypeData)) &
                      CALL AdjustRateTypeData(Data,ThisFile%RateTypeData,DataInterval=ThisFile%Interval_InMinutes,LastDataDate=LastDataDate)
          END SELECT

          !If read method is completed, then no error occured
          FileReadCode = 0
      ELSE
          FileReadCode = -1  !Data was not read because it wasn't time to read data
      END IF
    END SUBROUTINE ReadTimeStampedMatrixData

  END SUBROUTINE ReadMatrixData_AsciiTSDInFile


  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR A TIME RANGE RANGE FOR A LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE ReadTimeSeries_ForTimeRange_AsciiTSDInFile(ThisFile,iRow,iCol,iRowMax,iColMax,cBeginDateAndTime,cEndDateAndTime,nActualOutput,Data,rDataDates,FileReadCode,iStat)
    CLASS(AsciiTSDInFileType)   :: ThisFile
    INTEGER,INTENT(IN)          :: iRow,iCol           !Defines the location within the matrix data
    INTEGER,INTENT(IN)          :: iRowMax,iColMax     !Defines the size of the matrix to be read; if iRowMax=1 input data is array
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    CLASS(*),INTENT(OUT)        :: Data(:)
    REAL(8),INTENT(OUT)         :: rDataDates(:)
    INTEGER,INTENT(OUT)         :: nActualOutput,FileReadCode,iStat

    !Local variables
    INTEGER                           :: iDataIni(iRowMax,iColMax),iData(iRow,iCol)
    REAL(8)                           :: rDataIni(iRowMax,iColMax),rData(iRow,iCol),JulianDate_ReadDateAndTime
    CHARACTER(LEN=f_iTimeStampLength) :: cReadDateAndTime

    !Initialize
    iStat                      = 0
    FileReadCode               = 0
    nActualOutput              = 0
    cReadDateAndTime           = cBeginDateAndTime
    JulianDate_ReadDateAndTime = TimeStampToJulian(cReadDateAndTime)

    !First, read data for the output begin date and time to make sure that the file is properly instantiated
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            CALL ReadMatrixData_AsciiTSDInFile(ThisFile,cReadDateAndTime,rDataIni,FileReadCode,iStat,TraceTime=.TRUE.)
            IF (FileReadCode .NE. 0) RETURN
            nActualOutput = 1
            Data(1)       = rDataIni(iRow,iCol)
            rDataDates(1) = ThisFile%JulianDate2
        TYPE IS (INTEGER)
            CALL ReadMatrixData_AsciiTSDInFile(ThisFile,cReadDateAndTime,iDataIni,FileReadCode,iStat,TraceTime=.TRUE.)
            IF (FileReadCode .NE. 0) RETURN
            nActualOutput = 1
            Data(1)       = iDataIni(iRow,iCol)
            rDataDates(1) = ThisFile%JulianDate2
    END SELECT

    !Find first input data timestamp at or after the output begin date and time, and increment it to read the next data
    cReadDateAndTime = JulianToTimeStamp(ThisFile%JulianDate2)

    !Read the rest of the data
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            DO
                !Increment date
                cReadDateAndTime = IncrementTimeStamp(cReadDateAndTime,ThisFile%Interval_InMinutes,1)

                !Exit if past the end output date and time
                IF (cReadDateAndTime .TSGT. cEndDateAndTime) EXIT

                !Read data
                CALL ReadMatrixData_AsciiTSDInFile(ThisFile,cReadDateAndTime,rData,FileReadCode,iStat,TraceTime=.TRUE.)
                IF (FileReadCode .NE. 0) EXIT
                nActualOutput             = nActualOutput + 1
                Data(nActualOutput)       = rData(iRow,iCol)
                rDataDates(nActualOutput) = ThisFile%JulianDate2

            END DO


        TYPE IS (INTEGER)
            DO
                !Increment date
                cReadDateAndTime = IncrementTimeStamp(cReadDateAndTime,ThisFile%Interval_InMinutes,1)

                !Exit if past the end output date and time
                IF (cReadDateAndTime .TSGT. cEndDateAndTime) EXIT

                !Read data
                CALL ReadMatrixData_AsciiTSDInFile(ThisFile,cReadDateAndTime,iData,FileReadCode,iStat,TraceTime=.TRUE.)
                IF (FileReadCode .NE. 0) EXIT
                nActualOutput             = nActualOutput + 1
                Data(nActualOutput)       = iData(iRow,iCol)
                rDataDates(nActualOutput) = ThisFile%JulianDate2

            END DO
    END SELECT

  END SUBROUTINE ReadTimeSeries_ForTimeRange_AsciiTSDInFile




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
  ! --- WRITE A SINGLE DATA TO ASCII OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteSingleData_AsciiOutFile(ThisFile,Data)
      CLASS(AsciiOutFileType) :: ThisFile
      CLASS(*),INTENT(IN)     :: Data

    !Local variables
    CHARACTER(LEN=ModNameLen+28),PARAMETER :: ThisProcedure = ModName // 'WriteSingleData_AsciiOutFile'

    SELECT TYPE (Data)
        TYPE IS (CHARACTER(LEN=*))
            WRITE (ThisFile%UnitN,'(A)') TRIM(Data)
            
        TYPE IS (REAL)
            WRITE (ThisFile%UnitN,*) Data

        TYPE IS (REAL(8))
            WRITE (ThisFile%UnitN,*) Data

        TYPE IS (INTEGER)
            WRITE (ThisFile%UnitN,*) Data

        TYPE IS (GenericString)
            WRITE (ThisFile%UnitN,'(A)') GenericString_To_String(Data)

        CLASS DEFAULT
            CALL LogMessage('Trying to write unrecognized data type to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)

    END SELECT

  END SUBROUTINE WriteSingleData_AsciiOutFile


  ! -------------------------------------------------------------
  ! --- WRITE ARRAY DATA TO ASCII OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteArrayData_AsciiOutFile(ThisFile,Data,FormatSpec)
    CLASS(AsciiOutFileType)              :: ThisFile
    CLASS(*),INTENT(IN)                  :: Data(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: FormatSpec

    !Local variables
    CHARACTER(LEN=ModNameLen+27),PARAMETER :: ThisProcedure = ModName // 'WriteArrayData_AsciiOutFile'
    INTEGER                                :: indx

    SELECT TYPE (Data)
        TYPE IS (CHARACTER(LEN=*))
            IF (PRESENT(FormatSpec)) THEN
                WRITE (ThisFile%UnitN,FormatSpec) (TRIM(Data(indx)),indx=1,SIZE(Data))
            ELSE
                DO indx=1,SIZE(Data)
                    WRITE (ThisFile%UnitN,'(A)') Data(indx)
                END DO
            END IF


        TYPE IS (GenericString)
            DO indx=1,SIZE(Data)
                WRITE (ThisFile%UnitN,'(A)') GenericString_To_String(Data(indx))
            END DO

        CLASS DEFAULT
            CALL LogMessage('Trying to write unrecognized data type to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)

    END SELECT

  END SUBROUTINE WriteArrayData_AsciiOutFile


  ! -------------------------------------------------------------
  ! --- WRITE ARRAY TIMES SERIES DATA TO ASCII TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteArrayData_AsciiTSDOutFile(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(AsciiTSDOutFileType)  :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:)
    LOGICAL,INTENT(IN),OPTIONAL :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+30),PARAMETER :: ThisProcedure = ModName // 'WriteArrayData_AsciiTSDOutFile'
    REAL(8)                                :: rData(1,SIZE(Data))

    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            rData(1,:) = Data
            CALL ThisFile%WriteMatrixData_AsciiTSDOutFile(SimulationTime,rData,FinalPrint)

        CLASS DEFAULT
            CALL LogMessage ('Array of data type cannot be written to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)

    END SELECT

  END SUBROUTINE WriteArrayData_AsciiTSDOutFile


  ! -------------------------------------------------------------
  ! --- WRITE MATRIX TIMES SERIES DATA TO ASCII TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData_AsciiTSDOutFile(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(AsciiTSDOutFileType)  :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:,:)
    LOGICAL,INTENT(IN)          :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+31),PARAMETER :: ThisProcedure = ModName // 'WriteMatrixData_AsciiTSDOutFile'
    INTEGER                                :: indx,indx1,indxend,DataPointer,NumberOfDataRows,NumberOfDataBatch

    !Initialize variables
    NumberOfDataBatch = ThisFile%NumberOfDataBatch
    NumberOfDataRows  = ThisFile%NumberOfDataRows
    DataPointer       = ThisFile%DataPointer

    !Advance DataPointer
    DataPointer = DataPointer+1

    !Update the stored results
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            ThisFile%ValuesForOutput(:,:,DataPointer) = Data
        CLASS DEFAULT
            CALL LogMessage('Array of data type cannot be written to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
            RETURN
    END SELECT

    !Store simulation time
    ThisFile%TimeArray(DataPointer) = SimulationTime

    !If the storage is full or it is the end of simulation, dump the contents into the file
    IF (DataPointer .EQ. NumberOfDataBatch .OR. FinalPrint) THEN
      IF (FinalPrint) THEN
        indxend = DataPointer
      ELSE
        indxend = NumberOfDataBatch
      END IF
      DO indx=1,indxend
        WRITE (ThisFile%UnitN,ThisFile%FormatStatement) ThisFile%TimeArray(indx),(ThisFile%ValuesForOutput(indx1,:,indx),indx1=1,NumberOfDataRows)
      END DO
      !Initialize the data storage pointer
      DataPointer = 0
    END IF

    !Update DataPointer field
    ThisFile%DataPointer = DataPointer

  END SUBROUTINE WriteMatrixData_AsciiTSDOutFile




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
  ! --- SKIP COMMENT LINES IN A FILE
  ! -------------------------------------------------------------
  SUBROUTINE SkipComment(ThisFile,iStat)
    CLASS(AsciiInFileType) :: ThisFile
    INTEGER,INTENT(OUT)    :: iStat

    !Local variables
    CHARACTER :: junk*1

    !Initialize
    iStat = 0

    !Skip lines that start with 'C','c' or '*'
    DO
      CALL ReadFirstCharacter(ThisFile,junk,iStat)
      IF (iStat .EQ. -1) RETURN
      IF (SCAN(f_cCommentIndicators,junk) .EQ. 0) THEN
        CALL ThisFile%Backspace()
        EXIT
      END IF
    END DO

  END SUBROUTINE SkipComment


  ! -------------------------------------------------------------
  ! --- SKIP A GIVEN NUMBER OF LINES IN A FILE
  ! -------------------------------------------------------------
  SUBROUTINE SkipLines(ThisFile,NLinesToSkip,iStat)
    CLASS(AsciiInFileType)      :: ThisFile
    INTEGER,OPTIONAL,INTENT(IN) :: NLinesToSkip
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER :: Skip*1
    INTEGER   :: indx,LocalNLinesToSkip

    !Initialize
    iStat = 0
    IF (PRESENT(NLinesToSkip)) THEN
        LocalNLinesToSkip = NLinesToSkip
    ELSE
        LocalNLinesToSkip = 1
    END IF

    DO indx=1,LocalNLinesToSkip
      CALL ReadFirstCharacter(ThisFile,Skip,iStat)
      IF (iStat .EQ. -1) RETURN
    END DO

  END SUBROUTINE SkipLines


  ! -------------------------------------------------------------
  ! --- SKIP BLOCK(S) OF DATA
  ! -------------------------------------------------------------
  SUBROUTINE SkipDataBlock(ThisFile,NBlock,iStat)
    CLASS(AsciiInFileType) :: ThisFile
    INTEGER,INTENT(IN)     :: NBlock
    INTEGER,INTENT(OUT)    :: iStat

    !Local variables
    INTEGER :: indx

    !Initialize
    iStat = 0

    DO indx=1,NBlock
      CALL SkipComment(ThisFile,iStat)    ;  IF (iStat .EQ. -1) RETURN
      CALL ReadToComment(ThisFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    CALL SkipComment(ThisFile,iStat)

  END SUBROUTINE SkipDataBlock


  ! -------------------------------------------------------------
  ! --- BACKSPACE IN FILE
  ! -------------------------------------------------------------
  SUBROUTINE Backspace_AsciiInFile(ThisFile,NBackspace)
    CLASS(AsciiInFileType)      :: ThisFile
    INTEGER,OPTIONAL,INTENT(IN) :: NBackspace

    !Local variables
    CHARACTER(LEN=ModNameLen+21),PARAMETER :: ThisProcedure = ModName // 'Backspace_AsciiInFile'
    INTEGER                                :: indx,LocalNBackspace

    !Initialize
    IF (PRESENT(NBackspace)) THEN
        LocalNBackspace = NBackspace
    ELSE
        LocalNBackspace = 1
    END IF

    !Backspace the file
    DO indx=1,LocalNBackspace
      BACKSPACE(ThisFile%UnitN)
      ThisFile%AtLine = ThisFile%AtLine - 1
    END DO

  END SUBROUTINE Backspace_AsciiInFile


  ! -------------------------------------------------------------
  ! --- REWIND ASCII INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Rewind_AsciiInFile(ThisFile)
    CLASS(AsciiInFileType) :: ThisFile

    !Rewind the file
    REWIND(ThisFile%UnitN)
    ThisFile%AtLine=1

  END SUBROUTINE Rewind_AsciiInFile


  ! -------------------------------------------------------------
  ! --- REWIND ASCII OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Rewind_AsciiOutFile(ThisFile)
    CLASS(AsciiOutFileType) :: ThisFile

    !Rewind the file
    REWIND(ThisFile%UnitN)

  END SUBROUTINE Rewind_AsciiOutFile


  ! -------------------------------------------------------------
  ! --- MARK THE INITIAL DATA LINE AND RE-POSITION THE POINTER
  ! -------------------------------------------------------------
  SUBROUTINE MarkInitialDataLine(ThisFile,InitialDataLine,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(OUT) :: InitialDataLine
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=LEN(InitialDataLine)+100) :: DataLine  !DataLine is 100 characters longer than InitialDataLine to compensate for the possibilty that pointer was not at the beginning of data line
    CHARACTER(LEN=20)                       :: FmtStmt
    INTEGER                                 :: Length,PointerPosition
    LOGICAL                                 :: IsToSkip

    !Initialize
    iStat    = 0
    IsToSkip = .FALSE.
    Length   = LEN(InitialDataLine)

    !Read the initial data line for reference to compute total number of data lines later
    CALL ReadSingleData_AsciiInFile(ThisFile,InitialDataLine,Skip=IsToSkip,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ThisFile%Backspace()    !This command positions the pointer at the beginning of the data line
    CALL ReadSingleData_AsciiInFile(ThisFile,DataLine,Skip=IsToSkip,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ThisFile%Backspace()     !This command positions the pointer at the beginning of the data line
    IF (DataLine(1:Length) .EQ. InitialDataLine) THEN
        !The pointer was at the beginning of line
        PointerPosition = 1
    ELSE
        !The pointer was not at the beginning of the data line; find where it was
        PointerPosition = 2
        DO
           IF (DataLine(PointerPosition:PointerPosition+Length-1) .EQ. InitialDataLine) EXIT
           PointerPosition = PointerPosition + 1
      END DO
    END IF

    !Reset the InitialDataline so that it includes the data line from the beginning
    InitialDataLine = DataLine(1:Length)

    !Position the pointer to its previous location
    IF (PointerPosition .EQ. 1) RETURN
    FmtStmt = '('//TRIM(IntToText(PointerPosition-1))//'X)'
    READ (ThisFile%UnitN,FmtStmt,ADVANCE='NO')

  END SUBROUTINE MarkInitialDataLine


  ! -------------------------------------------------------------
  ! --- FIND NUMBER OF DATA LINES READ
  ! -------------------------------------------------------------
  SUBROUTINE FindNumberOfDataLines(ThisFile,InitialDataLine,nrow,NDataLines,iStat)
    CLASS(AsciiInFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)  :: InitialDataLine
    INTEGER,INTENT(IN)           :: nrow
    INTEGER,INTENT(OUT),OPTIONAL :: NDataLines
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=LEN(InitialDataLine)) :: Dataline
    INTEGER                             :: NumberOfDataLines

    !Initialize
    iStat             = 0
    NumberOfDataLines = 1

    !Move up in the file to find the InitialDataLine
    DO
        BACKSPACE(ThisFile%UnitN)
        READ (ThisFile%UnitN,'(A)') DataLine ; BACKSPACE(ThisFile%UnitN)
        IF (DataLine.EQ.InitialDataLine .AND.   & !When the initial reference data line is reached
            NumberOfDataLines.GE.nrow         ) & ! -- this line makes sure that a repeating line doesn't fool the method --
            EXIT                                  !exit line counting routine
        NumberOfDataLines = NumberOfDataLines + 1
    END DO

    !Move down where the pointer should be
    CALL SkipLines(ThisFile,NLinesToSkip=NumberOfDataLines,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Assign return value to NDataLines
    IF (PRESENT(NDataLines))  NDataLines = NumberOfDataLines

  END SUBROUTINE FindNumberOfDataLines


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO RE-POSITION THE POINTER AT A SPECIFIC LINE IN AN ASCII TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE GoToLine(ThisFile,LineNumber,iStat)
    TYPE(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(IN)       :: LineNumber
    INTEGER,INTENT(OUT)      :: iStat

    !Local variables
    INTEGER :: AtLine

    !Initialize
    AtLine = ThisFile%AtLine

    !Go to line
    IF (LineNumber .GT. AtLine) THEN
      CALL SkipLines(ThisFile,LineNumber-AtLine,iStat=iStat)
    ELSE
      CALL ThisFile%Backspace(AtLine-LineNumber)
    END IF

  END SUBROUTINE GoToLine


  ! -------------------------------------------------------------
  ! --- LOCATE A TIME IN A TIME-STAMPED FILE
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE LocateDate(ThisFile,SimulationTime,NLinesInBatch,LineNumber1,LastDataDate,iStat)
    TYPE(AsciiTSDInFileType)                      :: ThisFile
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN)  :: SimulationTime
    INTEGER,INTENT(IN)                            :: NLinesInBatch
    INTEGER,INTENT(OUT)                           :: LineNumber1
    CHARACTER(LEN=f_iTimeStampLength),INTENT(OUT) :: LastDataDate
    INTEGER,INTENT(OUT)                           :: iStat

    !Local variables
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp1,TimeStamp2
    REAL(8)                           :: JulianDate,JulianDate1,JulianDate2
    LOGICAL                           :: Year4000Flag,LessThanJulianDate1
    INTEGER                           :: LineNumber2

    !Initialize
    iStat        = 0
    LastDataDate = ''

    !Convert simulation time stamp to Julian Date
    JulianDate = TimeStampToJulian(SimulationTime)

    !Read in the first time stamp and go to the next time stamp
    CALL GetTimeStamp(TimeStamp1,LineNumber1,FirstReading=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read in the second time stamp and skip appropriately
    CALL GetTimeStamp(TimeStamp2,LineNumber2,FirstReading=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Correct time stamps for leap year
    TimeStamp1 = LeapYearCorrection(TimeStamp1)
    TimeStamp2 = LeapYearCorrection(TimeStamp2)

    !Convert time stamps to Julian dates
    JulianDate1 = TimeStampToJulian(TimeStamp1)
    JulianDate2 = TimeStampToJulian(TimeStamp2)

    !Check the consistency of JulianDate1,JulianDate2 and JulianDate for files with Year 4000 flag
    IF (Year4000Flag) CALL CheckJulianDateConsistency()

    !Check if the JulianDate is between JulianDate1 and JulianDate2
    IF (JulianDateIsInBetween(JulianDate,JulianDate1,JulianDate2,LessThanJulianDate1)) THEN
      !Bingo; JulianDate is located
      !Store time stamps in the array to transfer back to calling program
      CALL AssignLastDataDate(iStat)  ;  IF (iStat .EQ. -1) RETURN
      IF (LessThanJulianDate1) THEN
          JulianDate2 = JulianDate1
          CALL GoToLine(ThisFile,LineNumber1,iStat=iStat)
          IF (iStat .EQ. -1) RETURN
      END IF
      ThisFile%JulianDate1 = JulianDate1
      ThisFile%JulianDate2 = JulianDate2
      ThisFile%FileRewound = .FALSE.
    ELSE
      !JulianDate is not located yet; keep reading
      CALL LocateDate(ThisFile,SimulationTime,NLinesInBatch,LineNumber1,LastDataDate,iStat=iStat)
    END IF


  CONTAINS


    !#################################################################################
    ! --- SUBROUTINE TO READ TIME STAMP
    !#################################################################################
    SUBROUTINE GetTimeStamp(TimeStamp,LineNumber,FirstReading,iStat)
      CHARACTER(LEN=f_iTimeStampLength),INTENT(OUT) :: TimeStamp
      INTEGER,INTENT(OUT)                           :: LineNumber,iStat
      LOGICAL,INTENT(IN)                            :: FirstReading

      !Local variables
      CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'GetTimeStamp'
      CHARACTER(LEN=150)           :: ALine
      INTEGER                      :: Location
      LOGICAL                      :: lEndOfFile

      !Initialize
      iStat = 0

      !Check if the pointer is at the end of file (hard or soft end-of-file) and perform accordingly
      CALL IsEndOfFile(lEndOfFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
      IF (lEndOfFile) THEN
        SELECT CASE (FirstReading)
          !If this is a reading for the first time stamp, generate an error
          CASE (.TRUE.)
            CALL SetLastMessage('End-of-file is reached in file ' //TRIM(ThisFile%Name)//'.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN

          !If this is a reading for the second time stamp, see if Year4000 flag is used to recycle the data
          CASE (.FALSE.)
            IF (Year4000Flag) THEN
              !If the data is recycled using Year 4000 flag, rewind file and continue reading data from top
              IF (ThisFile%FileRewound) THEN
                  CALL SetLastMessage('Simulation date cannot be located in file '//TRIM(ThisFile%Name),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
              ThisFile%NFQ         = ThisFile%NumberOfReads ; CALL CheckRewindStatus(ThisFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
              ThisFile%FileRewound = .TRUE.
            ELSE
              !Check if the first time stamp covers the simulation time
              IF (TimeStampToJulian(TimeStamp1) .GE. JulianDate) THEN
                  !Reposition the pointer at the last time stamp entry
                  CALL ThisFile%Backspace(NLinesInBatch)
              ELSE
                  CALL SetLastMessage('End-of-file is reached in file ' //TRIM(ThisFile%Name)//'.',f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
            END IF

        END SELECT
      END IF

      !Get the line number that the time stamp is found
      LineNumber = ThisFile%AtLine

      !Read the data line that stores the time stamp
      CALL ThisFile%ReadSingleData_AsciiInFile(ALine,SKIP=.FALSE.,iStat=iStat)
      IF (iStat .EQ. -1) RETURN

      !Extract the TimeStamp from data line and check if extraction was succesful
      TimeStamp = StripTimeStamp(ALine,Location)
      IF (Location.EQ.0)  THEN  !Extraction was not succesful; generate error
          CALL SetLastMessage('Error in time stamp in file '                                     //  &
                              TRIM(ThisFile%Name) //  &
                              ' at or around line '                                              //  &
                              IntToText(ThisFile%AtLine),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Adjust the time stamp if Year4000 flag is used
      CALL AdjustTimeStampWithYear4000(TimeStamp,SimulationTime,Year4000Flag)

      !Advance the pointer to the line where the next time stamp can be found
      IF (FirstReading) THEN
          CALL SkipLines(ThisFile,NLinesInBatch-1,iStat=iStat)
          IF (iStat .EQ. -1) RETURN
      END IF
      IF (.NOT. FirstReading) CALL ThisFile%Backspace()

    END SUBROUTINE GetTimeStamp


    !#################################################################################
    ! --- SUBROUTINE TO CHECK CONSISTENCY BETWEEN JULIAN DATES FOR FILES W/ YEAR 4000 FLAG
    !#################################################################################
    SUBROUTINE CheckJulianDateConsistency()

      !Local variables
      INTEGER,PARAMETER :: YearInMinutes=525600

      !If JulianDate is less than JulianDate1, decrement both JulianDate1 and JulianDate2 by 1 year
      IF (JulianDate .LE. JulianDate1) THEN
        TimeStamp1  = IncrementTimeStamp(TimeStamp1,-YearInMinutes)
        JulianDate1 = TimeStampToJulian(TimeStamp1)
        TimeStamp2  = IncrementTimeStamp(TimeStamp2,-YearInMinutes)
        JulianDate2 = TimeStampToJulian(TimeStamp2)
      END IF

      !Make sure that JulianDate2 is greater than JulianDate1; if not increment JulianDate2 by 1 year
      IF (JulianDate2 .LT. JulianDate1) THEN
        TimeStamp2  = IncrementTimeStamp(TimeStamp2,YearInMinutes)
        JulianDate2 = TimeStampToJulian(TimeStamp2)
      END IF

    END SUBROUTINE CheckJulianDateConsistency


    !#################################################################################
    ! --- FUNCTION TO CHECK IF THE DATA ENTRY IS THE LAST IN THE FILE
    !#################################################################################
    SUBROUTINE IsEndOfFile(IsItEndOfFile,iStat)
      LOGICAL,INTENT(OUT) :: IsItEndOfFile
      INTEGER,INTENT(OUT) :: iStat

      !Local variables
      CHARACTER :: ALine*3000
      INTEGER   :: NLines,ErrorCode

      !Initialize
      iStat         = 0
      IsItEndOfFile = .TRUE.
      NLines        = 1

      DO
        CALL ThisFile%ReadSingleData_AsciiInFile(ALine,Status=ErrorCode,Skip=.FALSE.,iStat=iStat)
        IF (ErrorCode .EQ. -1) EXIT !End-of-file flag is reached
        CALL CleanSpecialCharacters(ALine)
        IF (ALine.NE.'' .AND. SCAN(f_cCommentIndicators,ALine(1:1)).EQ.0) THEN     !An empty line is in-between data lines
          IsItEndOfFile = .FALSE.
          EXIT
        END IF
        NLines = NLines+1
      END DO
      CALL ThisFile%Backspace(NLines)

    END SUBROUTINE IsEndOfFile


    !#################################################################################
    ! --- SUBROUTINE TO ASSIGN DataTimes ARRAY FOR THE DATES OF TWO DATA ENTRY THAT ENCOMPASS SIMULATION TIME
    !#################################################################################
    SUBROUTINE AssignLastDataDate(iStat)
      INTEGER,INTENT(OUT) :: iStat

      !Local variables
      CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'AssignLastDataDate'
      INTEGER                                :: JulianDateBegin,MinutesBegin,JulianDateEnd,MinutesEnd,Interval_InMinutes

      !Initialize
      iStat = 0

      !If the Interval_InMinutes data field is not specified, specify
      IF (ThisFile%Interval_InMinutes .LT. 0) THEN
        IF (TimeStamp1 .EQ. TimeStamp2) THEN
          ThisFile%Interval_InMinutes = 0
        ELSE
          CALL TimeStampToJulianDateAndMinutes(TimeStamp1,JulianDateBegin,MinutesBegin)
          CALL TimeStampToJulianDateAndMinutes(TimeStamp2,JulianDateEnd,MinutesEnd)
          Interval_InMinutes = (JulianDateEnd-JulianDateBegin)*1440+(MinutesEnd-MinutesBegin)
          !Special treatment for monthly inetrval
          IF (Interval_InMinutes.LE.44640 .AND. Interval_InMinutes.GE.40320) Interval_InMinutes = 43200
          !Special treatment for yearly interval
          IF (Interval_InMinutes .GE. 525600) Interval_InMinutes = 525600
          IF (LocateInList(Interval_InMinutes,f_iRecognizedIntervals_InMinutes) .LT. 1) THEN
              CALL SetLastMessage('Error in computing data interval for file '//TRIM(ThisFile%Name)//'!',f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
          ThisFile%Interval_InMinutes = Interval_InMinutes
        END IF
      END IF

      !Assign LastDataDate
      LastDataDate = TimeStamp2
      IF (LessThanJulianDate1) LastDataDate = TimeStamp1

    END SUBROUTINE AssignLastDataDate

  END SUBROUTINE LocateDate


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CHECK IF ENOUGH NUMBER OF TIME STEPS HAVE PASSED BEFORE READING A DATA
  ! -------------------------------------------------------------
  FUNCTION IsItTimeToRead(ThisFile,Time) RESULT(ItIsTimeToRead)
    TYPE(AsciiTSDInFileType),INTENT(IN)  :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Time
    LOGICAL                              :: ItIsTimeToRead

    !Local variables
    REAL(8) :: JulianDate

    ItIsTimeToRead = .FALSE.
    IF (PRESENT(Time)) THEN
      JulianDate = TimeStampToJulian(Time)
      IF (.NOT. JulianDateIsInBetween(JulianDate,ThisFile%JulianDate1,ThisFile%JulianDate2)) ItIsTimeToRead = .TRUE.
    ELSE
      IF (ThisFile%NSP.EQ.ThisFile%NumberOfTimeStepsPassed) ItIsTimeToRead = .TRUE.
    END IF

  END FUNCTION IsItTimeToRead


  ! -------------------------------------------------------------
  ! --- FUNCTION TO TEST IF A JULIAN DATE IS BETWEEN TWO OTHER JULIAN DATES
  ! -------------------------------------------------------------
  FUNCTION JulianDateIsInBetween(JulianDate,JulianDate1,JulianDate2,LessThanJulianDate1) RESULT(IsInBetween)
    REAL(8),INTENT(IN)           :: JulianDate,JulianDate1,JulianDate2
    LOGICAL,OPTIONAL,INTENT(OUT) :: LessThanJulianDate1
    LOGICAL                      :: IsInBetween

    IsInBetween = .FALSE.
    IF (JulianDate.LE.JulianDate1                                  .OR.  &
        (JulianDate.GT.JulianDate1 .AND. JulianDate.LE.JulianDate2)    ) &
      IsInBetween = .TRUE.
    IF (PRESENT(LessThanJulianDate1)) THEN
      LessThanJulianDate1 = .FALSE.
      IF (JulianDate.LE.JulianDate1) LessThanJulianDate1 = .TRUE.
    END IF

  END FUNCTION JulianDateIsInBetween


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO RE-INITIALIZE THE NUMBER OF TIME STEPS PASSED SINCE THE LAST READ ACTION
  ! -------------------------------------------------------------
  SUBROUTINE ReinitializeNumberOfTimeStepsPassed(ThisFile)
    TYPE(AsciiTSDInFileType) :: ThisFile

    ThisFile%NumberOfTimeStepsPassed = 1

  END SUBROUTINE ReinitializeNumberOfTimeStepsPassed


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO CHECK THE REWIND STATUS OF A FILE
  ! -------------------------------------------------------------
  SUBROUTINE CheckRewindStatus(ThisFile,iStat)
    TYPE(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(OUT)      :: iStat

    IF (ThisFile%NFQ .EQ. ThisFile%NumberOfReads) THEN
        CALL ThisFile%Rewind()
        CALL SkipDataBlock(ThisFile,ThisFile%NumberOfBlocksToSkip,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL SkipComment(ThisFile,iStat=iStat)                                  ;  IF (iStat .EQ. -1) RETURN
        ThisFile%NumberOfReads = 0
    END IF

  END SUBROUTINE CheckRewindStatus


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO INCREMENT NUMBER OF READ ACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE IncrementNumberOfReads(ThisFile)
    TYPE(AsciiTSDInFileType) :: ThisFile

    ThisFile%NumberOfReads = ThisFile%NumberOfReads + 1

  END SUBROUTINE IncrementNumberOfReads


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO INCREMENT NUMBER OF TIME STEPS PASSED
  ! -------------------------------------------------------------
  SUBROUTINE IncrementNumberOfTimeStepsPassed(ThisFile)
    TYPE(AsciiTSDInFileType) :: ThisFile

    ThisFile%NumberOfTimeStepsPassed = ThisFile%NumberOfTimeStepsPassed + 1

  END SUBROUTINE IncrementNumberOfTimeStepsPassed


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CHECK IF THE TIME SERIES DATA CAN BE USED FOR TIME-STAMPED DATA READING
  ! -------------------------------------------------------------
  SUBROUTINE IsDataTimeStamped(ThisFile,TimeStampStatus,iStat)
    TYPE(AsciiTSDInFileType) :: ThisFile
    LOGICAL,INTENT(OUT)      :: TimeStampStatus
    INTEGER,INTENT(OUT)      :: iStat

    !Local variables
    CHARACTER(LEN=150) :: ALine

    !Initialize
    iStat = 0

    !Check if the TimeStampedData component has already been identified, if not identify
    SELECT CASE (ThisFile%TimeStampedData)
      CASE (0)
        !First assume data is non time-stamped
        !Read a character line from the time series data for checking, then backspace file
        CALL ThisFile%ReadSingleData_AsciiInFile(ALine,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%Backspace()
        !Is the data non time-stamped?
        IF (.NOT.IsTimeStampValid(ALine)) THEN
          ThisFile%TimeStampedData = -1
          TimeStampStatus          = .FALSE.
        ELSE
          ThisFile%TimeStampedData = 1
          TimeStampStatus          = .TRUE.
        END IF

      CASE (1)
        TimeStampStatus = .TRUE.

      CASE (-1)
        TimeStampStatus = .FALSE.
    END SELECT

  END SUBROUTINE IsDataTimeStamped


  ! -------------------------------------------------------------
  ! --- PLACE FILE POINTER BEHIND TIME COLUMN
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE PlacePointerBehindTimeColumn(ThisFile,iStat)
    TYPE(AsciiTSDInFileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    CHARACTER(150)                :: ALine
    CHARACTER(10)                 :: ReadFormat
    CHARACTER(1)                  :: Dummy
    CHARACTER(f_iTimeStampLength) :: TimeStamp
    INTEGER                       :: TimeStampLocation,EndOfTimeColumn
    LOGICAL                       :: IsPreviousBlank,IsCurrentBlank,DummyLogical

    !Initialize
    iStat = 0

    !Check if file is time stamped
    SELECT CASE (ThisFile%TimeStampedData)
      CASE (0) !It is not known yet if the data is time stamped or not
        CALL IsDataTimeStamped(ThisFile,DummyLogical,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL PlacePointerBehindTimeColumn(ThisFile,iStat)    ;  IF (iStat .EQ. -1) RETURN

      CASE (1)  !Time stamped data; i.e. first reading will be a string
        CALL ThisFile%ReadData(ALine,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%Backspace()
        TimeStamp       = StripTimeStamp(ALine,Location=TimeStampLocation)
        EndOfTimeColumn = TimeStampLocation + f_iTimeStampLength - 1
        !Make a direct read from the file
        WRITE (ReadFormat,'(A)') ADJUSTL('('//TRIM(IntToText(EndOfTimeColumn))//'X)')
        READ (UNIT=ThisFile%UnitN,FMT=ReadFormat,ADVANCE='NO')

      CASE (-1) !Non time stamped; i.e. first reading will be an integer
        !Make a direct read from the file
        IsPreviousBlank = .TRUE.
        DO
          READ (UNIT=ThisFile%UnitN,FMT='(A)',ADVANCE='NO') Dummy
          IF (Dummy.EQ.' ' .OR. Dummy.EQ.ACHAR(9)) THEN
            IsCurrentBlank = .TRUE.
          ELSE
            IsCurrentBlank = .FALSE.
          END IF
          IF (.NOT.IsPreviousBlank .AND. IsCurrentBlank) EXIT
          IsPreviousBlank = IsCurrentBlank
        END DO

    END SELECT

  END SUBROUTINE PlacePointerBehindTimeColumn


  ! -------------------------------------------------------------
  ! --- REWIND TIME SERIES INPUT FILE TO THE BEGINNING OF THE TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE Rewind_To_BeginningOfTSData(ThisFile,iStat)
    CLASS(AsciiTSDInFileType) :: ThisFile
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    TYPE(AsciiTSDInFileType) :: Dummy

    !Initialize
    iStat = 0

    CALL ThisFile%Rewind()
    CALL SkipDataBlock(ThisFile,ThisFile%NumberOfBlocksToSkip,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL SkipComment(ThisFile,iStat=iStat)                                  ;  IF (iStat .EQ. -1) RETURN

    ThisFile%JulianDate1 = Dummy%JulianDate1
    ThisFile%JulianDate2 = Dummy%JulianDate2

  END SUBROUTINE Rewind_To_BeginningOfTSData


END MODULE
