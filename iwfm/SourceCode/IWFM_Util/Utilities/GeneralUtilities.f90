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
MODULE GeneralUtilities
  !$ USE OMP_LIB
  USE ISO_C_BINDING , ONLY: C_CHAR      , &
                            C_NULL_CHAR 
  USE MessageLogger , ONLY: SetLastMessage  , &
                            f_iFatal
  IMPLICIT NONE
  
  
  PRIVATE
  PUBLIC :: IntToText                     , &
            TextToInt                     , &
            ArrangeText                   , &
            ReplaceString                 , &
            FirstLocation                 , &
            CountOccurance                , &
            FindSubStringInString         , &
            CleanSpecialCharacters        , &
            UpperCase                     , &
            LowerCase                     , &
            StripTextUntilCharacter       , &
            GetStartLocation              , &
            f_cLineFeed                   , &
            LEN_TRIM_ARRAY                , &
            PrepareTitle                  , &
            String_Copy_C_F               , &
            String_Copy_F_C               , &
            CString_Len                   , &
            GenericString                 , &
            GenericString_To_String       , &
            String_To_GenericString       , &
            AppendString_To_GenericString 

  !Public array utilities
  PUBLIC :: AllocArray                    , &
            AllocPointerToArray           , &
            LocateInList                  , &
            NormalizeArray                , &
            ShellSort                     , &
            GetUniqueArrayComponents      , &
            GetArrayData                  , &
            L2Norm
                                            
  !Public directory utilities               
  PUBLIC :: ConvertPathToWindowsStyle     , &
            ConvertPathToLinuxStyle       , &
            IsPathWindowsStyle            , &
            IsAbsolutePathname            , &
            StripFileNameFromPath         , &
            EstablishAbsolutePathFileName , &
            GetFileDirectory                
                                            
  !Public misc. utilities                   
  PUBLIC :: GetDate                       , &
            GetTime                       , &
            Tolerance                     , &
            ConvertID_To_Index            , &
            FEXP


  !Data type for a generic string
  TYPE GenericString
    INTEGER                      :: NChars  = 0
    CHARACTER(LEN=1),ALLOCATABLE :: Chars(:) 
  END TYPE GenericString
  
  
  !Overload GenericString_To_String converter
  INTERFACE GenericString_To_String
    MODULE PROCEDURE SingleGenericString_To_String
    MODULE PROCEDURE ArrayGenericString_To_String
  END INTERFACE GenericString_To_String


  !Overload array allocation method
  INTERFACE AllocArray
    MODULE PROCEDURE Alloc1DIntArray
    MODULE PROCEDURE Alloc1DRealArray
    MODULE PROCEDURE Alloc1DLogicalArray
    MODULE PROCEDURE Alloc1DCharacterArray
    MODULE PROCEDURE Alloc2DIntArray
    MODULE PROCEDURE Alloc2DRealArray
    MODULE PROCEDURE Alloc3DRealArray
    MODULE PROCEDURE Alloc2DCharacterArray
  END INTERFACE AllocArray


  !Overload pointer to array allocation method
  INTERFACE AllocPointerToArray
    MODULE PROCEDURE AllocPointerTo1DCharacterArray
    MODULE PROCEDURE AllocPointerTo1DRealArray
    MODULE PROCEDURE AllocPointerTo1DLogicalArray
  END INTERFACE AllocPointerToArray


  !Overload text arranging methods
  INTERFACE ArrangeText
    MODULE PROCEDURE Arrange1String
    MODULE PROCEDURE Arrange2String
    MODULE PROCEDURE Arrange3String
    MODULE PROCEDURE Arrange4String
    MODULE PROCEDURE Arrange5String
  END INTERFACE ArrangeText


  !Overload string cleaning methods
  INTERFACE CleanSpecialCharacters
    MODULE PROCEDURE CleanSpecialCharactersInAString
    MODULE PROCEDURE CleanSpecialCharactersInStringArray
  END INTERFACE CleanSpecialCharacters


  !Overload list item locating methods
  INTERFACE LocateInList
    MODULE PROCEDURE LocateIntIn1DArray
    MODULE PROCEDURE LocateRealIn1DArray
    MODULE PROCEDURE LocateIntSetIn1DArray
  END INTERFACE LocateInList
  
  
  !Overload array data retrieving methods
  INTERFACE GetArrayData
    MODULE PROCEDURE GetRealArrayData
    MODULE PROCEDURE GetIntArrayData
    MODULE PROCEDURE GetCharArrayData
    MODULE PROCEDURE GetLogicalArrayData
    MODULE PROCEDURE GetRealArrayDataFromString
    MODULE PROCEDURE GetIntArrayDataFromString
  END INTERFACE GetArrayData

  
  !Overload shell sort methods
  INTERFACE ShellSort
    MODULE PROCEDURE ShellSort_NoSecondArray
    MODULE PROCEDURE ShellSort_IntSecondArray
    MODULE PROCEDURE ShellSort_RealSecondArray
    MODULE PROCEDURE ShellSort_LogicalSecondArray
    MODULE PROCEDURE ShellSort_CharSecondArray
  END INTERFACE ShellSort

  
  !Overload ID to index conversion methods
  INTERFACE ConvertID_To_Index
    MODULE PROCEDURE ConvertID_To_Index_Array
    MODULE PROCEDURE ConvertID_To_Index_Scalar
  END INTERFACE ConvertID_To_Index
  

  CHARACTER(LEN=16),PARAMETER :: ThisProcedure = 'GeneralUtilities'
  CHARACTER(LEN=1),PARAMETER  :: f_cLineFeed   = CHAR(10)
  INTEGER                     :: ErrorCode
          
          
          


CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** TEXT UTILITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- FIND THE LENGTH OF A C STRING
  ! -------------------------------------------------------------
  FUNCTION CString_Len(cstr) RESULT(iLen)
    CHARACTER(C_CHAR),INTENT(IN) :: cstr(*)
    INTEGER                      :: iLen
    
    iLen = 1
    DO 
        IF (cstr(iLen) .EQ. C_NULL_CHAR) EXIT
        iLen = iLen + 1
    END DO
    iLen = iLen - 1
        
  END FUNCTION CString_Len
  
  
  ! -------------------------------------------------------------
  ! --- COPY FORTRAN STRING TO C STRING
  ! -------------------------------------------------------------
  SUBROUTINE String_Copy_F_C(f_string, c_string)
      CHARACTER(LEN=*),INTENT(IN)   :: f_string
      CHARACTER(C_CHAR),INTENT(OUT) :: c_string(:)
     
      !Local variables
      INTEGER :: indx
      
      DO indx=1,LEN_TRIM(f_string)
          c_string(indx) = f_string(indx:indx)
      END DO
      IF (LEN(f_string) .GT. LEN_TRIM(f_string)) c_string(LEN_TRIM(f_string)+1) = C_NULL_CHAR
      
  END SUBROUTINE String_Copy_F_C
  
  
  ! -------------------------------------------------------------
  ! --- COPY C STRING TO FORTRAN STRING
  ! -------------------------------------------------------------
  SUBROUTINE String_Copy_C_F(c_string, f_string)
      CHARACTER(C_CHAR),INTENT(IN) :: c_string(:)
      CHARACTER(LEN=*),INTENT(OUT) :: f_string
      
      !Local variables
      INTEGER :: indx
      
      DO indx=1,SIZE(c_string) 
          IF (c_string(indx) .EQ. C_NULL_CHAR) THEN
              f_string(indx:) = ' '
              RETURN
          ELSE
              f_string(indx:indx) = c_string(indx)
          END IF
      END DO
      
  END SUBROUTINE String_Copy_C_F
  
  
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A SINGLE GENERIC STRING TO REGULAR STRING
  ! -------------------------------------------------------------
  FUNCTION SingleGenericString_To_String(GString) RESULT(String)
    TYPE(GenericString),INTENT(IN) :: GString
    CHARACTER(LEN=GString%NChars)  :: String
    
    !Local variables
    INTEGER :: indx
    
    String = ''
    DO indx=1,GString%NChars
      String(indx:indx) = GString%Chars(indx)
    END DO
  
  END FUNCTION SingleGenericString_To_String


  ! -------------------------------------------------------------
  ! --- CONVERT AN ARRAY OF GENERIC STRINGS TO REGULAR STRINGS
  ! -------------------------------------------------------------
  FUNCTION ArrayGenericString_To_String(GString) RESULT(String)
    TYPE(GenericString),INTENT(IN)     :: GString(:)
    CHARACTER(LEN=MAXVAL(GString%NChars,DIM=1)) :: String(SIZE(GString))
    
    !Local variables
    INTEGER :: indx,indxString
    
    String = ''
    DO indxString = 1,SIZE(GString)
      DO indx=1,GString(indxString)%NChars
        String(indxString)(indx:indx) = GString(indxString)%Chars(indx)
      END DO
    END DO
  
  END FUNCTION ArrayGenericString_To_String


  ! -------------------------------------------------------------
  ! --- CONVERT A SINGLE STRING TO GENERIC STRING
  ! -------------------------------------------------------------
  ELEMENTAL FUNCTION String_To_GenericString(String) RESULT(GString)
    CHARACTER(LEN=*),INTENT(IN) :: String
    TYPE(GenericString)         :: GString
    
    !Local variables
    INTEGER :: indx,NChars
    
    NChars         = LEN(String)
    GString%NChars = NChars 
    ALLOCATE (GString%Chars(NChars))
    DO indx=1,NChars
      GString%Chars(indx) = String(indx:indx)
    END DO
  
  END FUNCTION String_To_GenericString
  

  ! -------------------------------------------------------------
  ! --- APPEND A STRING TO A GENERIC STRING
  ! -------------------------------------------------------------
  FUNCTION AppendString_To_GenericString(GString,String) RESULT(AppendGString)
    TYPE(GenericString),INTENT(IN) :: GString
    CHARACTER(LEN=*),INTENT(IN)    :: String
    TYPE(GenericString)            :: AppendGString
    
    !Local variables
    TYPE(GenericString) :: TempGString
    
    TempGString = String_To_GenericString(String)
    AppendGString%NChars = GString%NChars + TempGString%NChars
    ALLOCATE (AppendGString%Chars(AppendGString%NChars))
    AppendGString%Chars(1:GString%NChars)  = GString%Chars
    AppendGString%Chars(GString%NChars+1:) = TempGString%Chars
    
  END FUNCTION AppendString_To_GenericString


  ! -------------------------------------------------------------
  ! --- FIND THE MAXIMUM LENGTH OF A STRING ARRAY
  ! -------------------------------------------------------------
  FUNCTION LEN_TRIM_ARRAY(StrArray) RESULT(Length)
    CHARACTER(LEN=*)::StrArray(:)
    INTEGER::Length
    
    !Local variables
    INTEGER::indx,LT
    
    Length = 0
    DO indx=1,SIZE(StrArray(:))
      LT = LEN_TRIM(StrArray(indx))
      IF (LT .GT. Length) Length = LT
    END DO
    
  END FUNCTION LEN_TRIM_ARRAY


  ! -------------------------------------------------------------
  ! --- REPLACE STRING WITH ANOTHER STRING IN A STRING VARIABLE
  ! -------------------------------------------------------------
  SUBROUTINE ReplaceString(InString,StringToReplace,StringToReplaceWith,iStat)
    CHARACTER(LEN=*)            :: InString
    CHARACTER(LEN=*),INTENT(IN) :: StringToReplace,StringToReplaceWith
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER :: BeginLocation,iLen
    
    !Initialize
    iStat = 0
    
    !Make sure that string to be replaced and string to replace with has the same length
    IF (LEN(StringToReplace) .NE. LEN(StringToReplaceWith)) THEN
        CALL SetLastMessage('String to be replaced with must have the same length as the replacing string!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    iLen = LEN(StringToReplace) - 1
    DO
        CALL FindSubStringInString(StringToReplace,InString,BeginLocation)
        IF (BeginLocation .EQ. 0) EXIT
        InString(BeginLocation:BeginLocation+iLen) = StringToReplaceWith
    END DO
    
  END SUBROUTINE ReplaceString
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT AN INTEGER TO TEXT
  ! -------------------------------------------------------------
  FUNCTION IntToText(Number) RESULT(Text)
    INTEGER,PARAMETER           :: MaxTextLength=20
    INTEGER,INTENT(IN)          :: Number
    CHARACTER(LEN=MaxTextLength):: Text

    !Local variables
    CHARACTER(LEN=6)::FormatStatement
    WRITE (FormatStatement,'(A,I3,A)') '(I',MaxTextLength,')'
    
    WRITE (Text,FMT=FormatStatement) Number
    Text=ADJUSTL(Text)

  END FUNCTION IntToText


  ! -------------------------------------------------------------
  ! --- CONCATENATE SEVERAL STRINGS AND CENTER THE RESULT IN A LINE 
  ! -------------------------------------------------------------
  FUNCTION ArrangeTextMaster(InputData,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN),DIMENSION(:)::InputData
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=2000)::WorkString
    INTEGER::StringLength,NumberOfLeadingSpace,indx

    !Concatenate the strings
    WorkString=''
    DO indx=1,SIZE(InputData)
      WorkString=TRIM(ADJUSTL(WorkString))//TRIM(ADJUSTL(InputData(indx)))
    END DO

    !Center the concatenated strings in the line
    StringLength=LEN_TRIM(WorkString)  !Length of the concatenated strings
    Text=''
    IF (StringLength.GT.LineLength) THEN
      Text(1:LineLength)=WorkString(1:LineLength)
    ELSE
      NumberOfLeadingSpace=(LineLength-StringLength)/2
      Text(NumberOfLeadingSpace+1:NumberOfLeadingSpace+StringLength)=WorkString(1:StringLength)
    END IF

  END FUNCTION ArrangeTextMaster


  !ARRANGE SINGLE STRING
  FUNCTION Arrange1String(InputData,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN)::InputData
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=LEN(InputData))::SentData(1)
    SentData(1)=InputData

    !Transfer control to master text arranger
    Text=ArrangeTextMaster(SentData,LineLength)

  END FUNCTION Arrange1String


  !ARRANGE TWO STRINGS
  FUNCTION Arrange2String(InputData1,InputData2,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN)::InputData1,InputData2
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=LineLength)::SentData(2)
    SentData(1)=InputData1
    SentData(2)=InputData2

    !Transfer control to master text arranger
    Text=ArrangeTextMaster(SentData,LineLength)

  END FUNCTION Arrange2String


  !ARRANGE THREE STRINGS
  FUNCTION Arrange3String(InputData1,InputData2,InputData3,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN)::InputData1,InputData2,InputData3
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=LineLength)::SentData(3)
    SentData(1)=InputData1
    SentData(2)=InputData2
    SentData(3)=InputData3

    !Transfer control to master text arranger
    Text=ArrangeTextMaster(SentData,LineLength)

  END FUNCTION Arrange3String


  !ARRANGE FOUR STRINGS
  FUNCTION Arrange4String(InputData1,InputData2,InputData3,InputData4,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN)::InputData1,InputData2,InputData3,InputData4
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=LineLength)::SentData(4)
    SentData(1)=InputData1
    SentData(2)=InputData2
    SentData(3)=InputData3
    SentData(4)=InputData4

    !Transfer control to master text arranger
    Text=ArrangeTextMaster(SentData,LineLength)

  END FUNCTION Arrange4String


  !ARRANGE FIVE STRINGS
  FUNCTION Arrange5String(InputData1,InputData2,InputData3,InputData4,InputData5,LineLength) RESULT(Text)
    CHARACTER(LEN=*),INTENT(IN)::InputData1,InputData2,InputData3,InputData4,InputData5
    INTEGER,INTENT(IN)::LineLength
    CHARACTER(LEN=LineLength)::Text

    !Local variables
    CHARACTER(LEN=LineLength)::SentData(5)
    SentData(1)=InputData1
    SentData(2)=InputData2
    SentData(3)=InputData3
    SentData(4)=InputData4
    SentData(5)=InputData5

    !Transfer control to master text arranger
    Text=ArrangeTextMaster(SentData,LineLength)

  END FUNCTION Arrange5String


  ! -------------------------------------------------------------
  ! --- FIND THE FIRST OCCURANCE OF A CHARACTER IN A STRING
  ! -------------------------------------------------------------
  FUNCTION FirstLocation(ACharacter,SearchString,Back) RESULT(Location)
    CHARACTER(LEN=1),INTENT(IN)::ACharacter
    CHARACTER(LEN=*),INTENT(IN)::SearchString
    LOGICAL,INTENT(IN),OPTIONAL::Back
    INTEGER::Location

    !Local variables
    LOGICAL::LocalBack
    LocalBack=.FALSE.
    IF (PRESENT(Back)) LocalBack=Back

    Location=0 !Default: 0 means character is not found in the search string 
    Location=SCAN(SearchString,ACharacter,BACK=LocalBack)      

  END FUNCTION FirstLocation


  ! -------------------------------------------------------------
  ! --- FIND THE NUMBER OF OCCURANCES OF A CHARACTER IN A STRING
  ! -------------------------------------------------------------
  FUNCTION CountOccurance(ACharacter,SearchString) RESULT(NumberOfOccurance)
    CHARACTER(LEN=1),INTENT(IN)::ACharacter
    CHARACTER(LEN=*),INTENT(IN)::SearchString
    INTEGER::NumberOfOccurance

    INTEGER::indx

    NumberOfOccurance=0  !Default
    DO indx=1,LEN(SearchString)
      IF (SearchString(indx:indx).EQ.ACharacter) NumberOfOccurance=NumberOfOccurance+1
    END DO
    
  END FUNCTION CountOccurance


  ! -------------------------------------------------------------
  ! --- FIND THE LOCATION OF A SUB-STRING IN A STRING 
  ! -------------------------------------------------------------
  SUBROUTINE FindSubStringInString(SubString,String,BeginLocation,CaseSensitive)
    CHARACTER(LEN=*),INTENT(IN)::SubString,String
    INTEGER,INTENT(OUT)::BeginLocation
    LOGICAL,OPTIONAL,INTENT(IN)::CaseSensitive

    !Local variables
    INTEGER::indx,SubStringLength
    CHARACTER(LEN=LEN(SubString))::LocalSubString
    CHARACTER(LEN=LEN(String))::LocalString
    LOGICAL::LocalCaseSensitive

    !Initialize
    BeginLocation=0
    LocalCaseSensitive=.TRUE. ; IF (PRESENT(CaseSensitive)) LocalCaseSensitive=CaseSensitive
    IF (LocalCaseSensitive) THEN
      LocalSubString=SubString
      LocalString=String
    ELSE
      LocalSubString=UpperCase(SubString)
      LocalString=UpperCase(String)      
    END IF
    SubStringLength=LEN(LocalSubString)

    !Find the beginning location of the sub-string in the string
    DO indx=1,LEN(LocalString)-SubStringLength+1
      IF (LocalString(indx:indx+SubStringLength-1).EQ.LocalSubString) THEN
        BeginLocation=indx
        EXIT
      END IF
    END DO
    
  END SUBROUTINE FindSubStringInString


  ! -------------------------------------------------------------
  ! --- CONVERT A STRING TO UPPERCASE
  ! --- [This function is copied from Ed Akin's book 
  ! ---  (Object-oriented Programming via Fortran 90/95, printed 2003)]
  ! -------------------------------------------------------------
  FUNCTION UpperCase(AString) RESULT(UpperCaseString)
    CHARACTER(LEN=*)::AString
    CHARACTER(LEN=LEN(AString))::UpperCaseString

    CHARACTER(LEN=26), parameter ::UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ,&              
                                   lower = 'abcdefghijklmnopqrstuvwxyz'
    INTEGER :: k    ! Loop counter
    INTEGER :: loc  ! Position in alphabet

    UpperCaseString = AString       ! Copy everything
    DO k = 1, LEN_TRIM(AString)     !  to change letters
      loc = INDEX(lower,AString (k:k))                     ! Locate
      IF (loc.NE.0) UpperCaseString (k:k) = UPPER(loc:loc) ! Convert
    END DO 

  END FUNCTION UpperCase


  ! -------------------------------------------------------------
  ! --- CONVERT A STRING TO LOWERCASE
  ! -------------------------------------------------------------
  FUNCTION LowerCase(AString) RESULT(LowerCaseString)
    CHARACTER(LEN=*)::AString
    CHARACTER(LEN=LEN(AString))::LowerCaseString

    CHARACTER(LEN=26), parameter ::UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ,&              
                                   lower = 'abcdefghijklmnopqrstuvwxyz'
    INTEGER :: k    ! Loop counter
    INTEGER :: loc  ! Position in alphabet

    LowerCaseString = AString       ! Copy everything
    DO k = 1, LEN_TRIM(AString)     !  to change letters
      loc = INDEX(UPPER,AString (k:k))                     ! Locate
      IF (loc.NE.0) LowerCaseString (k:k) = lower(loc:loc) ! Convert
    END DO 

  END FUNCTION LowerCase


  ! -------------------------------------------------------------
  ! --- CONVERT A STRING VERSION OF AN INTEGER INTO "NUMERIC" INTEGER 
  ! -------------------------------------------------------------
  FUNCTION TextToInt(AString) RESULT(Number)
    CHARACTER(LEN=*)::AString
    INTEGER::Number

    CHARACTER(LEN=10), PARAMETER ::IntChar = '0123456789' 
    INTEGER :: k    ! Loop counter
    INTEGER :: loc  ! Position in character version of numbers
    INTEGER :: PowerOfTen

    Number=0  !Initialize
    PowerOfTen=0

    DO k = LEN_TRIM(AString),1,-1      !  to change letters
      loc = INDEX(IntChar,AString (k:k))                   ! Locate
      IF (loc.NE.0) THEN
        loc=loc-1
        Number = Number + loc*(10**PowerOfTen)             ! Calculate
      END IF
      PowerOfTen=PowerOfTen+1
    END DO 

  END FUNCTION TextToInt


  ! -------------------------------------------------------------
  ! --- STRIP TEXT UP TO A CERTAIN CHARACTER FROM A STRING 
  ! -------------------------------------------------------------
  FUNCTION StripTextUntilCharacter(TextIn,WhatCharacter,Back) RESULT(TextOut)
    CHARACTER(LEN=*),INTENT(IN)::TextIn
    CHARACTER(LEN=LEN(TextIn))::TextOut
    CHARACTER(LEN=1)::WhatCharacter
    LOGICAL,INTENT(IN),OPTIONAL::Back

    !Local variables
    INTEGER::Location
    LOGICAL::LocalBack
    LocalBack=.FALSE.
    IF (PRESENT(Back)) LocalBack=Back

    !Default
    TextOut=TextIn

    !Check if the character exists in the TextIn
    Location=FirstLocation(WhatCharacter,TextIn,Back=LocalBack)
    IF (Location.GT.0) TextOut=TextIn(1:Location-1)

  END FUNCTION StripTextUntilCharacter


  ! -------------------------------------------------------------
  ! --- CLEAN SPECIAL CHARACTERS IN A STRING
  ! -------------------------------------------------------------
  SUBROUTINE CleanSpecialCharactersInAString(String)
    CHARACTER(LEN=*)::String

    !Local variables
    INTEGER::indx

    !Replace special characters
    DO indx=1,LEN(String)
      IF (IACHAR(String(indx:indx)).LT.32) String(indx:indx)=' '
    END DO

    END SUBROUTINE CleanSpecialCharactersInAString


  ! -------------------------------------------------------------
  ! --- CLEAN SPECIAL CHARACTERS IN AN ARRAY OF STRINGS
  ! -------------------------------------------------------------
  SUBROUTINE CleanSpecialCharactersInStringArray(StringArray)
    CHARACTER(LEN=*),DIMENSION(:)::StringArray

    !Local variables
    INTEGER::indx

    DO indx=1,SIZE(StringArray)
      CALL CleanSpecialCharactersInAString(StringArray(indx))
    END DO

  END SUBROUTINE CleanSpecialCharactersInStringArray


  ! -------------------------------------------------------------
  ! --- FIND THE STARTING LOCATION OF A DATA COLUMN IN A STRING
  ! -------------------------------------------------------------
  FUNCTION GetStartLocation(String,ColumnNumber) RESULT(Location)
    CHARACTER(LEN=*),INTENT(IN)::String
    INTEGER,INTENT(IN)::ColumnNumber
    INTEGER::Location

    !Local variables
    INTEGER::WorkLocation,ColumnCounter,StringLength
    CHARACTER(LEN=LEN(String))::WorkString
    LOGICAL::IsCurrentLocationEmpty,IsPreviousLocationEmpty

    !Initialize
    Location=0
    WorkString=String

    !Clean string from special characters
    CALL CleanSpecialCharacters(WorkString)
    WorkString=StripTextUntilCharacter(WorkString,'/',Back=.TRUE.)
    StringLength=LEN_TRIM(WorkString)

    WorkLocation=0
    ColumnCounter=0
    IsCurrentLocationEmpty=.TRUE.
    DO
      IsPreviousLocationEmpty=IsCurrentLocationEmpty
      WorkLocation=WorkLocation+1
      IsCurrentLocationEmpty=.FALSE.
      IF (WorkString(WorkLocation:WorkLocation).EQ.' ' .OR.  &
          WorkString(WorkLocation:WorkLocation).EQ.','     ) &
        IsCurrentLocationEmpty=.TRUE.
      IF (IsPreviousLocationEmpty .AND. (.NOT. IsCurrentLocationEmpty)) &
        ColumnCounter=ColumnCounter+1
      IF (ColumnCounter.EQ.ColumnNumber) THEN
        Location=WorkLocation
        EXIT
      END IF
      IF (WorkLocation.EQ.StringLength) EXIT      
    END DO

  END FUNCTION GetStartLocation


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO PREPARE ORGANIZED TITLES GIVEN SEVERAL TITLE LINES
  ! -------------------------------------------------------------
  SUBROUTINE PrepareTitle(Title,TitleLines,TitleLength,TitleStartLocation)
    CHARACTER(LEN=*),INTENT(OUT)::Title
    CHARACTER(LEN=*),INTENT(IN),DIMENSION(:)::TitleLines
    INTEGER,INTENT(IN)::TitleLength,TitleStartLocation

    !Local variables
    INTEGER::indx
    CHARACTER(LEN=TitleStartLocation-1)::Lead
    Lead=ADJUSTL('*')

    !First line
    Title=Lead//REPEAT('*',TitleLength)//NEW_LINE('x')

    !Middle lines
    DO indx=1,SIZE(TitleLines)
      Title=TRIM(Title)//Lead//'*'//TitleLines(indx)(1:TitleLength-2)//'*'//NEW_LINE('x')
    END DO

    !Last line
    Title=TRIM(Title)//Lead//REPEAT('*',TitleLength)

  END SUBROUTINE PrepareTitle  
  




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** ARRAY UTILITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE L2-NORM (PARALLEL OR SEQUENTIAL)
  ! -------------------------------------------------------------
  FUNCTION L2Norm(iDim,dVector) RESULT(dNorm)
    INTEGER,INTENT(IN) :: iDim
    REAL(8),INTENT(IN) :: dVector(iDim)
    REAL(8)            :: dNorm
    
    !Local variables
    INTEGER             :: iNThreads,iThread,indx
    REAL(8),ALLOCATABLE :: dNormThread(:)
    
    !Initialize
    iNThreads    = 1
    iThread      = 1
    !$ iNThreads = OMP_GET_NUM_PROCS() - 1
    
    !Allocate memory for thread-based norm variable
    ALLOCATE (dNormThread(iNThreads))
    dNormThread = 0.0
    
    !Calculate L2-Norm
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(indx,iThread) NUM_THREADS(iNThreads)
    !$ iThread = OMP_GET_THREAD_NUM() + 1
    !$OMP DO SCHEDULE(STATIC,500) 
    DO indx=1,iDim
        dNormThread(iThread) = dNormThread(iThread) + (dVector(indx) * dVector(indx))
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    dNorm = SQRT(SUM(dNormThread))
    
  END FUNCTION L2Norm

  
  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 1-D INTEGER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc1DIntArray(IntArray,ncolumn,SendingProcedure,iStat)
    INTEGER,ALLOCATABLE,DIMENSION(:)::IntArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(IntArray)) DEALLOCATE(IntArray) 
    ALLOCATE (IntArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IntArray=0

  END SUBROUTINE Alloc1DIntArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 1-D REAL ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc1DRealArray(RealArray,ncolumn,SendingProcedure,iStat)
    REAL(8),ALLOCATABLE,DIMENSION(:)::RealArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(RealArray)) DEALLOCATE(RealArray) 
    ALLOCATE (RealArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    RealArray=0.0

  END SUBROUTINE Alloc1DRealArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 1-D LOGICAL ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc1DLogicalArray(LogicalArray,ncolumn,SendingProcedure,iStat)
    LOGICAL,ALLOCATABLE,DIMENSION(:)::LogicalArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(LogicalArray)) DEALLOCATE(LogicalArray) 
    ALLOCATE (LogicalArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    LogicalArray = .FALSE.

  END SUBROUTINE Alloc1DLogicalArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 1-D CHARACTER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc1DCharacterArray(CharacterArray,ncolumn,SendingProcedure,iStat)
    CHARACTER(LEN=*),ALLOCATABLE,DIMENSION(:)::CharacterArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(CharacterArray)) DEALLOCATE(CharacterArray) 
    ALLOCATE (CharacterArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CharacterArray=''

  END SUBROUTINE Alloc1DCharacterArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 2-D CHARACTER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc2DCharacterArray(CharacterArray,nrow,ncolumn,SendingProcedure,iStat)
    CHARACTER(LEN=*),ALLOCATABLE,DIMENSION(:,:)::CharacterArray
    INTEGER,INTENT(IN)::nrow,ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(CharacterArray)) DEALLOCATE(CharacterArray) 
    ALLOCATE (CharacterArray(nrow,ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CharacterArray=''

  END SUBROUTINE Alloc2DCharacterArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 2-D REAL ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc2DRealArray(RealArray,nrow,ncolumn,SendingProcedure,iStat)
    REAL(8),ALLOCATABLE,DIMENSION(:,:)::RealArray
    INTEGER,INTENT(IN)::nrow,ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(RealArray)) DEALLOCATE(RealArray) 
    ALLOCATE (RealArray(nrow,ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    RealArray=0.0

  END SUBROUTINE Alloc2DRealArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 3-D REAL ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc3DRealArray(RealArray,n1,n2,n3,SendingProcedure,iStat)
    REAL(8),ALLOCATABLE,DIMENSION(:,:,:)::RealArray
    INTEGER,INTENT(IN)::n1,n2,n3
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(RealArray)) DEALLOCATE(RealArray) 
    ALLOCATE (RealArray(n1,n2,n3) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    RealArray=0.0

  END SUBROUTINE Alloc3DRealArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR 2-D INTEGER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE Alloc2DIntArray(IntArray,nrow,ncolumn,SendingProcedure,iStat)
    INTEGER,ALLOCATABLE,DIMENSION(:,:)::IntArray
    INTEGER,INTENT(IN)::nrow,ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ALLOCATED(IntArray)) DEALLOCATE(IntArray) 
    ALLOCATE (IntArray(nrow,ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IntArray=0

  END SUBROUTINE Alloc2DIntArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR POINTER TO 1-D LOGICAL ARRAY 
  ! -------------------------------------------------------------
  SUBROUTINE AllocPointerTo1DLogicalArray(LogicalArray,ncolumn,SendingProcedure,iStat)
    LOGICAL,DIMENSION(:),POINTER::LogicalArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ASSOCIATED(LogicalArray)) DEALLOCATE(LogicalArray) 
    ALLOCATE (LogicalArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    LogicalArray = .FALSE.

  END SUBROUTINE AllocPointerTo1DLogicalArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR POINTER TO 1-D REAL ARRAY 
  ! -------------------------------------------------------------
  SUBROUTINE AllocPointerTo1DRealArray(RealArray,ncolumn,SendingProcedure,iStat)
    REAL(8),DIMENSION(:),POINTER::RealArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ASSOCIATED(RealArray)) DEALLOCATE(RealArray) 
    ALLOCATE (RealArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    RealArray=0.0

  END SUBROUTINE AllocPointerTo1DRealArray


  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR POINTER TO 1-D CHARACTER ARRAY 
  ! -------------------------------------------------------------
  SUBROUTINE AllocPointerTo1DCharacterArray(CharacterArray,ncolumn,SendingProcedure,iStat)
    CHARACTER(LEN=*),DIMENSION(:),POINTER::CharacterArray
    INTEGER,INTENT(IN)::ncolumn
    CHARACTER(LEN=*),INTENT(IN)::SendingProcedure
    INTEGER,INTENT(OUT) :: iStat

    !Initialize
    iStat = 0
    
    IF (ASSOCIATED(CharacterArray)) DEALLOCATE(CharacterArray) 
    ALLOCATE (CharacterArray(ncolumn) ,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory in '//TRIM(ADJUSTL(SendingProcedure)),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CharacterArray=''

  END SUBROUTINE AllocPointerTo1DCharacterArray


  ! -------------------------------------------------------------
  ! --- FIND LOCATION OF A COMPONENT IN AN INTEGER 1-D ARRAY
  ! -------------------------------------------------------------
  PURE FUNCTION LocateIntIn1DArray(Item,List) RESULT(Location)
    INTEGER,INTENT(IN) :: Item,List(:)
    INTEGER            :: Location


    DO Location=1,SIZE(List)
      IF (Item .EQ. List(Location)) RETURN
    END DO

    Location = 0

  END FUNCTION LocateIntIn1DArray


  ! -------------------------------------------------------------
  ! --- FIND LOCATION OF A COMPONENT IN A REAL 1-D ARRAY
  ! -------------------------------------------------------------
  FUNCTION LocateRealIn1DArray(Item,List) RESULT(Location)
    REAL(8),INTENT(IN) :: Item,List(:)
    INTEGER            :: Location


    DO Location=1,SIZE(List)
      IF (Item .EQ. List(Location)) RETURN
    END DO

    Location = 0

  END FUNCTION LocateRealIn1DArray


  ! -------------------------------------------------------------
  ! --- FIND LOCATIONS OF A SET OF INTEGER VALUES IN A 1-D INTEGER ARRAY
  ! -------------------------------------------------------------
  PURE FUNCTION LocateIntSetIn1DArray(Item,List) RESULT(Location)
    INTEGER,INTENT(IN) :: Item(:),List(:)
    INTEGER            :: Location(SIZE(Item))

    !Local variables
    INTEGER :: indx

    !Initialize
    Location = 0

    DO indx=1,SIZE(Item)
      Location(indx) = LocateInList(Item(indx),List)
    END DO

  END FUNCTION LocateIntSetIn1DArray


  ! -------------------------------------------------------------
  ! --- NORMALIZE A 1-D REAL ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE NormalizeArray(R)
    REAL(8) :: R(:)

    !Local variables
    REAL(8)::Total

    !Initialize
    Total = SUM(R)

    !Normalize
    IF (Total .NE. 0.0) R = R/Total

  END SUBROUTINE NormalizeArray
  

  ! -------------------------------------------------------------
  ! --- SORT AN INTEGER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE ShellSort_NoSecondArray(a)
    INTEGER :: a(:)
    
    !Local variables
    INTEGER :: i,j,inc,va,n
    
    !Initialize
    n   = SIZE(a)
    inc = 1
    
    DO
      inc = 3*inc + 1
      IF (inc .GT. n) EXIT
    END DO
    
    DO
      inc = inc/3
      DO i=inc+1,n
        va = a(i)
        j = i
 3      IF (a(j-inc) .GT. va) THEN
          a(j) = a(j-inc)
          j    = j-inc
          IF (j .LE. inc) GOTO 4
          GOTO 3
        END IF
 4      a(j) = va 
      END DO
      IF (inc .LE. 1) EXIT
    END DO
    
  END SUBROUTINE ShellSort_NoSecondArray

  
  ! -------------------------------------------------------------
  ! --- SORT AN INTEGER ARRAY ALONG WITH AN ACCOMPANYING INTEGER ARRAY USING SHELL SORT
  ! -------------------------------------------------------------
  SUBROUTINE ShellSort_IntSecondArray(a,b)
    INTEGER :: a(:),b(:)
    
    !Local variables
    INTEGER :: i,j,inc,va,n,ib
    
    !Initialize
    n   = SIZE(a)
    inc = 1
    
    DO
      inc = 3*inc + 1
      IF (inc .GT. n) EXIT
    END DO
    
    DO
      inc = inc/3
      DO i=inc+1,n
        va = a(i)
        ib = b(i)
        j = i
 3      IF (a(j-inc) .GT. va) THEN
          a(j) = a(j-inc)
          b(j) = b(j-inc)
          j    = j-inc
          IF (j .LE. inc) GOTO 4
          GOTO 3
        END IF
4       a(j) = va 
        b(j) = ib
      END DO
      IF (inc .LE. 1) EXIT
    END DO
    
  END SUBROUTINE ShellSort_IntSecondArray

  
  ! -------------------------------------------------------------
  ! --- SORT AN INTEGER ARRAY ALONG WITH AN ACCOMPANYING REAL(8) ARRAY USING SHELL SORT
  ! -------------------------------------------------------------
  SUBROUTINE ShellSort_RealSecondArray(a,b)
    INTEGER :: a(:)
    REAL(8) :: b(:)
    
    !Local variables
    INTEGER :: i,j,inc,va,n
    REAL(8) :: rb
    
    !Initialize
    n      = SIZE(a)
    inc    = 1
    
    DO
      inc = 3*inc + 1
      IF (inc .GT. n) EXIT
    END DO
    
    DO
      inc = inc/3
      DO i=inc+1,n
        va = a(i)
        rb = b(i)
        j = i
 3      IF (a(j-inc) .GT. va) THEN
          a(j) = a(j-inc)
          b(j) = b(j-inc)
          j    = j-inc
          IF (j .LE. inc) GOTO 4
          GOTO 3
        END IF
 4      a(j) = va 
        b(j) = rb
      END DO
      IF (inc .LE. 1) EXIT
    END DO
    
  END SUBROUTINE ShellSort_RealSecondArray

  
  ! -------------------------------------------------------------
  ! --- SORT AN INTEGER ARRAY ALONG WITH AN ACCOMPANYING LOGICAL ARRAY USING SHELL SORT
  ! -------------------------------------------------------------
  SUBROUTINE ShellSort_LogicalSecondArray(a,b)
    INTEGER :: a(:)
    LOGICAL :: b(:)
    
    !Local variables
    INTEGER :: i,j,inc,va,n
    LOGICAL :: lb
    
    !Initialize
    n      = SIZE(a)
    inc    = 1
    
    DO
      inc = 3*inc + 1
      IF (inc .GT. n) EXIT
    END DO
    
    DO
      inc = inc/3
      DO i=inc+1,n
        va = a(i)
        lb = b(i)
        j = i
 3      IF (a(j-inc) .GT. va) THEN
          a(j) = a(j-inc)
          b(j) = b(j-inc)
          j    = j-inc
          IF (j .LE. inc) GOTO 4
          GOTO 3
        END IF
 4      a(j) = va 
        b(j) = lb
      END DO
      IF (inc .LE. 1) EXIT
    END DO
    
  END SUBROUTINE ShellSort_LogicalSecondArray

  
  ! -------------------------------------------------------------
  ! --- SORT AN INTEGER ARRAY ALONG WITH AN ACCOMPANYING CHARACTER ARRAY USING SHELL SORT
  ! -------------------------------------------------------------
  SUBROUTINE ShellSort_CharSecondArray(a,b)
    INTEGER          :: a(:)
    CHARACTER(LEN=*) :: b(:)
    
    !Local variables
    INTEGER                  :: i,j,inc,va,n,iLen
    CHARACTER(:),ALLOCATABLE :: cb
    
    !Initialize
    n    = SIZE(a)
    inc  = 1
    iLen = LEN(b(1))
    ALLOCATE (CHARACTER(LEN=iLen) :: cb)
    
    DO
      inc = 3*inc + 1
      IF (inc .GT. n) EXIT
    END DO
    
    DO
      inc = inc/3
      DO i=inc+1,n
        va = a(i)
        cb = b(i)
        j = i
 3      IF (a(j-inc) .GT. va) THEN
          a(j) = a(j-inc)
          b(j) = b(j-inc)
          j    = j-inc
          IF (j .LE. inc) GOTO 4
          GOTO 3
        END IF
 4      a(j) = va 
        b(j) = cb
      END DO
      IF (inc .LE. 1) EXIT
    END DO
    
  END SUBROUTINE ShellSort_CharSecondArray

  
  ! -------------------------------------------------------------
  ! --- COMPILE UNIQUE COMPONENTS OF AN INTEGER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE GetUniqueArrayComponents(ArrayIn,ArrayOut)
    INTEGER,INTENT(IN)              :: ArrayIn(:)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: ArrayOut(:)
    
    !Local variables
    INTEGER :: N,WorkArray(SIZE(ArrayIn)),indx,iValue
    
    !Initialize
    N            = 1
    WorkArray(1) = ArrayIn(1)
    DO indx=2,SIZE(ArrayIn)
      iValue = ArrayIn(indx)
      IF (.NOT. ANY(WorkArray(1:N) .EQ. iValue)) THEN
        N = N + 1
        WorkArray(N) = iValue
      END IF
    END DO
    
    !Allocate memory for return array and store data
    ALLOCATE (ArrayOut(N))
    ArrayOut = WorkArray(1:N)
    
  END SUBROUTINE GetUniqueArrayComponents
  
  
  ! -------------------------------------------------------------
  ! --- RETRIEVE REAL ARRAY DATA FROM A SOURCE ARRAY; THE SOURCE ARRAY MAY NEED TO BE REWOUND
  ! -------------------------------------------------------------
  SUBROUTINE GetRealArrayData(SourceArray,Array,cProcess,iStat)
    REAL(8),INTENT(IN)          :: SourceArray(:)
    REAL(8),INTENT(INOUT)       :: Array(:)
    CHARACTER(LEN=*),INTENT(IN) :: cProcess
    INTEGER,INTENT(OUT)         :: iStat

    !LOcal variables
    INTEGER :: indx,iSizeSA,iSizeA,iRewound,indxS,indxE
    
    !Initialize
    iStat   = 0
    iSizeSA = SIZE(SourceArray)
    iSizeA  = SIZE(Array)
    
    !How many times the SourceArray will be rowound?
    iRewound = iSizeA/iSizeSA
    
    !MAke sure arrays are matched properly
    IF (iSizeA - iRewound*iSizeSA .GT. 0) THEN
        CALL SetLastMessage('Sizes of source array and destination arrays are not matched properly!',f_iFatal,cProcess)
        iStat = -1
        RETURN
    END IF
    
    !Retrieve data
    DO indx=1,iRewound
      indxE = indx * iSizeSA
      indxS = indxE - iSizeSA + 1
      Array(indxS:indxE) = SourceArray
    END DO
    
  END SUBROUTINE GetRealArrayData


  ! -------------------------------------------------------------
  ! --- RETRIEVE INTEGER ARRAY DATA FROM A SOURCE ARRAY; THE SOURCE ARRAY MAY NEED TO BE REWOUND
  ! -------------------------------------------------------------
  SUBROUTINE GetIntArrayData(SourceArray,Array,cProcess,iStat)
    INTEGER,INTENT(IN)          :: SourceArray(:)
    INTEGER,INTENT(INOUT)       :: Array(:)
    CHARACTER(LEN=*),INTENT(IN) :: cProcess
    INTEGER,INTENT(OUT)         :: iStat
    
    !LOcal variables
    INTEGER :: indx,iSizeSA,iSizeA,iRewound,indxS,indxE
    
    !Initialize
    iStat   = 0
    iSizeSA = SIZE(SourceArray)
    iSizeA  = SIZE(Array)
    
    !How many times the SourceArray will be rowound?
    iRewound = iSizeA/iSizeSA
    
    !MAke sure arrays are matched properly
    IF (iSizeA - iRewound*iSizeSA .GT. 0) THEN
        CALL SetLastMessage('Sizes of source array and destination arrays are not matched properly!',f_iFatal,cProcess)
        iStat = -1
        RETURN
    END IF
    
    !Retrieve data
    DO indx=1,iRewound
      indxE = indx * iSizeSA
      indxS = indxE - iSizeSA + 1
      Array(indxS:indxE) = SourceArray
    END DO
    
  END SUBROUTINE GetIntArrayData


  ! -------------------------------------------------------------
  ! --- RETRIEVE CHARACTER ARRAY DATA FROM A SOURCE ARRAY; THE SOURCE ARRAY MAY NEED TO BE REWOUND
  ! -------------------------------------------------------------
  SUBROUTINE GetCharArrayData(SourceArray,Array,cProcess,iStat)
    CHARACTER(LEN=*),INTENT(IN)    :: SourceArray(:)
    CHARACTER(LEN=*),INTENT(INOUT) :: Array(:)
    CHARACTER(LEN=*),INTENT(IN)    :: cProcess
    INTEGER,INTENT(OUT)            :: iStat
    
    !LOcal variables
    INTEGER :: indx,iSizeSA,iSizeA,iRewound,indxS,indxE
    
    !Initialize
    iStat   = 0
    iSizeSA = SIZE(SourceArray)
    iSizeA  = SIZE(Array)
    
    !How many times the SourceArray will be rowound?
    iRewound = iSizeA/iSizeSA
    
    !MAke sure arrays are matched properly
    IF (iSizeA - iRewound*iSizeSA .GT. 0) THEN
        CALL SetLastMessage('Sizes of source array and destination arrays are not matched properly!',f_iFatal,cProcess)
        iStat = -1
        RETURN
    END IF
    
    !Retrieve data
    DO indx=1,iRewound
      indxE = indx * iSizeSA
      indxS = indxE - iSizeSA + 1
      Array(indxS:indxE) = SourceArray
    END DO
    
  END SUBROUTINE GetCharArrayData
  
  
  ! -------------------------------------------------------------
  ! --- RETRIEVE LOGICAL ARRAY DATA FROM A SOURCE ARRAY; THE SOURCE ARRAY MAY NEED TO BE REWOUND
  ! -------------------------------------------------------------
  SUBROUTINE GetLogicalArrayData(SourceArray,Array,cProcess,iStat)
    LOGICAL,INTENT(IN)          :: SourceArray(:)
    LOGICAL,INTENT(INOUT)       :: Array(:)
    CHARACTER(LEN=*),INTENT(IN) :: cProcess
    INTEGER,INTENT(OUT)         :: iStat
    
    !LOcal variables
    INTEGER :: indx,iSizeSA,iSizeA,iRewound,indxS,indxE
    
    !Initialize
    iStat   = 0
    iSizeSA = SIZE(SourceArray)
    iSizeA  = SIZE(Array)
    
    !How many times the SourceArray will be rowound?
    iRewound = iSizeA/iSizeSA
    
    !Make sure arrays are matched properly
    IF (iSizeA - iRewound*iSizeSA .GT. 0) THEN
        CALL SetLastMessage('Sizes of source array and destination arrays are not matched properly!',f_iFatal,cProcess)
        iStat = -1
        RETURN
    END IF
    
    !Retrieve data
    DO indx=1,iRewound
      indxE = indx * iSizeSA
      indxS = indxE - iSizeSA + 1
      Array(indxS:indxE) = SourceArray
    END DO
    
  END SUBROUTINE GetLogicalArrayData
  
  
  ! -------------------------------------------------------------
  ! --- RETRIEVE REAL ARRAY DATA FROM A STRING DATA; MODIFY STRING DATA AS WELL
  ! -------------------------------------------------------------
  SUBROUTINE GetRealArrayDataFromString(SourceString,rArray,cProcess,iStat)
    CHARACTER(LEN=*)    :: SourceString,cProcess
    REAL(8)             :: rArray(:)
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    INTEGER :: indx,iLoc
    
    !Initialize
    iStat = 0
    
    !Prepare SourceString
    CALL CleanSpecialCharacters(SourceString)
    SourceString = ADJUSTL(StripTextUntilCharacter(SourceString,'/',Back=.TRUE.))
    CALL ReplaceString(SourceString,',',' ',iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Read array
    DO indx=1,SIZE(rArray)
      READ (SourceString,*) rArray(indx)
      iLoc         = FirstLocation(' ',SourceString)
      IF (iLoc .EQ. 0) THEN
          CALL SetLastMessage('Error in data entry for '//TRIM(cProcess)//'!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      SourceString = ADJUSTL(SourceString(iLoc:LEN_TRIM(SourceString)))
    END DO
  
  END SUBROUTINE GetRealArrayDataFromString
  
  
  ! -------------------------------------------------------------
  ! --- RETRIEVE INTEGER ARRAY DATA FROM A STRING DATA; MODIFY STRING DATA AS WELL
  ! -------------------------------------------------------------
  SUBROUTINE GetIntArrayDataFromString(SourceString,iArray,cProcess,iStat)
    CHARACTER(LEN=*)    :: SourceString,cProcess
    INTEGER             :: iArray(:)
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    INTEGER :: indx,iLoc
    
    !Initialize
    iStat = 0
    
    !Prepare SourceString
    CALL CleanSpecialCharacters(SourceString)
    SourceString = ADJUSTL(StripTextUntilCharacter(SourceString,'/',Back=.TRUE.))
    CALL ReplaceString(SourceString,',',' ',iStat)
    IF (iStat .EQ. -1) RETURN

    DO indx=1,SIZE(iArray)
      READ (SourceString,*) iArray(indx)
      iLoc         = FirstLocation(' ',SourceString)
      IF (iLoc .EQ. 0) THEN
          CALL SetLastMessage('Error in data entry for '//TRIM(cProcess)//'!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      SourceString = ADJUSTL(SourceString(iLoc:LEN_TRIM(SourceString)))
    END DO
  
  END SUBROUTINE GetIntArrayDataFromString        
        
    
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DIRECTORY AND FILENAME UTILITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- ESTABLISH FILE NAME WITH ABSOLUTE PATH 
  ! -------------------------------------------------------------
  SUBROUTINE EstablishAbsolutePathFileName(cFileName,cDefaultPath,cAbsPathFileName)
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cDefaultPath
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cAbsPathFileName
    
    !Local variables
    INTEGER :: ErrorCode,iLen
    
    !Deallocate just in case
    DEALLOCATE (cAbsPathFileName ,STAT=ErrorCode)
    
    !Return if no filename is defined
    IF (cFileName .EQ. '') THEN
      ALLOCATE (CHARACTER(1) :: cAbsPathFileName)
      cAbsPathFileName = ''
      RETURN
    END IF
    
    !If the filename is already given as an abolute path...
    IF (IsAbsolutePathname(cFileName)) THEN
      ALLOCATE (CHARACTER(LEN(cFileName)) :: cAbsPathFileName)
      cAbsPathFileName = cFileName
      
    !If the filename is given relative to a default path...
    ELSE
      iLen = LEN_TRIM(cDefaultPath) + LEN_TRIM(cFileName)
      ALLOCATE (CHARACTER(LEN=iLen) :: cAbsPathFileName)
      cAbsPathFileName = TRIM(cDefaultPath) // TRIM(cFileName)
    END IF

  END SUBROUTINE EstablishAbsolutePathFileName
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF THE PATH IS IN WINDOWS STYLE
  ! -------------------------------------------------------------
  FUNCTION IsPathWindowsStyle(cPath) RESULT(lWStyle)
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    LOGICAL                     :: lWStyle
    
    !Local variable
    CHARACTER :: cPathWork*100
        
    !Windows style path
    IF (FirstLocation('\',cPath) .GT. 0) THEN
        lWStyle = .TRUE.    
    !Linux style path
    ELSE IF (FirstLocation('/',cPath) .GT. 0) THEN
        lWStyle = .FALSE.
    !Windows vs Linux style cannot be obtained from cPath, check environment variable PATH
    ELSE
        CALL GET_ENVIRONMENT_VARIABLE('PATH',cPathWork)
        IF (FirstLocation('\',cPathWork) .GT. 0) THEN
            lWStyle = .TRUE.
        ELSE
            lWStyle = .FALSE.
        END IF
    END IF
    
  END FUNCTION IsPathWindowsStyle
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A PATH TO WINDOWS STYLE
  ! -------------------------------------------------------------
  FUNCTION ConvertPathToWindowsStyle(cPath) RESULT(cWPath)
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CHARACTER(LEN=LEN(cPath))   :: cWPath
    
    !Local variables
    INTEGER :: iLoc
    
    !Initialize
    cWPath = cPath
    
    !Convert
    DO 
      iLoc = FirstLocation('/',cWPath)
      IF (iLoc .EQ. 0) EXIT
      cWPath(iLoc:iLoc) = '\'
    END DO
    
  END FUNCTION ConvertPathToWindowsStyle


  ! -------------------------------------------------------------
  ! --- CONVERT A PATH TO LINUX STYLE
  ! -------------------------------------------------------------
  FUNCTION ConvertPathToLinuxStyle(cPath) RESULT(cLxPath)
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CHARACTER(LEN=LEN(cPath))   :: cLxPath
    
    !Local variables
    INTEGER :: iLoc
    
    !Initialize
    cLxPath = cPath
    
    !Convert
    DO 
      iLoc = FirstLocation('\',cLxPath)
      IF (iLoc .EQ. 0) EXIT
      cLxPath(iLoc:iLoc) = '/'
    END DO
    
  END FUNCTION ConvertPathToLinuxStyle

  
  ! -------------------------------------------------------------
  ! --- FIND IF THE PATHNAME TO A FILE IS ABSOLUTE OR RELATIVE
  ! -------------------------------------------------------------
  FUNCTION IsAbsolutePathname(cPathName) RESULT(lAbsolute)
    CHARACTER(LEN=*),INTENT(IN) :: cPathName
    LOGICAL                     :: lAbsolute
    
    !Local variables
    CHARACTER(LEN=LEN(cPathName)) :: cPathName_Local
    CHARACTER(LEN=26),PARAMETER   :: cDriveLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    !Initialize
    lAbsolute       = .FALSE.
    cPathName_Local = cPathName
    
    !Clean cpathName just in case
    CALL CleanSpecialCharacters(cPathName_Local)
    cPathName_Local = ADJUSTL(cPathName_Local)
    
    !Convert pathname to Windows style
    cPathName_Local = ConvertPathToWindowsStyle(cPathName_Local)
    
    !If the path starts with a letter followed with semi-colon, get rid of them to make it equal to Unix style
    IF (FirstLocation(UpperCase(cPathName_Local(1:1)),cDriveLetters) .GT. 0) THEN
      IF (cPathName_Local(2:2) .EQ. ':') THEN
        cPathName_Local = cPathName_Local(3:LEN_TRIM(cPathName_Local))
      END IF
    END IF
    
    !Determine if pathname is absolute
    IF (cPathName_Local(1:1) .EQ. '\') lAbsolute = .TRUE.

  END FUNCTION IsAbsolutePathname
  
  
  ! -------------------------------------------------------------
  ! --- STRIP FILENAME FROM A PATH
  ! -------------------------------------------------------------
  SUBROUTINE StripFileNameFromPath(cPath,cFileName)
    CHARACTER(LEN=*),INTENT(IN)          :: cPath
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER             :: iLoc
    CHARACTER(LEN=1000) :: cName
    
    !Initialize
    cName = cPath
    CALL CleanSpecialCharacters(cName)
    cName = ADJUSTL(cName)
    
    !Convert path to Windows style
    cName = ConvertPathToWindowsStyle(cName)
    
    !Strip filename
    DO 
      iLoc = FirstLocation('\',cName)
      IF (iLoc .EQ. 0) EXIT
      cName = cName(iLoc+1:LEN_TRIM(cName))
    END DO
    ALLOCATE (CHARACTER(LEN_TRIM(cName)) :: cFileName)
    cFileName = cName
        
  END SUBROUTINE StripFileNameFromPath
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECTORY OF A FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetFileDirectory(cFileName,cDir)
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cDir
    
    !Local variables
    CHARACTER :: cDir_Local*1000
    LOGICAL   :: lPathWindowsStyle
    
    !Is this Linux or Windows?
    lPathWindowsStyle = IsPathWindowsStyle(cFileName)
    
    !Convert file to Windows style just to be safe
    cDir_Local = ConvertPathToWindowsStyle(cFileName)
    
    !Directory
    cDir_Local = ADJUSTL(cDir_Local(1:FirstLocation('\',cDir_Local,BACK=.TRUE.)))
    IF (LEN_TRIM(cDir_Local) .EQ. 0) cDir_Local = '.\'
    
    !Allocate return variable and store information
    ALLOCATE (CHARACTER(LEN_TRIM(cDir_Local)) :: cDir)
    cDir = TRIM(cDir_Local)
    
    !Convert to Linux style if necessary
    IF (.NOT. lPathWindowsStyle) cDir = ConvertPathToLinuxStyle(cDir)
   
  END SUBROUTINE GetFileDirectory




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISCELLENEOUS UTILITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET CURRENT DATE
  ! -------------------------------------------------------------
  FUNCTION GetDate() RESULT(TodaysDate)
    CHARACTER(LEN=10)::TodaysDate

    !Local variables
    CHARACTER(LEN=30)::RDATE,RTIME

    CALL DATE_AND_TIME(DATE=RDATE,TIME=RTIME)
    TodaysDate=RDATE(5:6)//'/'//RDATE(7:8)//'/'//RDATE(1:4)

  END FUNCTION GetDate


  ! -------------------------------------------------------------
  ! --- GET CURRENT TIME
  ! -------------------------------------------------------------
  FUNCTION GetTime() RESULT(RightNow)
    CHARACTER(LEN=8)::RightNow

    !Local variables
    CHARACTER(LEN=30)::RDATE,RTIME

    CALL DATE_AND_TIME(DATE=RDATE,TIME=RTIME)
    RightNow=RTIME(1:2)//':'//RTIME(3:4)//':'//RTIME(5:6)

  END FUNCTION GetTime


  ! -------------------------------------------------------------
  ! --- DEFINE ERROR TOLERENCE GIVEN TWO REAL NUMBERS
  ! -------------------------------------------------------------
  FUNCTION Tolerance(R1,R2) RESULT(Toler)
    REAL(8),INTENT(IN)::R1,R2
    REAL(8)::Toler

    !Local variables
    INTEGER::dg1,dg2,prec1,prec2
    REAL(8)::err1,err2,absR1,absR2

    !Initialize
    prec1 = PRECISION(R1)
    prec2 = PRECISION(R2)
    absR1 = ABS(R1)
    absR2 = ABS(R2)

    !Find the number of digits on the left side of decimal point for R1
    DO dg1=prec1,1,-1
      IF (INT(absR1/10d0**(dg1-1)) .GT. 0d0) EXIT
    END DO

    !Find the number of digits on the left side of decimal point for R2
    DO dg2=prec2,1,-1
      IF (INT(absR2/10d0**(dg2-1)) .GT. 0d0) EXIT
    END DO

    !Tolernace for each number
    err1 = 10d0**(-(prec1-dg1+1))
    err2 = 10d0**(-(prec2-dg2+1))

    !Final tolerance is the larger of the two tolerances
    Toler = MAX(err1,err2)

  END FUNCTION Tolerance
  

  ! -------------------------------------------------------------
  ! --- FAST EXP FUNCTION
  ! --- (Based on Schraudolph, 1999. A fast, compact approximation of the Exp function
  ! -------------------------------------------------------------
  PURE FUNCTION FEXP(rArg) RESULT(rExp)
    REAL(8),INTENT(IN) :: rArg
    REAL(8)            :: rExp
    
    !Local variables
    INTEGER(8) :: i8
    REAL(8)    :: r8
    EQUIVALENCE (r8,i8)
    
    IF (rArg .LT. -500.0) THEN
        rExp = 0.0
    ELSEIF (rArg .GT. 500.0) THEN
        rExp = HUGE(0d0)
    ELSE
        i8   = 6497320848556798 * rArg + 4607182418800017408
        rExp = r8
    END IF
    
  END FUNCTION FEXP
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT FEATURE IDs TO FEATURE INDICES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertID_To_Index_Array(IDs,AllIDs,Indices)
    INTEGER,INTENT(IN)  :: IDs(:),AllIDs(:)
    INTEGER,INTENT(OUT) :: Indices(:)
    
    !Local variables
    INTEGER :: indx,indx1
    
    !Initialize
    Indices = 0
    
    DO indx=1,SIZE(IDs)
        IF (IDs(indx) .EQ. 0) CYCLE
        
        DO indx1=1,SIZE(AllIDs)
            IF (IDs(indx) .EQ. AllIDs(indx1)) THEN
                Indices(indx) = indx1
                EXIT
            END IF
        END DO
    END DO

  END SUBROUTINE ConvertID_To_Index_Array
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT SINGLE FEATURE ID TO FEATURE INDEX
  ! -------------------------------------------------------------
  SUBROUTINE ConvertID_To_Index_Scalar(ID,AllIDs,Index)
    INTEGER,INTENT(IN)  :: ID,AllIDs(:)
    INTEGER,INTENT(OUT) :: Index
    
    !Local variables
    INTEGER :: indx
    
    !Initialize
    Index = 0
    
    IF (ID .EQ. 0) RETURN
    
    DO indx=1,SIZE(AllIDs)
        IF (ID .EQ. AllIDs(indx)) THEN
            Index = indx
            EXIT
        END IF
    END DO
    
  END SUBROUTINE ConvertID_To_Index_Scalar

  
END MODULE