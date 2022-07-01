!==============================================================
!
! SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
! http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
!
! Copyright 2016 Intel Corporation
!
! THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
!
! =============================================================
!
! Add this source to your project to assist with debugging a coarray application.
! For further details, see 
!    https://software.intel.com/en-us/articles/how-to-debug-fortran-coarray-applications-on-windows
!
    
module Coarray_Debugging
    implicit none
    
    logical :: Global_Debug_Flag[*] ! Used to synchronize across images
    
    contains
    
    subroutine Enable_Coarray_Debugging ()
    use kernel32
    use psapi
    use ifport, only: SLEEPQQ
    implicit none
    
    ! Local variables
    integer(DWORD) :: pid
    character(MAX_PATH) :: base_file_name
    integer(DWORD) :: base_name_len
    logical :: Local_Debug_Flag
    
    ! On image 1, initialize Global_Debug_Flag. All images will
    ! wait until this becomes .TRUE.
    if (THIS_IMAGE() == 1) Global_Debug_Flag = .FALSE.
    
    ! Everyone wait for initializaion to complete
    sync all
    
    ! Display image number, image name and process ID on standard
    ! output (will be viewable in the Output pane of the debugger
    Local_Debug_Flag = .false.
    pid = GetCurrentProcessId()
    base_name_len = len(base_file_name)
    base_name_len = GetModuleBaseName(GetCurrentProcess(),NULL,base_file_name,base_name_len)
    write(*,'("Image ",I0,": ",A,"(",I0,")")') THIS_IMAGE(),base_file_name(1:base_name_len),pid
    
    ! Wait until the global flag has been set to .TRUE. This will happen when
    ! the user sets Local_Debug_Flag in any image to .TRUE.
    do while (.not. Global_Debug_Flag[1])

        ! Set a breakpoint on the following line. The debugger will stop here
        ! once you have attached to the process.
        
        call SLEEPQQ(500) ! <<<<<<< SET BREAKPOINT HERE
        
        sync memory ! Update global state
        
        ! When you have finished setting all the desired breakpoints in
        ! this image, use the debugger's Locals variable display to
        ! change the value of Local_Debug_Flag to .TRUE. and then continue
        ! execution. This will cause all images to exit from this routine 
        ! resume execution of the application. Note that only the image
        ! currently attached to will stop at breakpoints.
        if (Local_Debug_Flag) Global_Debug_Flag[1] = Local_Debug_Flag
    end do
    
    return
    end subroutine Enable_Coarray_Debugging
    
end module Coarray_Debugging
    