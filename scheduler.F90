! Author: Victor Anisimov, NCSA Blue Waters, University of Illinois at Urbana-Champaign
! send bug reports and feature requests to anisimov@illinois.edu
program scheduler
  implicit none

  include 'mpif.h'
  integer :: mpierror, nMPIprocs, iam, status(MPI_STATUS_SIZE)
  integer, parameter :: READY=1, JOBINFO=2
  integer, parameter :: iunit = 22
  integer :: i, child
  integer :: nJobs
  logical :: newJob, exists

  integer, parameter     :: dnSize = 4086, fnSize = 4086    ! cwdir name and file name size
  character (len=dnSize) :: cwd, jobDir, executable, execute
  character (len=fnSize) :: jobName, bFormat, masterJobFile
  integer, parameter     :: bSize = 32 + dnSize + fnSize
  character (len=bSize)  :: buffer, str
  character (len=32)     :: sFormat
  integer                :: iArg, returnCode
  logical                :: stdout = .TRUE., exitOnError = .TRUE.
  character (len=512)    :: keyword
  real (kind=8)          :: tstart, tend

  integer :: chdir, err, iargc

  call MPI_Init(mpierror)
  call MPI_Comm_size(MPI_COMM_WORLD, nMPIprocs, mpierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, iam, mpierror)

  if(iam == 0) then
    ! check command-line arguments
    if(iargc() < 2 .or. iargc() > 4) then
      call getarg(0,cwd)
      write(*,*) "Usage: ",trim(cwd), " masterJobFile fullPathToExecutable [-nostdout] [-noexit]"
      call MPI_ABORT(MPI_COMM_WORLD, mpierror)
    endif

    ! check presence of master file
    call getarg(1,masterJobFile)
    inquire(file=masterJobFile, exist=exists)
    if(.not.exists) then
      write(*,*) "ERROR: File ",trim(masterJobFile)," doesn't exist"
      call MPI_ABORT(MPI_COMM_WORLD, mpierror)
    endif

    ! get number of jobs
    open(unit=iunit, file=masterJobFile, status='OLD', form='FORMATTED')
    nJobs = 0
    do while(.true.)
      read(iunit,*,end=10) str
      nJobs = nJobs + 1
    enddo
    10 close(iunit)
    if(nJobs == 0) then
      write(*,*) "ERROR: no jobs to run"
      call MPI_ABORT(MPI_COMM_WORLD, mpierror)
    endif

    ! get current working directory name
    call getcwd(cwd)

    ! format to read masterJobFile
    write(sFormat,"('(',i0,'A)')") bSize  ! format to read masterJobFile
  endif

  ! format of data in buffer: newJob dirName fileName (logical string string)
  write(bFormat,"('(L,1X,',i0,'A,1X,',i0,'A)')") dnSize, fnSize

  ! check presence of executable
  call getarg(2,executable)
  inquire(file=executable, exist=exists)
  if(.not.exists) then
    write(*,*) "ERROR: File ",trim(executable)," doesn't exist"
    call MPI_ABORT(MPI_COMM_WORLD, mpierror)
  endif

  ! check presence of command-line options
  do iArg = 3, iargc()
    call getarg(iArg,keyword)
    if(index(keyword,"-nostdout") > 0) then
      stdout = .FALSE.
    endif
    if(index(keyword,"-noexit") > 0) then
      exitOnError = .FALSE.
    endif
  enddo

  if(iam == 0) then
    !
    ! Master process
    !
    open(unit=iunit, file=masterJobFile, status='OLD', form='FORMATTED')
    do i = 1, nJobs + nMPIprocs - 1
      !
      if(i <= nJobs) then
        read(iunit,sFormat,end=20) str
        call readSubString(1, str,bSize, jobDir, dnSize)
        call readSubString(2, str,bSize, jobName,fnSize)
        newJob = .true.
      else
        newJob = .false.
      endif
      !
      ! wait for child to signal ready
      call MPI_Recv(child, 1, MPI_INTEGER, MPI_ANY_SOURCE, READY, MPI_COMM_WORLD, status, mpierror)
      !
      ! prepare new job
      ! do not prepend cwd if path starts from /
      if(jobDir(1:1) /= '/') then
        jobDir = trim(cwd) // "/" // trim(jobDir)
      endif
      write(buffer,bFormat) newJob, jobDir, jobName
      !
      ! send new job to the child
      call MPI_SSend(buffer, bSize, MPI_CHARACTER, child, JOBINFO, MPI_COMM_WORLD, mpierror)
    enddo
    close(iunit)

    write(*,*) iam," Finished normally"
    call MPI_Finalize(mpierror)

  else
    !
    ! Slave process
    !
    newJob = .true.  ! expect to receive new job from master
    do while (newJob)
      !
      ! send ready signal
      call MPI_SSend(iam, 1, MPI_INTEGER, 0, READY, MPI_COMM_WORLD, mpierror)
      !
      ! get job to do
      call MPI_Recv(buffer, bSize, MPI_CHARACTER, 0, JOBINFO, MPI_COMM_WORLD, status, mpierror)
      !
      ! do the job
      read(buffer,bFormat) newJob, jobDir, jobName

      if(newJob) then

        err = chdir(jobDir)
        if(err /= 0) then
          write(*,'(a,i0,a,a)') "ERROR: ", iam, " cannot change directory to ", trim(jobDir)
        else
          if(stdout) then
            execute = trim(executable) // " " // trim(jobName) // " > " // trim(jobName) // ".slog"
          else
            execute = trim(executable) // " " // trim(jobName) // " > /dev/null"
          endif
          write(*,'(i0,2x,a,2x,a)') iam, trim(jobDir), trim(execute)
          tstart = MPI_Wtime()
          call system(execute, returnCode)
          if(returnCode /= 0) then
            write(*,'(a,i0,a,a,a,a)') "ERROR: process ",iam," failed in ",trim(jobDir),"  ",trim(execute)
            if(exitOnError) then
              call exit(returnCode)
            endif
          endif
          tend = MPI_Wtime()
          write(*,'(a,a,f10.2)') trim(jobDir),":_seconds_:",tend-tstart
        endif

      endif
      !
    enddo

    write(*,*) iam," Finished normally"
    call MPI_Finalize(mpierror)
  endif

  stop ! All Done

  20 write(*,*) "ERROR reading ", trim(masterJobFile)
  call MPI_ABORT(MPI_COMM_WORLD, mpierror)

end program scheduler


subroutine readSubString(nss, str,sSize, substr,ssSize)
  implicit none
  integer, intent(in) :: nss  ! substring to read
  integer, intent(in) :: sSize, ssSize
  character (len=sSize),  intent(in)  :: str
  character (len=ssSize), intent(out) :: substr
  integer :: i,j,k

  i=0
  k=1
  substr=" "

  do while(k <= sSize .and. i < nss)
    ! skip leading blanks
    do while(k <= sSize .and. index(str(k:k)," ") == 1)
      k = k + 1
    enddo
    if(k <= sSize .and. index(str(k:k)," ") == 0) i = i + 1
    if(i == nss) then
      ! read substring
      j=1
      do while(k <= sSize .and. index(str(k:k)," ") == 0)
        substr(j:j) = str(k:k)
        j = j + 1
        k = k + 1
      enddo
    else
      ! skip substring
      do while(k <= sSize .and. index(str(k:k)," ") == 0)
        k = k + 1
      enddo
    endif
  enddo

end subroutine readSubString
