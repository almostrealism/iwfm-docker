#include <stdio.h>
#ifdef _MSC_VER
	#include <windows.h>
	#include <io.h>
	#include <sys/locking.h>
	#define F_UNLCK 0
	#define F_RDLCK 1
	#define F_WRLCK 2
	typedef __int32 int32_t;
	typedef __int64 int64_t;
#else
	#include <sys/types.h>
	#include <unistd.h>
	#include <fcntl.h>
	#include <stdint.h>
#endif
 
#define UNLOCK      0
#define LOCK_WAIT   1
#define LOCK_NOWAIT 2
#define TEST_LOCK   3

extern "C" {   
	
#if (defined(__SUNPRO_CC) && defined(__linux))
//
// Sun Studio 12 C++ on 32-bit Linux doesn't support large file operations and
// Sun Studio 12 C++ on 64-bit Linux doesn't use special 64-bit names
// 
	extern int64_t lseek64(int32_t ihandl, int64_t iofset, int32_t iorigin);
	#define lockf64 lockf
#endif


#ifdef _MSC_VER
	int32_t lockFile64(int32_t ihandle, int32_t mode, int64_t position, int64_t nbytes)
	{
		int32_t status = 0;
		LARGE_INTEGER start, length;
		OVERLAPPED ovl;
	
		start.QuadPart = position;
		length.QuadPart = nbytes;
	
		ovl.Offset = start.LowPart;
		ovl.OffsetHigh = start.HighPart;
		ovl.hEvent = 0;

		switch (mode) {
	
			case UNLOCK :
				status = UnlockFileEx(
					(HANDLE)ihandle, 
					0,
					length.LowPart, 
					length.HighPart,
					&ovl);
				break;
	
			case LOCK_WAIT :
				status = LockFileEx(
					(HANDLE)ihandle, 
					LOCKFILE_EXCLUSIVE_LOCK,
					0,
					length.LowPart, 
					length.HighPart,
					&ovl);
				if (!status) printf("\nError: Lock Failed:\n");
				break;
	
			case LOCK_NOWAIT :
				status = LockFileEx(
					(HANDLE)ihandle, 
					LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY,
					0,
					length.LowPart, 
					length.HighPart,
					&ovl);
				break;
	
			case TEST_LOCK :
				status = LockFileEx(
					(HANDLE)ihandle, 
					LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY,
					0,
					length.LowPart, 
					length.HighPart,
					&ovl);
				if (status) UnlockFileEx(
					(HANDLE)ihandle, 
					0,
					length.LowPart, 
					length.HighPart,
					&ovl);
				break;
	
			default :
				printf("\nInvalid lock mode: %d\n", mode);
		}
		
		return status ? 0 : -1;
	}
#else
	int32_t lockFile64(int32_t ihandle, int32_t mode, int64_t position, int64_t nbytes)
	{
		int32_t status = -1;
		int64_t original_offset = lseek64(ihandle, position,  SEEK_SET);

		switch (mode) {

			case UNLOCK :
				status = lockf64(ihandle, F_ULOCK, nbytes);
				break;

			case LOCK_WAIT :
				status = lockf64(ihandle, F_LOCK, nbytes);
				if (status) printf("\nError: Lock Failed:\n");
				break;

			case LOCK_NOWAIT :
				status = lockf64(ihandle, F_TLOCK, nbytes);
				break;

			case TEST_LOCK :
				status = lockf64(ihandle, F_TEST, nbytes);
				break;

			default :
				printf("\nInvalid lock mode: %d\n", mode);
		}

		lseek64(ihandle, original_offset, SEEK_SET);
		return status;
	}
#endif

void lockdss64_(int32_t *ihandle, int32_t *mode, int64_t *position, int64_t *nbytes, int32_t *istat) {
	
	*istat = lockFile64(*ihandle, *mode, *position, *nbytes);
}

int32_t lockFile(int32_t ihandle, int32_t mode, int32_t position, int32_t nbytes) {

	return lockFile64(ihandle, mode,(int64_t)position, (int64_t)nbytes);
}

void lockdss_(int32_t *ihandle, int32_t *mode, int32_t *position, int32_t *nbytes, int32_t *istat) {
	
	*istat = lockFile(*ihandle, *mode, *position, *nbytes);
}

void lockf_(int32_t *ihandle, int32_t *lock, int32_t *istat) {

	int32_t mode = *lock ? LOCK_WAIT : UNLOCK;
#ifdef _MSC_VER	
	LARGE_INTEGER size;
	GetFileSizeEx((HANDLE)*ihandle, &size);
	*istat = lockFile64(*ihandle,mode, 0, size.QuadPart);
#else
	*istat = lockFile(*ihandle,mode, 0, 0);
#endif	
}

int32_t lockfx(int32_t ihandle, int32_t mode) {
	
#ifdef _MSC_VER
	const DWORD RDLCK_FLAGS = LOCKFILE_FAIL_IMMEDIATELY;
	const DWORD WRLCK_FLAGS = LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK;
	LARGE_INTEGER lck_pos, original_pos, new_pos;
	OVERLAPPED ovl;
	int32_t istat = 0;
	lck_pos.QuadPart = 0ll;
	
	SetFilePointerEx((HANDLE)ihandle, lck_pos, &original_pos, FILE_CURRENT);
	switch (mode) {
		case F_WRLCK :
			SetFilePointerEx((HANDLE)ihandle, lck_pos, &new_pos, FILE_BEGIN);
			memset(&ovl, 0, sizeof(ovl));
			UnlockFileEx((HANDLE)ihandle, 0, 1, 0, &ovl);
			memset(&ovl, 0, sizeof(ovl));
			istat = LockFileEx((HANDLE)ihandle, WRLCK_FLAGS, 0, 1, 0, &ovl) ? 0 : -1;
			break;
			
		case F_RDLCK :
			SetFilePointerEx((HANDLE)ihandle, lck_pos, &new_pos, FILE_BEGIN);
			memset(&ovl, 0, sizeof(ovl));
			UnlockFileEx((HANDLE)ihandle, 0, 1, 0, &ovl);
			memset(&ovl, 0, sizeof(ovl));
			istat = LockFileEx((HANDLE)ihandle, RDLCK_FLAGS, 0, 1, 0, &ovl) ? 0 : -1;
			break;
			
		case F_UNLCK :
			SetFilePointerEx((HANDLE)ihandle, lck_pos, &new_pos, FILE_BEGIN);
			memset(&ovl, 0, sizeof(ovl));
			istat = UnlockFileEx((HANDLE)ihandle, 0, 1, 0, &ovl) ? 0 : -1;
			break;

		default :
			istat = -1;
			printf("\nInvalid lock mode: %d\n", mode);
			break;
	}
	SetFilePointerEx((HANDLE)ihandle, original_pos, &new_pos, FILE_BEGIN);
	return istat;
#else
#if defined(__linux__)
	struct flock lockbuf = {mode, SEEK_SET, 0, 0, 0};
#else	
	struct flock lockbuf = {mode, SEEK_SET, 0, 0, 0, 0};
#endif
	return fcntl(ihandle, F_SETLK, &lockbuf);
#endif
}

void lockfr_(int32_t *ihandle, int32_t *istat) {*istat = lockfx(*ihandle, F_RDLCK);}

void lockfw_(int32_t *ihandle, int32_t *istat) {*istat = lockfx(*ihandle, F_WRLCK);}

void lockfu_(int32_t *ihandle, int32_t *istat) {*istat = lockfx(*ihandle, F_UNLCK);}

}
