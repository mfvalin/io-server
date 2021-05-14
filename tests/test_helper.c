#include <sys/ipc.h>
#include <sys/shm.h>

//C_StArT
#include <stdint.h>
//C_EnD

//F_StArT
//interface
//F_EnD

//F_StArT
// !> allocate a shared memory segment<br>
// !> ptr = memory_allocate_shared(shmid, size)
// function memory_allocate_shared(shmid, size) result(ptr) BIND(C,name='memory_allocate_shared')
//   import :: C_PTR, C_INT, C_INT64_T
//   integer(C_INT), intent(OUT) :: shmid           !< shared memory id of segment (set by memory_allocate_shared) (see shmget)
//   integer(C_INT64_T), intent(IN), value :: size  !< size of segment in bytes
//   type(C_PTR) :: ptr                             !< local address of memory segment
// end function memory_allocate_shared
//
//F_EnD
//C_StArT
//! allocate a shared memory segment<br>
//! ptr = memory_allocate_shared(shmid, size)
//! @return local address of memory block
void *memory_allocate_shared(
  int *shmid,                 //!< [out] shared memory id of segment (set by memory_allocate_shared) (see shmget)
  uint64_t size               //!< [in]  size of segment in 32 bit units
  )
//C_EnD
{
  int id = -1;
  void *shmaddr = NULL;
  size_t shmsz = size ;                     // size in bytes of memory segment
  int err;
  struct shmid_ds dummy;

  id = shmget(IPC_PRIVATE, shmsz, 0600);    // get a memory block, only accessible by user
  *shmid = id;                              // shared memory block id returned to caller
  if(id == -1) return NULL;                 // miserable failure

  shmaddr = shmat(id, NULL, 0);             // get local address of shared memory block
  if(shmaddr == NULL) return NULL;          // miserable failure

  err = shmctl(id, IPC_RMID, &dummy);       // mark block "to be deleted when no process attached"
  if(err == -1) {                           // miserable failure
    err = shmdt(shmaddr);
    return NULL;
  }

  return shmaddr;     // return local address of memory block
}

//F_StArT
// !> get memory address associated with shared memory segment id<br>
// !> ptr = memory_address_from_id(shmid)
// function memory_address_from_id(shmid) result(ptr) BIND(C,name='memory_address_from_id')
//   import :: C_PTR, C_INT
//   integer(C_INT), intent(IN), value :: shmid           !< shared memory id of segment (see shmget)
//   type(C_PTR) :: ptr                             !< local memory addres of shared memory segment
// end function memory_address_from_id
//
//F_EnD
//C_StArT
//! get memory address associated with shared memory segment id<br>
//! ptr = memory_address_from_id(shmid)
//! @return local memory addres of shared memory segment
void *memory_address_from_id(
  int shmid                  //!< [in] shared memory id of segment (see shmget)
  )
//C_EnD
{
  void *p = shmat(shmid, NULL, 0) ;
//   printf("shmid = %d, address = %p\n",shmid, p);
  return p;
}


//F_StArT
//end interface
//F_EnD
