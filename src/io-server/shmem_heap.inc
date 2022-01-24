! Copyright (C) 2022  Environnement et Changement climatique Canada
! 
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
! 
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
! 
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

! This file has been generated from shmem_heap.c

!******************************************************************************
!        INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
!  --------------------------------------------------------------------------
! We use the '//F_StArT' and '//F_EnD' tags to indicate the beginning and end
! of extraction. Anything that happens to be between these tags will be
! included in the output, with the leading '//' and trailing spaces removed.
!
!******************************************************************************
!     INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A FORTRAN FILE
!  --------------------------------------------------------------------------
! Same as for .C files, except that we use the '!! F_StArT' and '!! F_EnD'
! tags. Don't forget to include parameter declarations too.
!
!******************************************************************************

interface
subroutine ShmemHeap_lock(heap) bind(C, name='ShmemHeap_lock_f')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: heap
end subroutine ShmemHeap_lock
subroutine ShmemHeap_unlock(heap) bind(C, name='ShmemHeap_unlock_f')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: heap
end subroutine ShmemHeap_unlock
function Pointer_offset(ref, to, szeof) result(offset) bind(C,name='Pointer_offset')
import :: C_INTPTR_T, C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value    :: ref
type(C_PTR), intent(IN), value    :: to
integer(C_INT), intent(IN), value :: szeof
integer(C_INTPTR_T)               :: offset
end function Pointer_offset
function Pointer_add_offset(ref, offset, szeof) result(to) bind(C,name='Pointer_add_offset')
import :: C_INTPTR_T, C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value          :: ref
integer(C_INTPTR_T), intent(IN), value  :: offset
integer(C_INT), intent(IN), value       :: szeof
type(C_PTR)                             :: to
end function Pointer_add_offset
subroutine ShmemHeap_dump_info(heap) bind(C,name='ShmemHeap_dump_info')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: heap
end subroutine ShmemHeap_dump_info
function ShmemHeap_get_info(heap, sz, max, nblk, nbyt) result(status) bind(C,name='ShmemHeap_get_info')
import :: C_INT, C_LONG_LONG, C_PTR
implicit none
type(C_PTR), intent(in), value    :: heap
integer(C_LONG_LONG), intent(OUT) :: sz
integer(C_LONG_LONG), intent(OUT) :: max
integer(C_LONG_LONG), intent(OUT) :: nblk
integer(C_LONG_LONG), intent(OUT) :: nbyt
integer(C_INT) :: status
end function ShmemHeap_get_info
function ShmemHeap_get_size(heap) result(s) bind(C,name='ShmemHeap_get_size')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
integer(HEAP_ELEMENT) :: s
end function ShmemHeap_get_size
function ShmemHeap_is_block_valid(heap, block) result(status) bind(C,name='ShmemHeap_is_block_valid')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: heap
type(C_PTR), intent(IN), value :: block
integer(C_INT) :: status
end function ShmemHeap_is_block_valid
function ShmemHeap_ptr_from_offset(heap, offset) result(p) bind(C,name='ShmemHeap_ptr_from_offset')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
integer(HEAP_ELEMENT), intent(IN), value :: offset
type(C_PTR) :: p
end function ShmemHeap_ptr_from_offset
function ShmemHeap_block_size_from_pointer(heap, block) result(num_bytes) bind(C, name='ShmemHeap_block_size_from_pointer')
import :: C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(in), value :: heap
type(C_PTR), intent(in), value :: block
integer(C_SIZE_T) :: num_bytes
end function ShmemHeap_block_size_from_pointer
function ShmemHeap_block_size_from_offset(heap, offset) result(num_bytes) bind(C, name='ShmemHeap_block_size_from_offset')
import :: C_PTR, HEAP_ELEMENT, C_SIZE_T
implicit none
type(C_PTR), intent(in), value :: heap
integer(HEAP_ELEMENT), intent(in), value :: offset
integer(C_SIZE_T) :: num_bytes
end function ShmemHeap_block_size_from_offset
function ShmemHeap_offset_from_pointer(heap, block) result(offset) bind(C, name='ShmemHeap_offset_from_pointer')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
type(C_PTR), intent(IN), value :: block
integer(HEAP_ELEMENT) :: offset
end function ShmemHeap_offset_from_pointer
function ShmemHeap_init_from_scratch(heap, nbytes) result(h) bind(C,name='ShmemHeap_init_from_scratch')
import :: C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_SIZE_T), intent(IN), value :: nbytes
type(C_PTR) :: h
end function ShmemHeap_init_from_scratch
function ShmemHeap_clone(shmem_ptr) result(status) bind(C, name='ShmemHeap_clone')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: shmem_ptr
type(C_PTR) :: status
end function ShmemHeap_clone
subroutine ShmemHeap_delete(heap) bind(C, name='ShmemHeap_delete')
import :: C_PTR
implicit none
type(C_PTR), intent(in), value :: heap
end subroutine ShmemHeap_delete
function ShmemHeap_check(heap, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeap_check')
import :: C_INT, C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
integer(C_SIZE_T), intent(OUT) :: free_space, used_space
integer(C_INT) :: status
end function ShmemHeap_check
function ShmemHeap_alloc_block(heap, nbytes, safe) result(b) bind(C,name='ShmemHeap_alloc_block')
import :: C_INT, C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_SIZE_T), intent(IN), value :: nbytes
integer(C_INT), intent(IN), value :: safe
type(C_PTR) :: b
end function ShmemHeap_alloc_block
function ShmemHeap_set_block_meta(heap, block, block_metadata, block_meta_size) result(status) bind(C,name='ShmemHeap_set_block_meta')
import :: C_PTR, C_INT, block_meta_c
implicit none
type(C_PTR), intent(IN), value    :: heap
type(C_PTR), intent(IN), value    :: block
type(block_meta_c), intent(IN)    :: block_metadata
integer(C_INT), intent(IN), value :: block_meta_size
integer(C_INT) :: status
end function ShmemHeap_set_block_meta
function ShmemHeap_get_block_meta(heap, block, metadata, msz) result(status) bind(C,name='ShmemHeap_get_block_meta')
import :: C_PTR, C_INT, block_meta_c
implicit none
type(C_PTR), intent(IN), value    :: heap
type(C_PTR), intent(IN), value    :: block
type(block_meta_c), intent(OUT)    :: metadata
integer(C_INT), intent(IN), value :: msz
integer(C_INT) :: status
end function ShmemHeap_get_block_meta
function ShmemHeap_free_block(heap, block) result(status) bind(C,name='ShmemHeap_free_block')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: heap
type(C_PTR), intent(IN), value :: block
integer(C_INT) :: status
end function ShmemHeap_free_block
end interface
