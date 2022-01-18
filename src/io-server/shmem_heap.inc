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
subroutine ShmemHeapDumpInfo() bind(C,name='ShmemHeapDumpInfo')
implicit none
end subroutine ShmemHeapDumpInfo
function ShmemHeapGetInfo(ix, sz, max, nblk, nbyt) result(status) bind(C,name='ShmemHeapGetInfo')
import :: C_INT, C_LONG_LONG
implicit none
integer(C_INT), intent(IN), value :: ix
integer(C_LONG_LONG), intent(OUT) :: sz
integer(C_LONG_LONG), intent(OUT) :: max
integer(C_LONG_LONG), intent(OUT) :: nblk
integer(C_LONG_LONG), intent(OUT) :: nbyt
integer(C_INT) :: status
end function ShmemHeapGetInfo
function ShmemHeapGetDefault() result(h) bind(C,name='ShmemHeapGetDefault')
import :: C_PTR
implicit none
type(C_PTR) :: h
end function ShmemHeapGetDefault
function ShmemHeapSetDefault(heap) result(ix) bind(C,name='ShmemHeapSetDefault')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_INT) :: ix
end function ShmemHeapSetDefault
function ShmemHeapIndex(heap) result(ix) bind(C,name='ShmemHeapIndex')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_INT) :: ix
end function ShmemHeapIndex
function ShmemHeapSize(heap) result(s) bind(C,name='ShmemHeapSize')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
integer(HEAP_ELEMENT) :: s
end function ShmemHeapSize
function ShmemHeapContains(addr) result(p) bind(C,name='ShmemHeapContains')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: addr
type(C_PTR) :: p
end function ShmemHeapContains
function ShmemHeapPtrToOffset(addr) result(offset) bind(C,name='ShmemHeapPtrToOffset')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: addr
integer(HEAP_ELEMENT) :: offset
end function ShmemHeapPtrToOffset
function ShmemHeapValidBlock(addr) result(status) bind(C,name='ShmemHeapValidBlock')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: addr
integer(C_INT) :: status
end function ShmemHeapValidBlock
function ShmemHeapBlockSize(heap, addr, offset) result(bsz) bind(C,name='ShmemHeapBlockSize')
import :: C_PTR, C_SIZE_T, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
type(C_PTR), intent(IN), value :: addr
integer(HEAP_ELEMENT), intent(IN) :: offset
integer(C_SIZE_T) :: bsz
end function ShmemHeapBlockSize
function ShmemHeapPtr(heap, offset) result(p) bind(C,name='ShmemHeapPtr')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
integer(HEAP_ELEMENT), intent(IN), value :: offset
type(C_PTR) :: p
end function ShmemHeapPtr
function ShmemHeapOffsetFromPtr(heap, block) result(offset) bind(C, name='ShmemHeapOffsetFromPtr')
import :: C_PTR, HEAP_ELEMENT
implicit none
type(C_PTR), intent(IN), value :: heap
type(C_PTR), intent(IN), value :: block
integer(HEAP_ELEMENT) :: offset
end function ShmemHeapOffsetFromPtr
function ShmemHeapRegister(heap) result(status) bind(C,name='ShmemHeapRegister')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_INT) :: status
end function ShmemHeapRegister
function ShmemHeapInit(heap, nbytes) result(h) bind(C,name='ShmemHeapInit')
import :: C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_SIZE_T), intent(IN), value :: nbytes
type(C_PTR) :: h
end function ShmemHeapInit
function ShmemHeapCheck(heap, free_blocks, free_space, used_blocks, used_space) result(status) bind(C,name='ShmemHeapCheck')
import :: C_INT, C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_INT), intent(OUT)    :: free_blocks, used_blocks
integer(C_SIZE_T), intent(OUT) :: free_space, used_space
integer(C_INT) :: status
end function ShmemHeapCheck
function ShmemHeapAllocBlock(heap, nbytes, safe) result(b) bind(C,name='ShmemHeapAllocBlock')
import :: C_INT, C_PTR, C_SIZE_T
implicit none
type(C_PTR), intent(IN), value :: heap
integer(C_SIZE_T), intent(IN), value :: nbytes
integer(C_INT), intent(IN), value :: safe
type(C_PTR) :: b
end function ShmemHeapAllocBlock
function ShmemHeapSetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapSetBlockMeta')
import :: C_PTR, C_INT, block_meta_c
implicit none
type(C_PTR), intent(IN), value    :: block
type(block_meta_c), intent(IN)    :: metadata
integer(C_INT), intent(IN), value :: msz
integer(C_INT) :: status
end function ShmemHeapSetBlockMeta
function ShmemHeapGetBlockMeta(block, metadata, msz) result(status) bind(C,name='ShmemHeapGetBlockMeta')
import :: C_PTR, C_INT, block_meta_c
implicit none
type(C_PTR), intent(IN), value    :: block
type(block_meta_c), intent(OUT)    :: metadata
integer(C_INT), intent(IN), value :: msz
integer(C_INT) :: status
end function ShmemHeapGetBlockMeta
function ShmemHeapFreeBlock(block) result(status) bind(C,name='ShmemHeapFreeBlock')
import :: C_INT, C_PTR
implicit none
type(C_PTR), intent(IN), value :: block
integer(C_INT) :: status
end function ShmemHeapFreeBlock
end interface
