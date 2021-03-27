! Copyright (C) 2021  Environnement et Changement climatique Canada
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
!     M. Valin,   Recherche en Prevision Numerique, 2020/2021
!     V. Magnoux, Recherche en Prevision Numerique, 2020/2021

! This file has been generated from distributed_circular_buffer.c
include 'io-server/circular_buffer.inc'
interface
subroutine DCB_sync_window(buffer) BIND(C, name = 'DCB_sync_window')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
end subroutine DCB_sync_window
function DCB_create(f_communicator, f_server_communicator, num_producers, num_channels, num_elements) result(p) BIND(C, name = 'DCB_create_f')
import :: C_PTR, C_INT
implicit none
integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the distributed buffer is shared
integer(C_INT), intent(IN), value :: f_server_communicator !< Communicator that groups the server processes
integer(C_INT), intent(IN), value :: num_producers  !< Number of producers (circular buffer instances)
integer(C_INT), intent(IN), value :: num_channels   !< Number of channels (PEs used for communication only)
integer(C_INT), intent(IN), value :: num_elements   !< Number of desired #data_element in the circular buffer
type(C_PTR) :: p                                    !< Pointer to created distributed circular buffer
end function DCB_create
subroutine DCB_print(buffer) BIND(C, name = 'DCB_print')
import :: C_PTR
implicit none
type(C_PTR), INTENT(IN), value :: buffer   !< Buffer for which to print data
end subroutine DCB_print
subroutine DCB_delete(buffer) BIND(C, name = 'DCB_delete')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
end subroutine DCB_delete
function DCB_get_num_elements(buffer, buffer_id) result(num_elements) BIND(C, name = 'DCB_get_num_elements')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value    :: buffer
integer(C_INT), intent(in), value :: buffer_id
integer(C_INT) :: num_elements
end function DCB_get_num_elements
function DCB_get_num_spaces(buffer, update_from_remote) result(num_spaces) BIND(C, name = 'DCB_get_num_spaces')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT), intent(in), value :: update_from_remote
integer(C_INT) :: num_spaces
end function DCB_get_num_spaces
function DCB_get_producer_id(buffer) result(producer_id) BIND(C, name = 'DCB_get_producer_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: producer_id
end function DCB_get_producer_id
function DCB_get_channel_id(buffer) result(channel_id) BIND(C, name = 'DCB_get_channel_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: channel_id
end function DCB_get_channel_id
function DCB_get_consumer_id(buffer) result(consumer_id) BIND(C, name = 'DCB_get_consumer_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: consumer_id
end function DCB_get_consumer_id
function DCB_get_num_producers(buffer) result(num_producers) BIND(C, name = 'DCB_get_num_producers')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_producers
end function DCB_get_num_producers
function DCB_get_num_consumers(buffer) result(num_consumers) BIND(C, name = 'DCB_get_num_consumers')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_consumers
end function DCB_get_num_consumers
function DCB_max_num_element_per_instance(buffer) result(num_elements) BIND(C, name = 'DCB_max_num_element_per_instance')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_elements
end function DCB_max_num_element_per_instance
function DCB_channel_start_listening(buffer) result(return_value) BIND(C, name = 'DCB_channel_start_listening')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: return_value
end function DCB_channel_start_listening
subroutine DCB_full_barrier(buffer) BIND(C, name = 'DCB_full_barrier')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
end subroutine DCB_full_barrier
subroutine DCB_server_barrier(buffer) BIND(C, name = 'DCB_server_barrier')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
end subroutine DCB_server_barrier
function DCB_put(buffer, src_data, num_elements, operation) result(num_available) BIND(C, name = 'DCB_put')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(in), value           :: buffer          !< Buffer where we want to insert data
integer(DATA_ELEMENT), dimension(*), intent(in) :: src_data !< Data to insert
integer(C_INT), intent(in), value        :: num_elements    !< How many data elements we want to insert
integer(C_INT), intent(in), value        :: operation       !< Whether to commit the transaction or wait
integer(C_INT) :: num_available !< How many element can still fit in the buffer after this call
end function DCB_put
function DCB_get(buffer, buffer_id, dest_data, num_elements, operation) result(num_available) BIND(C, name = 'DCB_get')
import :: C_PTR, C_INT, DATA_ELEMENT
implicit none
type(C_PTR), intent(in), value       :: buffer        !< DCB from which we want to read
integer(C_INT), intent(in), value    :: buffer_id     !< Which buffer in the DCB we want to read from
integer(DATA_ELEMENT), dimension(*), intent(inout) :: dest_data !< Where to put the data from the buffer
integer(C_INT), intent(in), value    :: num_elements  !< How many data elements to read
integer(C_INT), intent(in), value    :: operation     !< Whether to actually extract, read or just peek at the data
integer(C_INT) :: num_available
end function DCB_get
function DCB_check_integrity(buffer, verbose) result(result) BIND(C, name = 'DCB_check_integrity')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value    :: buffer
integer(C_INT), intent(in), value :: verbose
integer(C_INT)                    :: result
end function DCB_check_integrity
end interface
