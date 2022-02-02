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

! This file has been generated from distributed_circular_buffer.c

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
subroutine DCB_sync_window(buffer) BIND(C, name = 'DCB_sync_window')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
end subroutine DCB_sync_window
function DCB_create(f_communicator, f_server_communicator, communication_type, num_bytes_server_bound, num_bytes_client_bound, verbose) result(p) BIND(C, name = 'DCB_create_f')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
integer(C_INT),    intent(IN), value :: f_communicator         !< Communicator on which the distributed buffer is shared
integer(C_INT),    intent(IN), value :: f_server_communicator  !< Communicator that groups the server processes
integer(C_INT),    intent(IN), value :: communication_type     !< Communication type of the calling process (server-bound, client-bound or channel, DCB_*_TYPE)
integer(C_SIZE_T), intent(IN), value :: num_bytes_server_bound !< Number of bytes in a single server-bound circular buffer (only needed on the root process)
integer(C_SIZE_T), intent(IN), value :: num_bytes_client_bound !< Number of bytes in a single client-bound circular buffer (only needed on the root process)
integer(C_INT),    intent(IN), value :: verbose                !< Print some info when == 1
type(C_PTR) :: p                                       !< Pointer to created distributed circular buffer
end function DCB_create
subroutine DCB_print(buffer, dump_data) BIND(C, name = 'DCB_print')
import :: C_PTR, C_INT
implicit none
type(C_PTR),    INTENT(IN), value :: buffer    !< Buffer for which to print data
integer(C_INT), INTENT(IN), value :: dump_data !< Whether to print buffer content
end subroutine DCB_print
subroutine DCB_delete(buffer) BIND(C, name = 'DCB_delete')
import :: C_PTR
implicit none
type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
end subroutine DCB_delete
function DCB_get_available_data(buffer, buffer_id) result(num_bytes) BIND(C, name = 'DCB_get_available_data')
import :: C_PTR, C_INT, C_INT64_T
implicit none
type(C_PTR), intent(in), value    :: buffer
integer(C_INT), intent(in), value :: buffer_id
integer(C_INT64_T) :: num_bytes
end function DCB_get_available_data
function DCB_get_available_space(buffer, update_from_remote) result(num_spaces) BIND(C, name = 'DCB_get_available_space')
import :: C_PTR, C_INT, C_INT64_T
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT), intent(in), value :: update_from_remote
integer(C_INT64_T) :: num_spaces
end function DCB_get_available_space
function DCB_get_server_bound_client_id(buffer) result(server_bound_client_id) BIND(C, name = 'DCB_get_server_bound_client_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: server_bound_client_id
end function DCB_get_server_bound_client_id
function DCB_get_channel_id(buffer) result(channel_id) BIND(C, name = 'DCB_get_channel_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: channel_id
end function DCB_get_channel_id
function DCB_get_server_bound_server_id(buffer) result(server_bound_server_id) BIND(C, name = 'DCB_get_server_bound_server_id')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: server_bound_server_id
end function DCB_get_server_bound_server_id
function DCB_get_num_server_bound_instances(buffer) result(num_instances) BIND(C, name = 'DCB_get_num_server_bound_instances')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_instances
end function DCB_get_num_server_bound_instances
function DCB_get_num_server_consumers(buffer) result(num_consumers) BIND(C, name = 'DCB_get_num_server_consumers')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT) :: num_consumers
end function DCB_get_num_server_consumers
function DCB_get_capacity_local(buffer) result(capacity_bytes) BIND(C, name = 'DCB_get_capacity_local')
import :: C_PTR, C_INT64_T
implicit none
type(C_PTR), intent(in), value :: buffer
integer(C_INT64_T) :: capacity_bytes
end function DCB_get_capacity_local
function DCB_get_capacity_server(buffer, buffer_id) result(capacity_bytes) BIND(C, name = 'DCB_get_capacity_server')
import :: C_PTR, C_INT, C_INT64_T
implicit none
type(C_PTR),    intent(in), value :: buffer
integer(C_INT), intent(in), value :: buffer_id
integer(C_INT64_T) :: capacity_bytes
end function DCB_get_capacity_server
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
function DCB_put_client(buffer, src_data, num_bytes, operation) result(status) BIND(C, name = 'DCB_put_client')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
type(C_PTR),       intent(in), value :: buffer    !< Buffer where we want to insert data
type(C_PTR),       intent(in), value :: src_data  !< Data to insert
integer(C_SIZE_T), intent(in), value :: num_bytes !< How many data elements we want to insert
integer(C_INT),    intent(in), value :: operation !< Whether to commit the transaction or wait
integer(C_INT) :: status !< 0 if success, -1 if failure
end function DCB_put_client
function DCB_get_server(buffer, buffer_id, dest_data, num_bytes, operation) result(status) BIND(C, name = 'DCB_get_server')
import :: C_PTR, C_INT, C_SIZE_T
implicit none
type(C_PTR),       intent(in), value :: buffer    !< DCB from which we want to read
integer(C_INT),    intent(in), value :: buffer_id !< Which buffer in the DCB we want to read from
type(C_PTR),       intent(in), value :: dest_data !< Where to put the data from the buffer
integer(C_SIZE_T), intent(in), value :: num_bytes !< How many bytes to read
integer(C_INT),    intent(in), value :: operation !< Whether to actually extract, read or just peek at the data
integer(C_INT) :: status
end function DCB_get_server
function DCB_check_integrity(buffer, verbose) result(result) BIND(C, name = 'DCB_check_integrity')
import :: C_PTR, C_INT
implicit none
type(C_PTR), intent(in), value    :: buffer
integer(C_INT), intent(in), value :: verbose
integer(C_INT)                    :: result
end function DCB_check_integrity
end interface
