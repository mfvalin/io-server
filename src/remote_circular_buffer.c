/*
 * Copyright (C) 2020  Environnement Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>

#include <mpi.h>

#include "circular_buffer.h"

typedef struct {
  MPI_Comm communicator;
  MPI_Win window;
  int root;
  int rank;
  circular_buffer_p local_buffer;
} remote_circular_buffer;

typedef remote_circular_buffer *remote_circular_buffer_p;


// TODO remove this, will be automatically generated
remote_circular_buffer_p remote_circular_buffer_print(remote_circular_buffer_p buffer);

//F_StArT
//    function remote_circular_buffer_create(f_communicator, root, rank, num_words) result(p) BIND(C, name = 'remote_circular_buffer_create')
//      import :: C_PTR, C_INT
//      implicit none
//      integer(C_INT), intent(IN), value :: f_communicator !< Communicator on which the remote buffer is shared
//      integer(C_INT), intent(IN), value :: root           !< Process rank on which buffer data is located
//      integer(C_INT), intent(IN), value :: rank           !< Rank of the current process
//      integer(C_INT), intent(IN), value :: num_words      !< Number of 32-bit elements in the circular buffer
//      type(C_PTR) :: p                                    !< Pointer to created remote circular buffer
//   end function remote_circular_buffer_create
//F_EnD
remote_circular_buffer_p remote_circular_buffer_create(
    int32_t f_communicator,     //!< [in]  Communicator on which the remote buffer is shared (in Fortran)
    int32_t root,               //!< [in]  Process rank on which buffer data is located
    int32_t rank,               //!< [in]  Rank of the current process
    int32_t num_words           //!< [in]  Number of 32-bit elements in the buffer
  ) {
  remote_circular_buffer_p buffer = (remote_circular_buffer*) malloc(sizeof(remote_circular_buffer));

  MPI_Aint win_size = 0;
  buffer->communicator = MPI_Comm_f2c(f_communicator);
  buffer->root = root;
  buffer->rank = rank;
  buffer->local_buffer = NULL;

  if (rank == root)
    win_size = num_words * sizeof(int32_t);

  MPI_Win_allocate(win_size, sizeof(int32_t), MPI_INFO_NULL, buffer->communicator, &buffer->local_buffer,
                   &buffer->window);


  if (rank == root)
    circular_buffer_init(buffer->local_buffer, num_words);

  return buffer;
}

//F_StArT
//  function remote_circular_buffer_print(buffer) result(p) BIND(C, name = 'remote_circular_buffer_print')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), INTENT(IN), value :: buffer
//    type(C_PTR) :: p
//  end function remote_circular_buffer_print
//F_EnD
remote_circular_buffer_p remote_circular_buffer_print(remote_circular_buffer_p buffer)
{
  printf("Comm: %ld, window: %ld, root: %d, rank: %d, local_buffer %ld, local_buffer[0] %d\n",
         (long)buffer->communicator, (long)buffer->window, buffer->root, buffer->rank, (long)buffer->local_buffer,
         buffer->local_buffer->data[0]);
  return buffer;
}

//F_StArT
//  subroutine remote_circular_buffer_delete(buffer) BIND(C, name = 'remote_circular_buffer_delete')
//    import :: C_PTR
//    implicit none
//    type(C_PTR), intent(IN), value :: buffer !< Buffer to delete
//  end subroutine remote_circular_buffer_delete
//F_EnD
void remote_circular_buffer_delete(
    remote_circular_buffer_p buffer //!< [in,out] Buffer to delete
  )
{
  MPI_Win_free(&buffer->window);
  free(buffer);
}
