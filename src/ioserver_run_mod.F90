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

module ioserver_run_module
  use ioserver_context_module,  only: ioserver_input_parameters
  use run_server_node_module,   only: ioserver_run_server_node    => run_server_node,             &
                                      server_function_template
  use run_model_node_module,    only: ioserver_run_model_node     => run_model_node,              &
                                      model_function_template,                                    &
                                      relay_function_template,                                    &
                                      ioserver_server_bound_relay => default_server_bound_relay,  &
                                      ioserver_model_bound_relay  => default_model_bound_relay
  implicit none
contains
end module ioserver_run_module
