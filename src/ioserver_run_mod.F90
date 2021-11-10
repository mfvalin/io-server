module ioserver_run_module
  use run_server_node_module, only: ioserver_run_server_node => run_server_node
  use run_model_node_module, only: ioserver_run_model_node => run_model_node
  implicit none
  private

  public :: ioserver_run_server_node, ioserver_run_model_node

contains

end module ioserver_run_module
