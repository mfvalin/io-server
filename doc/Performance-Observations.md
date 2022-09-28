A few observations based on our benchmarks.

### One-sided MPI transfer

We tested various parameters that can be tuned using the IO-server structure of communication (1 relay per node, all sending to the server node):
- Number of relays
- Size of individual messages
- Number of channels on the server
- Shared vs exclusive locks on the MPI window
- Whether to use the MPI_MODE_NOCHECK assert when locking the window


The results:

[MPI bandwidth results](mpi_bandwidth_results.txt)

- A single relay, single channel can reach close to the maximum bandwidth for messages over 1MB in size
- Messages of size 2kB or less can reach about half the maximum bandwidth, no matter how many relays and channels
- When less than ~50 relays are transmitting data, the MPI_MODE_NOCHECK makes transmitting small(ish) messages much faster. Shared locks on MPI windows also clearly improve transmission speed for these messages, although to a lesser extent
- On large loads (200+ relays), the only circumstances where we can reach close to the maximum bandwidth is with large messages, exclusive locks and _no assert_.

### Disk write bandwidth

[Disk bandwidth results](disk_bandwidth_results.txt)

- For small write buffers (4kB), more concurrent processes always makes it faster
- For bigger buffers, we seem to reach the maximum write speed at around 4 processes. Adding more concurrent processes only brings marginal improvement (if any).

