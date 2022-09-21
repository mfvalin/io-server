# IO server

Library for gathering and processing data asynchronously on a specific node of a cluster. Its main use is to perform IO, which could otherwise occupy a large number of processors on large-scale compute clusters.

- [IO server overview](doc/Overview.md)
- [Usage](doc/Usage.md)
  - [Streams](doc/Streams.md)
  - [Shared memory heap](doc/ShmemHeap.md)
- [linkkkk](doc/test_page.md)

## Building at CMC

### Build dependencies

- CMake 3.16+
- A library that provides the `process_command` and `process_data` functions

Note: `cmake_rpn` and `ci-env` are included as a submodule.  Please clone with the
`--recursive` flag or run `git submodule update --init --recursive` in the
git repo after having cloned.

### Environment

Source the right file depending on the architecture you need from the env directory.
This will load the specified compiler and define the ECCI_DATA_DIR variable for the test datasets

- Example for PPP3 and skylake specific architecture:
```
. ci-env/latest/ubuntu-18.04-skylake-64/intel-19.0.3.199.sh
```

- Example for XC50 on intel-19.0.5
```
. ci-env/latest/sles-15-skylake-64/intel-19.0.5.281.sh
```

- Example for CMC network and gnu 7.5:
```
. ci-env/latest/ubuntu-18.04-amd-64/gnu-7.5.0.sh
```

### Build and install

```
mkdir build
cd build
cmake ../
make
make package
```

## Building outside CMC (External users)

### Build dependencies

- CMake 3.16+
- A library that provides the `process_command` and `process_data` functions

Note: `cmake_rpn` and `ci-env` are included as a submodule but `ci-env` is not available externally. 
Please run `git -c submodule."ci-env".update=none submodule update --init --recursive` in the git repo after having cloned.

### Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=${your_choice}
make 
make install
```
