#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t idris_pipe_64() {
    int fds[2];
    if(pipe(fds) != 0) {
        exit(1); // not very good error handling...
    }
    uint32_t fd0 = (uint32_t)fds[0];
    uint32_t fd1 = (uint32_t)fds[1];
    printf("fd0 = %d, fd1 = %d\n", fd0, fd1);
    uint64_t fd01 = ((uint64_t)fd0 << 32) | (uint64_t)fd1;
    return fd01;
}
