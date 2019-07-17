#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

uint64_t idris_pipe_64() {
    int fds[2];
    if(pipe(fds) != 0) {
        exit(1); // not very good error handling...
    }
    uint32_t fd0 = (uint32_t)fds[0];
    uint32_t fd1 = (uint32_t)fds[1];
    uint64_t fd01 = ((uint64_t)fd0 << 32) | (uint64_t)fd1;
    return fd01;
}

int idris_dup2(int fd0, int fd1) {
    return dup2(fd0, fd1);
}

int idris_close(int fd) {
    return close(fd);
}

void idris_exec_idris_ide_mode(char *idrisName) {
    char *args[3];
    args[0] = idrisName;
    args[1] = "--ide-mode";
    args[2] = NULL;
    execvp(args[0], args);
}

int idris_fork_proc() {
    return (int)fork();
}

void idris_setlinebuf(void *f) {
    setvbuf(f, (char *)NULL, _IOLBF, 0);
}


void *fileDescOpen(int fd, char *mode) {
    FILE *f = fdopen(fd, mode);
    return (void *)f;
}

void my_bzero(char *buf, int bs) {
    for(int i = 0; i < bs; i++) {
        buf[i] = '\0';
    }
}


void idris_test_reading(int fdr, int fdw) {
    char recBuf[1024];

    my_bzero(recBuf, 1024);


    printf("fdr = %d\n", fdr);
    printf("fdw = %d\n", fdw);

    read(fdr, recBuf, 1024);

    printf("Received from pipe: %s\n", recBuf);

    const char *buf = "000002x\n";
    write(fdw, buf, sizeof(buf));

    printf("Wrote to pipe!\n");

    my_bzero(recBuf, 1024);
    read(fdr, recBuf, 1024);

    printf("Received from pipe: %s\n", recBuf);
}
