/* SPDX-FileCopyrightText: Atlas Engineer LLC */
/* SPDX-License-Identifier: BSD-3-Clause */

#include <fcntl.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <util.h>

extern char **environ;

int nyxt_set_nonblock(int fd)
{
    int flags;

    flags = fcntl(fd, F_GETFL);
    if (flags < 0)
        return -1;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

int nyxt_set_winsize(int fd, unsigned short rows, unsigned short cols)
{
    struct winsize size;

    size.ws_row = rows;
    size.ws_col = cols;
    size.ws_xpixel = 0;
    size.ws_ypixel = 0;
    if (ioctl(fd, TIOCSWINSZ, &size) == -1)
        return -1;
    return 0;
}

int main(int argc, char **argv)
{
    if (argc < 2)
        return 127;
    login_tty(0);
    execve(argv[1], argv + 1, environ);
    return 127;
}
