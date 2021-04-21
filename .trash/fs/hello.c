/*
  FUSE: Filesystem in Userspace
  Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>
  Copyright (C) 2011       Sebastian Pipping <sebastian@pipping.org>

  This program can be distributed under the terms of the GNU GPL.
  See the file COPYING.
*/

/** @file
 *
 * This file system mirrors the existing file system hierarchy of the
 * system, starting at the root file system. This is implemented by
 * just "passing through" all requests to the corresponding user-space
 * libc functions. Its performance is terrible.
 *
 * Compile with
 *
 *     gcc -Wall passthrough.c `pkg-config fuse3 --cflags --libs` -o passthrough
 *
 * ## Source code ##
 * \include passthrough.c
 */


#define FUSE_USE_VERSION 31

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include <fuse.h>
#include <fuse_lowlevel.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#ifdef __FreeBSD__
#include <sys/socket.h>
#include <sys/un.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif
#include <stdlib.h>

int udirfd = -1;

static void *xmp_init(struct fuse_conn_info *conn)
{
     (void) conn;

     return NULL;
}

static int xmp_getattr(const char *path, struct stat *stbuf)
{
     int res;

     res = fstatat(udirfd, path + 1, stbuf, 0);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_access(const char *path, int mask)
{
     int res;

     res = faccessat(udirfd, path + 1, mask, 0);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_readlink(const char *path, char *buf, size_t size)
{
     int res;

     res = readlinkat(udirfd, path + 1, buf, size - 1);
     if (res == -1)
          return -errno;

     buf[res] = '\0';
     return 0;
}

static int xmp_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
		       off_t offset, struct fuse_file_info *fi)
{
     DIR *dp;
     struct dirent *de;
     int fd;

     (void) offset;
     (void) fi;

     printf("%s", path);

     fd = openat(udirfd, path + 1, O_DIRECTORY);
     if (fd < 0) {
          return -errno;
     }

     dp = fdopendir(fd);
     if (dp == NULL) {
          close(fd);
          return -errno;
     }

     while ((de = readdir(dp)) != NULL) {
          struct stat st;
          memset(&st, 0, sizeof(st));
          st.st_ino = de->d_ino;
          st.st_mode = de->d_type << 12;
          if (filler(buf, de->d_name, &st, 0))
               break;
     }

     closedir(dp);
     return 0;
}

static int xmp_mkdir(const char *path, mode_t mode)
{
     int res;

     res = mkdirat(udirfd, path + 1, mode);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_unlink(const char *path)
{
     int res;

     res = unlinkat(udirfd, path + 1, 0);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_rmdir(const char *path)
{
     int res;

     res = unlinkat(udirfd, path + 1, AT_REMOVEDIR);
     if (res == -1)
          return -errno;

     return 0;
}

// TODO
static int xmp_symlink(const char *from, const char *to)
{
     return -ENOSYS;
     int res;

     res = symlink(from, to);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_rename(const char *from, const char *to)
{
     int res;

     res = renameat(udirfd, from + 1, udirfd, to + 1);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_link(const char *from, const char *to)
{
     int res;

     res = linkat(udirfd, from + 1, udirfd, to + 1, 0);
     if (res == -1)
          return -errno;

     return 0;
}

// TODO
static int xmp_chmod(const char *path, mode_t mode)
{
     int res;

     res = chmod(path, mode);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_chown(const char *path, uid_t uid, gid_t gid)
{
     int res;

     res = fchownat(udirfd, path + 1, uid, gid, 0);
     if (res == -1)
          return -errno;

     return 0;
}

static int xmp_truncate(const char *path, off_t size)
{
     int res;
     int fd;

     fd = openat(udirfd, path + 1, O_WRONLY);
     if (fd < 0)
          return -errno;

     res = ftruncate(fd, size);
     if (res == -1) {
          close(fd);
          return -errno;
     }

     close(fd);
     return 0;
}

#ifdef HAVE_UTIMENSAT
static int xmp_utimens(const char *path, const struct timespec ts[2],
		       struct fuse_file_info *fi)
{
     (void) fi;
     int res;

     /* don't use utime/utimes since they follow symlinks */
     res = utimensat(udirfd, path + 1, ts, AT_SYMLINK_NOFOLLOW);
     if (res == -1)
          return -errno;

     return 0;
}
#endif

static int xmp_create(const char *path, mode_t mode,
		      struct fuse_file_info *fi)
{
     int res;

     res = openat(udirfd, path + 1, fi->flags, mode);
     if (res == -1)
          return -errno;

     fi->fh = res;
     return 0;
}

static int xmp_open(const char *path, struct fuse_file_info *fi)
{
     int res;

     res = openat(udirfd, path + 1, fi->flags);
     if (res == -1)
          return -errno;

     fi->fh = res;
     return 0;
}

static int xmp_read(const char *path, char *buf, size_t size, off_t offset,
		    struct fuse_file_info *fi)
{
     int fd;
     int res;

     if(fi == NULL)
          fd = openat(udirfd, path + 1, O_RDONLY);
     else
          fd = fi->fh;
	
     if (fd == -1)
          return -errno;

     res = pread(fd, buf, size, offset);
     if (res == -1)
          res = -errno;

     if(fi == NULL)
          close(fd);
     return res;
}

static int xmp_write(const char *path, const char *buf, size_t size,
		     off_t offset, struct fuse_file_info *fi)
{
     int fd;
     int res;

     (void) fi;
     if(fi == NULL)
          fd = openat(udirfd, path + 1, O_WRONLY);
     else
          fd = fi->fh;
	
     if (fd == -1)
          return -errno;

     res = pwrite(fd, buf, size, offset);
     if (res == -1)
          res = -errno;

     if(fi == NULL)
          close(fd);
     return res;
}

static int xmp_statfs(const char *path, struct statvfs *stbuf)
{
     int res;
     int fd;

     fd = openat(udirfd, path + 1, O_RDONLY);
     if (fd < 0)
          return -errno;

     res = fstatvfs(fd, stbuf);
     if (res == -1) {
          close(fd);
          return -errno;
     }

     close(fd);
     return 0;
}

static int xmp_release(const char *path, struct fuse_file_info *fi)
{
     (void) path;
     close(fi->fh);
     return 0;
}

static int xmp_fsync(const char *path, int isdatasync,
		     struct fuse_file_info *fi)
{
     /* Just a stub.	 This method is optional and can safely be left
        unimplemented */

     (void) path;
     (void) isdatasync;
     (void) fi;
     return 0;
}

#ifdef HAVE_POSIX_FALLOCATE
static int xmp_fallocate(const char *path, int mode,
                         off_t offset, off_t length, struct fuse_file_info *fi)
{
     int fd;
     int res;

     (void) fi;

     if (mode)
          return -EOPNOTSUPP;

     if(fi == NULL)
          fd = openat(udirfd, path + 1, O_WRONLY);
     else
          fd = fi->fh;
	
     if (fd == -1)
          return -errno;

     res = -posix_fallocate(fd, offset, length);

     if(fi == NULL)
          close(fd);
     return res;
}
#endif

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
static int xmp_setxattr(const char *path, const char *name, const char *value,
			size_t size, int flags)
{
     int fd;
     int res;

     fd = openat(udirfd, path + 1, O_WRONLY);
     if (fd < 0)
          return -errno;
     
     res = fsetxattrat(fd, name, value, size, flags);
     if (res == -1) {
          close(fd);
          return -errno;
     }

     close(fd);
     return 0;
}

static int xmp_getxattr(const char *path, const char *name, char *value,
			size_t size)
{
     int fd;
     int res;

     fd = openat(udirfd, path + 1, O_WRONLY);
     if (fd < 0)
          return -errno;
     
     res = fgetxattrat(fd, name, value, size, flags);
     if (res == -1) {
          close(fd);
          return -errno;
     }
     close(fd);
     return res;
}

static int xmp_listxattr(const char *path, char *list, size_t size)
{
     int fd;
     int res;

     fd = openat(udirfd, path + 1, O_WRONLY);
     if (fd < 0)
          return -errno;

     // FIXME
     res = flistxattrat(fd, list, size, 0);
     if (res == -1) {
          close(fd);
          return -errno;
     }
     close(fd);
     return res;
}

static int xmp_removexattr(const char *path, const char *name)
{
     int fd;
     int res;

     fd = openat(udirfd, path + 1, O_WRONLY);
     if (fd < 0)
          return -errno;

     // FIXME
     res = fremovexattr(fd, name, 0);
     if (res == -1) {
          close(fd);
          return -errno;
     }
     close(fd);
     return res;
}

#endif /* HAVE_SETXATTR */

#ifdef HAVE_COPY_FILE_RANGE
static ssize_t xmp_copy_file_range(const char *path_in,
				   struct fuse_file_info *fi_in,
				   off_t offset_in, const char *path_out,
				   struct fuse_file_info *fi_out,
				   off_t offset_out, size_t len, int flags)
{
     int fd_in, fd_out;
     ssize_t res;

     if(fi_in == NULL)
          fd_in = openat(udirfd, path_in + 1, O_RDONLY);
     else
          fd_in = fi_in->fh;

     if (fd_in == -1)
          return -errno;

     if(fi_out == NULL)
          fd_out = openat(udirfd, path_out + 1, O_WRONLY);
     else
          fd_out = fi_out->fh;

     if (fd_out == -1) {
          close(fd_in);
          return -errno;
     }

     res = copy_file_range(fd_in, &offset_in, fd_out, &offset_out, len,
                           flags);
     if (res == -1)
          res = -errno;

     close(fd_in);
     close(fd_out);

     return res;
}
#endif

static struct fuse_operations xmp_oper = {
     .init           = xmp_init,
     .getattr	= xmp_getattr,
     .access		= xmp_access,
     .readlink	= xmp_readlink,
     .readdir	= xmp_readdir,
     .mkdir		= xmp_mkdir,
     .symlink	= xmp_symlink,
     .unlink		= xmp_unlink,
     .rmdir		= xmp_rmdir,
     .rename		= xmp_rename,
     .link		= xmp_link,
     .chmod		= xmp_chmod,
     .chown		= xmp_chown,
     .truncate	= xmp_truncate,
#ifdef HAVE_UTIMENSAT
     .utimens	= xmp_utimens,
#endif
     .open		= xmp_open,
     .create 	= xmp_create,
     .read		= xmp_read,
     .write		= xmp_write,
     .statfs		= xmp_statfs,
     .release	= xmp_release,
     .fsync		= xmp_fsync,
#ifdef HAVE_POSIX_FALLOCATE
     .fallocate	= xmp_fallocate,
#endif
#ifdef HAVE_SETXATTR
     .setxattr	= xmp_setxattr,
     .getxattr	= xmp_getxattr,
     .listxattr	= xmp_listxattr,
     .removexattr	= xmp_removexattr,
#endif
#ifdef HAVE_COPY_FILE_RANGE
     .copy_file_range = xmp_copy_file_range,
#endif
};

int main(int argc, char *argv[])
{
     if (argc != 3) {
          fprintf(stderr, "usage: hello [mountpoint] [underlying]\n");
          exit(1);
     }

     umask(0);

     udirfd = open(argv[2], O_DIRECTORY);
     if (udirfd < 0) {
          perror("couldn't open directory");
          exit(2);
     }

     return fuse_main(argc-1, argv, &xmp_oper, NULL);
}
