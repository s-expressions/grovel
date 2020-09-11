(include <sys/stat.h>)

(type-size struct-stat)
(type-slot struct-stat st_dev)
(type-slot struct-stat st_ino)
(type-slot struct-stat st_mode)
(type-slot struct-stat st_nlink)
(type-slot struct-stat st_uid)
(type-slot struct-stat st_gid)
(type-slot struct-stat st_rdev)
(type-slot struct-stat st_size)

;;(type-slot struct-stat st_atim)
;;(type-slot struct-stat st_mtim)
;;(type-slot struct-stat st_ctim)

(type-slot struct-stat st_blksize)
(type-slot struct-stat st_blocks)


(constant unsigned S_IFMT)
(constant unsigned S_IFBLK)
(constant unsigned S_IFCHR)
(constant unsigned S_IFIFO)
(constant unsigned S_IFREG)
(constant unsigned S_IFDIR)
(constant unsigned S_IFLNK)
(constant unsigned S_IFSOCK)
