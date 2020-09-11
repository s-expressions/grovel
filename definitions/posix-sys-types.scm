(include <sys/types.h>)

(include <pthread.h>)
;;(include <time.h>)
;;(include <trace.h>)

;;; Either signed or unsigned integer types

(type-signedness dev_t)
(type-signedness gid_t)
(type-signedness id_t)
(type-signedness mode_t)
(type-signedness nlink_t)
(type-signedness time_t)
(type-signedness uid_t)

(type-size dev_t)
(type-size gid_t)
(type-size id_t)
(type-size mode_t)
(type-size nlink_t)
(type-size time_t)
(type-size uid_t)

;;; Unsigned integer types

(type-size fsblkcnt_t)
(type-size fsfilcnt_t)
(type-size ino_t)
(type-size size_t)

;;; Signed integer types

(type-size blkcnt_t)
(type-size blksize_t)
(type-size off_t)
(type-size pid_t)
(type-size ssize_t)
(type-size suseconds_t)

;;; Unknown arithmetic types

(type-size clock_t)
(type-size key_t)

;;; Unknown types

;;(may-fail (type-size clockid_t))

;;(may-fail (type-size pthread_attr_t))

;;(type-size pthread_barrier_t)
;;(type-size pthread_barrierattr_t)
(type-size pthread_cond_t)
(type-size pthread_condattr_t)
(type-size pthread_key_t)
(type-size pthread_mutex_t)
(type-size pthread_mutexattr_t)
(type-size pthread_once_t)
(type-size pthread_rwlock_t)
(type-size pthread_rwlockattr_t)
;;(type-size pthread_spinlock_t)
(type-size pthread_t)

;;(type-size timer_t)
;;(type-size trace_attr_t)
;;(type-size trace_event_id_t)
;;(type-size trace_event_set_t)
;;(type-size trace_id_t)
