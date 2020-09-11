(include <netdb.h>)

;;; POSIX EAI

(constant signed EAI_AGAIN)
(constant signed EAI_BADFLAGS)
(constant signed EAI_FAIL)
(constant signed EAI_FAMILY)
(constant signed EAI_MEMORY)
(constant signed EAI_NONAME)
(constant signed EAI_OVERFLOW)
(constant signed EAI_SERVICE)
(constant signed EAI_SOCKTYPE)
(constant signed EAI_SYSTEM)

(call-constant string gai_strerror EAI_AGAIN)
(call-constant string gai_strerror EAI_BADFLAGS)
(call-constant string gai_strerror EAI_FAIL)
(call-constant string gai_strerror EAI_FAMILY)
(call-constant string gai_strerror EAI_MEMORY)
(call-constant string gai_strerror EAI_NONAME)
(call-constant string gai_strerror EAI_OVERFLOW)
(call-constant string gai_strerror EAI_SERVICE)
(call-constant string gai_strerror EAI_SOCKTYPE)
(call-constant string gai_strerror EAI_SYSTEM)

;;; BSD EAI additions

(constant-ifdef signed EAI_MAX)
(constant-ifdef signed EAI_BADHINTS)
(constant-ifdef signed EAI_PROTOCOL)

(call-constant-ifdef string gai_strerror EAI_MAX)
(call-constant-ifdef string gai_strerror EAI_BADHINTS)
(call-constant-ifdef string gai_strerror EAI_PROTOCOL)

;;; BSD other additions

(constant-ifdef signed HOST_NOT_FOUND)
(constant-ifdef signed NETDB_INTERNAL)
(constant-ifdef signed NETDB_SUCCESS)
(constant-ifdef signed NO_ADDRESS)
(constant-ifdef signed NO_DATA)
(constant-ifdef signed NO_RECOVERY)
(constant-ifdef signed TRY_AGAIN)
