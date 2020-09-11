#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

static void check(int rv)
{
  if (rv < 0) {
    exit(1);
  }
}

static void grovel_uintmax(
  const char *prefix,
  const char *symbol,
  uintmax_t value
)
{
  check(printf("(%s %s %" PRIuMAX ")\n",
    prefix, symbol, value));
}

static void grovel_uintmax_2(
  const char *prefix,
  const char *symbol1,
  const char *symbol2,
  uintmax_t value
)
{
  check(printf("(%s %s %s %" PRIuMAX ")\n",
    prefix, symbol1, symbol2, value));
}

static void grovel_intmax(
  const char *prefix,
  const char *symbol,
  intmax_t value
)
{
  check(printf("(%s %s %" PRIdMAX ")\n",
    prefix, symbol, value));
}

static void grovel_symbol(
  const char *prefix,
  const char *symbol,
  const char *value
)
{
  check(printf("(%s %s %s)\n", prefix, symbol, value));
}

static void grovel_string(
  const char *prefix,
  const char *symbol,
  const char *value
)
{
  check(printf("(%s %s \"%s\")\n", prefix, symbol, value));
}

#include <errno.h>

static void grovel_1(void)
{
#ifdef E2BIG
  grovel_intmax("constant", "E2BIG", (intmax_t)(E2BIG));
#endif
}

static void grovel_2(void)
{
#ifdef EACCES
  grovel_intmax("constant", "EACCES", (intmax_t)(EACCES));
#endif
}

static void grovel_3(void)
{
#ifdef EADDRINUSE
  grovel_intmax("constant", "EADDRINUSE", (intmax_t)(EADDRINUSE));
#endif
}

static void grovel_4(void)
{
#ifdef EADDRNOTAVAIL
  grovel_intmax("constant", "EADDRNOTAVAIL", (intmax_t)(EADDRNOTAVAIL));
#endif
}

static void grovel_5(void)
{
#ifdef EADV
  grovel_intmax("constant", "EADV", (intmax_t)(EADV));
#endif
}

static void grovel_6(void)
{
#ifdef EAFNOSUPPORT
  grovel_intmax("constant", "EAFNOSUPPORT", (intmax_t)(EAFNOSUPPORT));
#endif
}

static void grovel_7(void)
{
#ifdef EAGAIN
  grovel_intmax("constant", "EAGAIN", (intmax_t)(EAGAIN));
#endif
}

static void grovel_8(void)
{
#ifdef EALREADY
  grovel_intmax("constant", "EALREADY", (intmax_t)(EALREADY));
#endif
}

static void grovel_9(void)
{
#ifdef EAUTH
  grovel_intmax("constant", "EAUTH", (intmax_t)(EAUTH));
#endif
}

static void grovel_10(void)
{
#ifdef EBADARCH
  grovel_intmax("constant", "EBADARCH", (intmax_t)(EBADARCH));
#endif
}

static void grovel_11(void)
{
#ifdef EBADE
  grovel_intmax("constant", "EBADE", (intmax_t)(EBADE));
#endif
}

static void grovel_12(void)
{
#ifdef EBADEXEC
  grovel_intmax("constant", "EBADEXEC", (intmax_t)(EBADEXEC));
#endif
}

static void grovel_13(void)
{
#ifdef EBADF
  grovel_intmax("constant", "EBADF", (intmax_t)(EBADF));
#endif
}

static void grovel_14(void)
{
#ifdef EBADFD
  grovel_intmax("constant", "EBADFD", (intmax_t)(EBADFD));
#endif
}

static void grovel_15(void)
{
#ifdef EBADMACHO
  grovel_intmax("constant", "EBADMACHO", (intmax_t)(EBADMACHO));
#endif
}

static void grovel_16(void)
{
#ifdef EBADMSG
  grovel_intmax("constant", "EBADMSG", (intmax_t)(EBADMSG));
#endif
}

static void grovel_17(void)
{
#ifdef EBADR
  grovel_intmax("constant", "EBADR", (intmax_t)(EBADR));
#endif
}

static void grovel_18(void)
{
#ifdef EBADRPC
  grovel_intmax("constant", "EBADRPC", (intmax_t)(EBADRPC));
#endif
}

static void grovel_19(void)
{
#ifdef EBADRQC
  grovel_intmax("constant", "EBADRQC", (intmax_t)(EBADRQC));
#endif
}

static void grovel_20(void)
{
#ifdef EBADSLT
  grovel_intmax("constant", "EBADSLT", (intmax_t)(EBADSLT));
#endif
}

static void grovel_21(void)
{
#ifdef EBFONT
  grovel_intmax("constant", "EBFONT", (intmax_t)(EBFONT));
#endif
}

static void grovel_22(void)
{
#ifdef EBUSY
  grovel_intmax("constant", "EBUSY", (intmax_t)(EBUSY));
#endif
}

static void grovel_23(void)
{
#ifdef ECANCELED
  grovel_intmax("constant", "ECANCELED", (intmax_t)(ECANCELED));
#endif
}

static void grovel_24(void)
{
#ifdef ECAPMODE
  grovel_intmax("constant", "ECAPMODE", (intmax_t)(ECAPMODE));
#endif
}

static void grovel_25(void)
{
#ifdef ECHILD
  grovel_intmax("constant", "ECHILD", (intmax_t)(ECHILD));
#endif
}

static void grovel_26(void)
{
#ifdef ECHRNG
  grovel_intmax("constant", "ECHRNG", (intmax_t)(ECHRNG));
#endif
}

static void grovel_27(void)
{
#ifdef ECOMM
  grovel_intmax("constant", "ECOMM", (intmax_t)(ECOMM));
#endif
}

static void grovel_28(void)
{
#ifdef ECONNABORTED
  grovel_intmax("constant", "ECONNABORTED", (intmax_t)(ECONNABORTED));
#endif
}

static void grovel_29(void)
{
#ifdef ECONNREFUSED
  grovel_intmax("constant", "ECONNREFUSED", (intmax_t)(ECONNREFUSED));
#endif
}

static void grovel_30(void)
{
#ifdef ECONNRESET
  grovel_intmax("constant", "ECONNRESET", (intmax_t)(ECONNRESET));
#endif
}

static void grovel_31(void)
{
#ifdef EDEADLK
  grovel_intmax("constant", "EDEADLK", (intmax_t)(EDEADLK));
#endif
}

static void grovel_32(void)
{
#ifdef EDEADLOCK
  grovel_intmax("constant", "EDEADLOCK", (intmax_t)(EDEADLOCK));
#endif
}

static void grovel_33(void)
{
#ifdef EDESTADDRREQ
  grovel_intmax("constant", "EDESTADDRREQ", (intmax_t)(EDESTADDRREQ));
#endif
}

static void grovel_34(void)
{
#ifdef EDEVERR
  grovel_intmax("constant", "EDEVERR", (intmax_t)(EDEVERR));
#endif
}

static void grovel_35(void)
{
#ifdef EDOM
  grovel_intmax("constant", "EDOM", (intmax_t)(EDOM));
#endif
}

static void grovel_36(void)
{
#ifdef EDOOFUS
  grovel_intmax("constant", "EDOOFUS", (intmax_t)(EDOOFUS));
#endif
}

static void grovel_37(void)
{
#ifdef EDOTDOT
  grovel_intmax("constant", "EDOTDOT", (intmax_t)(EDOTDOT));
#endif
}

static void grovel_38(void)
{
#ifdef EDQUOT
  grovel_intmax("constant", "EDQUOT", (intmax_t)(EDQUOT));
#endif
}

static void grovel_39(void)
{
#ifdef EEXIST
  grovel_intmax("constant", "EEXIST", (intmax_t)(EEXIST));
#endif
}

static void grovel_40(void)
{
#ifdef EFAULT
  grovel_intmax("constant", "EFAULT", (intmax_t)(EFAULT));
#endif
}

static void grovel_41(void)
{
#ifdef EFBIG
  grovel_intmax("constant", "EFBIG", (intmax_t)(EFBIG));
#endif
}

static void grovel_42(void)
{
#ifdef EFTYPE
  grovel_intmax("constant", "EFTYPE", (intmax_t)(EFTYPE));
#endif
}

static void grovel_43(void)
{
#ifdef EHOSTDOWN
  grovel_intmax("constant", "EHOSTDOWN", (intmax_t)(EHOSTDOWN));
#endif
}

static void grovel_44(void)
{
#ifdef EHOSTUNREACH
  grovel_intmax("constant", "EHOSTUNREACH", (intmax_t)(EHOSTUNREACH));
#endif
}

static void grovel_45(void)
{
#ifdef EHWPOISON
  grovel_intmax("constant", "EHWPOISON", (intmax_t)(EHWPOISON));
#endif
}

static void grovel_46(void)
{
#ifdef EIDRM
  grovel_intmax("constant", "EIDRM", (intmax_t)(EIDRM));
#endif
}

static void grovel_47(void)
{
#ifdef EILSEQ
  grovel_intmax("constant", "EILSEQ", (intmax_t)(EILSEQ));
#endif
}

static void grovel_48(void)
{
#ifdef EINPROGRESS
  grovel_intmax("constant", "EINPROGRESS", (intmax_t)(EINPROGRESS));
#endif
}

static void grovel_49(void)
{
#ifdef EINTEGRITY
  grovel_intmax("constant", "EINTEGRITY", (intmax_t)(EINTEGRITY));
#endif
}

static void grovel_50(void)
{
#ifdef EINTR
  grovel_intmax("constant", "EINTR", (intmax_t)(EINTR));
#endif
}

static void grovel_51(void)
{
#ifdef EINVAL
  grovel_intmax("constant", "EINVAL", (intmax_t)(EINVAL));
#endif
}

static void grovel_52(void)
{
#ifdef EIO
  grovel_intmax("constant", "EIO", (intmax_t)(EIO));
#endif
}

static void grovel_53(void)
{
#ifdef EIPSEC
  grovel_intmax("constant", "EIPSEC", (intmax_t)(EIPSEC));
#endif
}

static void grovel_54(void)
{
#ifdef EISCONN
  grovel_intmax("constant", "EISCONN", (intmax_t)(EISCONN));
#endif
}

static void grovel_55(void)
{
#ifdef EISDIR
  grovel_intmax("constant", "EISDIR", (intmax_t)(EISDIR));
#endif
}

static void grovel_56(void)
{
#ifdef EISNAM
  grovel_intmax("constant", "EISNAM", (intmax_t)(EISNAM));
#endif
}

static void grovel_57(void)
{
#ifdef EKEYEXPIRED
  grovel_intmax("constant", "EKEYEXPIRED", (intmax_t)(EKEYEXPIRED));
#endif
}

static void grovel_58(void)
{
#ifdef EKEYREJECTED
  grovel_intmax("constant", "EKEYREJECTED", (intmax_t)(EKEYREJECTED));
#endif
}

static void grovel_59(void)
{
#ifdef EKEYREVOKED
  grovel_intmax("constant", "EKEYREVOKED", (intmax_t)(EKEYREVOKED));
#endif
}

static void grovel_60(void)
{
#ifdef EL2HLT
  grovel_intmax("constant", "EL2HLT", (intmax_t)(EL2HLT));
#endif
}

static void grovel_61(void)
{
#ifdef EL2NSYNC
  grovel_intmax("constant", "EL2NSYNC", (intmax_t)(EL2NSYNC));
#endif
}

static void grovel_62(void)
{
#ifdef EL3HLT
  grovel_intmax("constant", "EL3HLT", (intmax_t)(EL3HLT));
#endif
}

static void grovel_63(void)
{
#ifdef EL3RST
  grovel_intmax("constant", "EL3RST", (intmax_t)(EL3RST));
#endif
}

static void grovel_64(void)
{
#ifdef ELIBACC
  grovel_intmax("constant", "ELIBACC", (intmax_t)(ELIBACC));
#endif
}

static void grovel_65(void)
{
#ifdef ELIBBAD
  grovel_intmax("constant", "ELIBBAD", (intmax_t)(ELIBBAD));
#endif
}

static void grovel_66(void)
{
#ifdef ELIBEXEC
  grovel_intmax("constant", "ELIBEXEC", (intmax_t)(ELIBEXEC));
#endif
}

static void grovel_67(void)
{
#ifdef ELIBMAX
  grovel_intmax("constant", "ELIBMAX", (intmax_t)(ELIBMAX));
#endif
}

static void grovel_68(void)
{
#ifdef ELIBSCN
  grovel_intmax("constant", "ELIBSCN", (intmax_t)(ELIBSCN));
#endif
}

static void grovel_69(void)
{
#ifdef ELNRNG
  grovel_intmax("constant", "ELNRNG", (intmax_t)(ELNRNG));
#endif
}

static void grovel_70(void)
{
#ifdef ELOCKUNMAPPED
  grovel_intmax("constant", "ELOCKUNMAPPED", (intmax_t)(ELOCKUNMAPPED));
#endif
}

static void grovel_71(void)
{
#ifdef ELOOP
  grovel_intmax("constant", "ELOOP", (intmax_t)(ELOOP));
#endif
}

static void grovel_72(void)
{
#ifdef EMEDIUMTYPE
  grovel_intmax("constant", "EMEDIUMTYPE", (intmax_t)(EMEDIUMTYPE));
#endif
}

static void grovel_73(void)
{
#ifdef EMFILE
  grovel_intmax("constant", "EMFILE", (intmax_t)(EMFILE));
#endif
}

static void grovel_74(void)
{
#ifdef EMLINK
  grovel_intmax("constant", "EMLINK", (intmax_t)(EMLINK));
#endif
}

static void grovel_75(void)
{
#ifdef EMSGSIZE
  grovel_intmax("constant", "EMSGSIZE", (intmax_t)(EMSGSIZE));
#endif
}

static void grovel_76(void)
{
#ifdef EMULTIHOP
  grovel_intmax("constant", "EMULTIHOP", (intmax_t)(EMULTIHOP));
#endif
}

static void grovel_77(void)
{
#ifdef ENAMETOOLONG
  grovel_intmax("constant", "ENAMETOOLONG", (intmax_t)(ENAMETOOLONG));
#endif
}

static void grovel_78(void)
{
#ifdef ENAVAIL
  grovel_intmax("constant", "ENAVAIL", (intmax_t)(ENAVAIL));
#endif
}

static void grovel_79(void)
{
#ifdef ENEEDAUTH
  grovel_intmax("constant", "ENEEDAUTH", (intmax_t)(ENEEDAUTH));
#endif
}

static void grovel_80(void)
{
#ifdef ENETDOWN
  grovel_intmax("constant", "ENETDOWN", (intmax_t)(ENETDOWN));
#endif
}

static void grovel_81(void)
{
#ifdef ENETRESET
  grovel_intmax("constant", "ENETRESET", (intmax_t)(ENETRESET));
#endif
}

static void grovel_82(void)
{
#ifdef ENETUNREACH
  grovel_intmax("constant", "ENETUNREACH", (intmax_t)(ENETUNREACH));
#endif
}

static void grovel_83(void)
{
#ifdef ENFILE
  grovel_intmax("constant", "ENFILE", (intmax_t)(ENFILE));
#endif
}

static void grovel_84(void)
{
#ifdef ENOANO
  grovel_intmax("constant", "ENOANO", (intmax_t)(ENOANO));
#endif
}

static void grovel_85(void)
{
#ifdef ENOATTR
  grovel_intmax("constant", "ENOATTR", (intmax_t)(ENOATTR));
#endif
}

static void grovel_86(void)
{
#ifdef ENOBUFS
  grovel_intmax("constant", "ENOBUFS", (intmax_t)(ENOBUFS));
#endif
}

static void grovel_87(void)
{
#ifdef ENOCSI
  grovel_intmax("constant", "ENOCSI", (intmax_t)(ENOCSI));
#endif
}

static void grovel_88(void)
{
#ifdef ENODATA
  grovel_intmax("constant", "ENODATA", (intmax_t)(ENODATA));
#endif
}

static void grovel_89(void)
{
#ifdef ENODEV
  grovel_intmax("constant", "ENODEV", (intmax_t)(ENODEV));
#endif
}

static void grovel_90(void)
{
#ifdef ENOENT
  grovel_intmax("constant", "ENOENT", (intmax_t)(ENOENT));
#endif
}

static void grovel_91(void)
{
#ifdef ENOEXEC
  grovel_intmax("constant", "ENOEXEC", (intmax_t)(ENOEXEC));
#endif
}

static void grovel_92(void)
{
#ifdef ENOKEY
  grovel_intmax("constant", "ENOKEY", (intmax_t)(ENOKEY));
#endif
}

static void grovel_93(void)
{
#ifdef ENOLCK
  grovel_intmax("constant", "ENOLCK", (intmax_t)(ENOLCK));
#endif
}

static void grovel_94(void)
{
#ifdef ENOLINK
  grovel_intmax("constant", "ENOLINK", (intmax_t)(ENOLINK));
#endif
}

static void grovel_95(void)
{
#ifdef ENOMEDIUM
  grovel_intmax("constant", "ENOMEDIUM", (intmax_t)(ENOMEDIUM));
#endif
}

static void grovel_96(void)
{
#ifdef ENOMEM
  grovel_intmax("constant", "ENOMEM", (intmax_t)(ENOMEM));
#endif
}

static void grovel_97(void)
{
#ifdef ENOMSG
  grovel_intmax("constant", "ENOMSG", (intmax_t)(ENOMSG));
#endif
}

static void grovel_98(void)
{
#ifdef ENONET
  grovel_intmax("constant", "ENONET", (intmax_t)(ENONET));
#endif
}

static void grovel_99(void)
{
#ifdef ENOPKG
  grovel_intmax("constant", "ENOPKG", (intmax_t)(ENOPKG));
#endif
}

static void grovel_100(void)
{
#ifdef ENOPOLICY
  grovel_intmax("constant", "ENOPOLICY", (intmax_t)(ENOPOLICY));
#endif
}

static void grovel_101(void)
{
#ifdef ENOPROTOOPT
  grovel_intmax("constant", "ENOPROTOOPT", (intmax_t)(ENOPROTOOPT));
#endif
}

static void grovel_102(void)
{
#ifdef ENOSPC
  grovel_intmax("constant", "ENOSPC", (intmax_t)(ENOSPC));
#endif
}

static void grovel_103(void)
{
#ifdef ENOSR
  grovel_intmax("constant", "ENOSR", (intmax_t)(ENOSR));
#endif
}

static void grovel_104(void)
{
#ifdef ENOSTR
  grovel_intmax("constant", "ENOSTR", (intmax_t)(ENOSTR));
#endif
}

static void grovel_105(void)
{
#ifdef ENOSYS
  grovel_intmax("constant", "ENOSYS", (intmax_t)(ENOSYS));
#endif
}

static void grovel_106(void)
{
#ifdef ENOTACTIVE
  grovel_intmax("constant", "ENOTACTIVE", (intmax_t)(ENOTACTIVE));
#endif
}

static void grovel_107(void)
{
#ifdef ENOTBLK
  grovel_intmax("constant", "ENOTBLK", (intmax_t)(ENOTBLK));
#endif
}

static void grovel_108(void)
{
#ifdef ENOTCAPABLE
  grovel_intmax("constant", "ENOTCAPABLE", (intmax_t)(ENOTCAPABLE));
#endif
}

static void grovel_109(void)
{
#ifdef ENOTCONN
  grovel_intmax("constant", "ENOTCONN", (intmax_t)(ENOTCONN));
#endif
}

static void grovel_110(void)
{
#ifdef ENOTDIR
  grovel_intmax("constant", "ENOTDIR", (intmax_t)(ENOTDIR));
#endif
}

static void grovel_111(void)
{
#ifdef ENOTEMPTY
  grovel_intmax("constant", "ENOTEMPTY", (intmax_t)(ENOTEMPTY));
#endif
}

static void grovel_112(void)
{
#ifdef ENOTNAM
  grovel_intmax("constant", "ENOTNAM", (intmax_t)(ENOTNAM));
#endif
}

static void grovel_113(void)
{
#ifdef ENOTRECOVERABLE
  grovel_intmax("constant", "ENOTRECOVERABLE", (intmax_t)(ENOTRECOVERABLE));
#endif
}

static void grovel_114(void)
{
#ifdef ENOTSOCK
  grovel_intmax("constant", "ENOTSOCK", (intmax_t)(ENOTSOCK));
#endif
}

static void grovel_115(void)
{
#ifdef ENOTSUP
  grovel_intmax("constant", "ENOTSUP", (intmax_t)(ENOTSUP));
#endif
}

static void grovel_116(void)
{
#ifdef ENOTTY
  grovel_intmax("constant", "ENOTTY", (intmax_t)(ENOTTY));
#endif
}

static void grovel_117(void)
{
#ifdef ENOTUNIQ
  grovel_intmax("constant", "ENOTUNIQ", (intmax_t)(ENOTUNIQ));
#endif
}

static void grovel_118(void)
{
#ifdef ENXIO
  grovel_intmax("constant", "ENXIO", (intmax_t)(ENXIO));
#endif
}

static void grovel_119(void)
{
#ifdef EOPNOTSUPP
  grovel_intmax("constant", "EOPNOTSUPP", (intmax_t)(EOPNOTSUPP));
#endif
}

static void grovel_120(void)
{
#ifdef EOVERFLOW
  grovel_intmax("constant", "EOVERFLOW", (intmax_t)(EOVERFLOW));
#endif
}

static void grovel_121(void)
{
#ifdef EOWNERDEAD
  grovel_intmax("constant", "EOWNERDEAD", (intmax_t)(EOWNERDEAD));
#endif
}

static void grovel_122(void)
{
#ifdef EPERM
  grovel_intmax("constant", "EPERM", (intmax_t)(EPERM));
#endif
}

static void grovel_123(void)
{
#ifdef EPFNOSUPPORT
  grovel_intmax("constant", "EPFNOSUPPORT", (intmax_t)(EPFNOSUPPORT));
#endif
}

static void grovel_124(void)
{
#ifdef EPIPE
  grovel_intmax("constant", "EPIPE", (intmax_t)(EPIPE));
#endif
}

static void grovel_125(void)
{
#ifdef EPROCLIM
  grovel_intmax("constant", "EPROCLIM", (intmax_t)(EPROCLIM));
#endif
}

static void grovel_126(void)
{
#ifdef EPROCUNAVAIL
  grovel_intmax("constant", "EPROCUNAVAIL", (intmax_t)(EPROCUNAVAIL));
#endif
}

static void grovel_127(void)
{
#ifdef EPROGMISMATCH
  grovel_intmax("constant", "EPROGMISMATCH", (intmax_t)(EPROGMISMATCH));
#endif
}

static void grovel_128(void)
{
#ifdef EPROGUNAVAIL
  grovel_intmax("constant", "EPROGUNAVAIL", (intmax_t)(EPROGUNAVAIL));
#endif
}

static void grovel_129(void)
{
#ifdef EPROTO
  grovel_intmax("constant", "EPROTO", (intmax_t)(EPROTO));
#endif
}

static void grovel_130(void)
{
#ifdef EPROTONOSUPPORT
  grovel_intmax("constant", "EPROTONOSUPPORT", (intmax_t)(EPROTONOSUPPORT));
#endif
}

static void grovel_131(void)
{
#ifdef EPROTOTYPE
  grovel_intmax("constant", "EPROTOTYPE", (intmax_t)(EPROTOTYPE));
#endif
}

static void grovel_132(void)
{
#ifdef EPWROFF
  grovel_intmax("constant", "EPWROFF", (intmax_t)(EPWROFF));
#endif
}

static void grovel_133(void)
{
#ifdef EQFULL
  grovel_intmax("constant", "EQFULL", (intmax_t)(EQFULL));
#endif
}

static void grovel_134(void)
{
#ifdef ERANGE
  grovel_intmax("constant", "ERANGE", (intmax_t)(ERANGE));
#endif
}

static void grovel_135(void)
{
#ifdef EREMCHG
  grovel_intmax("constant", "EREMCHG", (intmax_t)(EREMCHG));
#endif
}

static void grovel_136(void)
{
#ifdef EREMOTE
  grovel_intmax("constant", "EREMOTE", (intmax_t)(EREMOTE));
#endif
}

static void grovel_137(void)
{
#ifdef EREMOTEIO
  grovel_intmax("constant", "EREMOTEIO", (intmax_t)(EREMOTEIO));
#endif
}

static void grovel_138(void)
{
#ifdef ERESTART
  grovel_intmax("constant", "ERESTART", (intmax_t)(ERESTART));
#endif
}

static void grovel_139(void)
{
#ifdef ERFKILL
  grovel_intmax("constant", "ERFKILL", (intmax_t)(ERFKILL));
#endif
}

static void grovel_140(void)
{
#ifdef EROFS
  grovel_intmax("constant", "EROFS", (intmax_t)(EROFS));
#endif
}

static void grovel_141(void)
{
#ifdef ERPCMISMATCH
  grovel_intmax("constant", "ERPCMISMATCH", (intmax_t)(ERPCMISMATCH));
#endif
}

static void grovel_142(void)
{
#ifdef ESHLIBVERS
  grovel_intmax("constant", "ESHLIBVERS", (intmax_t)(ESHLIBVERS));
#endif
}

static void grovel_143(void)
{
#ifdef ESHUTDOWN
  grovel_intmax("constant", "ESHUTDOWN", (intmax_t)(ESHUTDOWN));
#endif
}

static void grovel_144(void)
{
#ifdef ESOCKTNOSUPPORT
  grovel_intmax("constant", "ESOCKTNOSUPPORT", (intmax_t)(ESOCKTNOSUPPORT));
#endif
}

static void grovel_145(void)
{
#ifdef ESPIPE
  grovel_intmax("constant", "ESPIPE", (intmax_t)(ESPIPE));
#endif
}

static void grovel_146(void)
{
#ifdef ESRCH
  grovel_intmax("constant", "ESRCH", (intmax_t)(ESRCH));
#endif
}

static void grovel_147(void)
{
#ifdef ESRMNT
  grovel_intmax("constant", "ESRMNT", (intmax_t)(ESRMNT));
#endif
}

static void grovel_148(void)
{
#ifdef ESTALE
  grovel_intmax("constant", "ESTALE", (intmax_t)(ESTALE));
#endif
}

static void grovel_149(void)
{
#ifdef ESTRPIPE
  grovel_intmax("constant", "ESTRPIPE", (intmax_t)(ESTRPIPE));
#endif
}

static void grovel_150(void)
{
#ifdef ETIME
  grovel_intmax("constant", "ETIME", (intmax_t)(ETIME));
#endif
}

static void grovel_151(void)
{
#ifdef ETIMEDOUT
  grovel_intmax("constant", "ETIMEDOUT", (intmax_t)(ETIMEDOUT));
#endif
}

static void grovel_152(void)
{
#ifdef ETOOMANYREFS
  grovel_intmax("constant", "ETOOMANYREFS", (intmax_t)(ETOOMANYREFS));
#endif
}

static void grovel_153(void)
{
#ifdef ETXTBSY
  grovel_intmax("constant", "ETXTBSY", (intmax_t)(ETXTBSY));
#endif
}

static void grovel_154(void)
{
#ifdef EUCLEAN
  grovel_intmax("constant", "EUCLEAN", (intmax_t)(EUCLEAN));
#endif
}

static void grovel_155(void)
{
#ifdef EUNATCH
  grovel_intmax("constant", "EUNATCH", (intmax_t)(EUNATCH));
#endif
}

static void grovel_156(void)
{
#ifdef EUSERS
  grovel_intmax("constant", "EUSERS", (intmax_t)(EUSERS));
#endif
}

static void grovel_157(void)
{
#ifdef EWOULDBLOCK
  grovel_intmax("constant", "EWOULDBLOCK", (intmax_t)(EWOULDBLOCK));
#endif
}

static void grovel_158(void)
{
#ifdef EXDEV
  grovel_intmax("constant", "EXDEV", (intmax_t)(EXDEV));
#endif
}

static void grovel_159(void)
{
#ifdef EXFULL
  grovel_intmax("constant", "EXFULL", (intmax_t)(EXFULL));
#endif
}

int main(void)
{
  grovel_1();
  grovel_2();
  grovel_3();
  grovel_4();
  grovel_5();
  grovel_6();
  grovel_7();
  grovel_8();
  grovel_9();
  grovel_10();
  grovel_11();
  grovel_12();
  grovel_13();
  grovel_14();
  grovel_15();
  grovel_16();
  grovel_17();
  grovel_18();
  grovel_19();
  grovel_20();
  grovel_21();
  grovel_22();
  grovel_23();
  grovel_24();
  grovel_25();
  grovel_26();
  grovel_27();
  grovel_28();
  grovel_29();
  grovel_30();
  grovel_31();
  grovel_32();
  grovel_33();
  grovel_34();
  grovel_35();
  grovel_36();
  grovel_37();
  grovel_38();
  grovel_39();
  grovel_40();
  grovel_41();
  grovel_42();
  grovel_43();
  grovel_44();
  grovel_45();
  grovel_46();
  grovel_47();
  grovel_48();
  grovel_49();
  grovel_50();
  grovel_51();
  grovel_52();
  grovel_53();
  grovel_54();
  grovel_55();
  grovel_56();
  grovel_57();
  grovel_58();
  grovel_59();
  grovel_60();
  grovel_61();
  grovel_62();
  grovel_63();
  grovel_64();
  grovel_65();
  grovel_66();
  grovel_67();
  grovel_68();
  grovel_69();
  grovel_70();
  grovel_71();
  grovel_72();
  grovel_73();
  grovel_74();
  grovel_75();
  grovel_76();
  grovel_77();
  grovel_78();
  grovel_79();
  grovel_80();
  grovel_81();
  grovel_82();
  grovel_83();
  grovel_84();
  grovel_85();
  grovel_86();
  grovel_87();
  grovel_88();
  grovel_89();
  grovel_90();
  grovel_91();
  grovel_92();
  grovel_93();
  grovel_94();
  grovel_95();
  grovel_96();
  grovel_97();
  grovel_98();
  grovel_99();
  grovel_100();
  grovel_101();
  grovel_102();
  grovel_103();
  grovel_104();
  grovel_105();
  grovel_106();
  grovel_107();
  grovel_108();
  grovel_109();
  grovel_110();
  grovel_111();
  grovel_112();
  grovel_113();
  grovel_114();
  grovel_115();
  grovel_116();
  grovel_117();
  grovel_118();
  grovel_119();
  grovel_120();
  grovel_121();
  grovel_122();
  grovel_123();
  grovel_124();
  grovel_125();
  grovel_126();
  grovel_127();
  grovel_128();
  grovel_129();
  grovel_130();
  grovel_131();
  grovel_132();
  grovel_133();
  grovel_134();
  grovel_135();
  grovel_136();
  grovel_137();
  grovel_138();
  grovel_139();
  grovel_140();
  grovel_141();
  grovel_142();
  grovel_143();
  grovel_144();
  grovel_145();
  grovel_146();
  grovel_147();
  grovel_148();
  grovel_149();
  grovel_150();
  grovel_151();
  grovel_152();
  grovel_153();
  grovel_154();
  grovel_155();
  grovel_156();
  grovel_157();
  grovel_158();
  grovel_159();
  return 0;
}
