(include <openssl/ssl.h>)

(constant unsigned SSL_SESSION_ASN1_VERSION)

(constant string SSL_TXT_NULL_WITH_MD5)
(constant string SSL_TXT_RC4_128_WITH_MD5)
(constant string SSL_TXT_RC4_128_EXPORT40_WITH_MD5)
(constant string SSL_TXT_RC2_128_CBC_WITH_MD5)
(constant string SSL_TXT_RC2_128_CBC_EXPORT40_WITH_MD5)
(constant string SSL_TXT_IDEA_128_CBC_WITH_MD5)
(constant string SSL_TXT_DES_64_CBC_WITH_MD5)
(constant string SSL_TXT_DES_64_CBC_WITH_SHA)
(constant string SSL_TXT_DES_192_EDE3_CBC_WITH_MD5)
(constant string SSL_TXT_DES_192_EDE3_CBC_WITH_SHA)

(constant string SSL_TXT_KRB5_DES_64_CBC_SHA)
(constant string SSL_TXT_KRB5_DES_192_CBC3_SHA)
(constant string SSL_TXT_KRB5_RC4_128_SHA)
(constant string SSL_TXT_KRB5_IDEA_128_CBC_SHA)
(constant string SSL_TXT_KRB5_DES_64_CBC_MD5)
(constant string SSL_TXT_KRB5_DES_192_CBC3_MD5)
(constant string SSL_TXT_KRB5_RC4_128_MD5)
(constant string SSL_TXT_KRB5_IDEA_128_CBC_MD5)

(constant string SSL_TXT_KRB5_DES_40_CBC_SHA)
(constant string SSL_TXT_KRB5_RC2_40_CBC_SHA)
(constant string SSL_TXT_KRB5_RC4_40_SHA)
(constant string SSL_TXT_KRB5_DES_40_CBC_MD5)
(constant string SSL_TXT_KRB5_RC2_40_CBC_MD5)
(constant string SSL_TXT_KRB5_RC4_40_MD5)

(constant string SSL_TXT_KRB5_DES_40_CBC_SHA)
(constant string SSL_TXT_KRB5_DES_40_CBC_MD5)
(constant string SSL_TXT_KRB5_DES_64_CBC_SHA)
(constant string SSL_TXT_KRB5_DES_64_CBC_MD5)
(constant string SSL_TXT_KRB5_DES_192_CBC3_SHA)
(constant string SSL_TXT_KRB5_DES_192_CBC3_MD5)

(constant unsigned SSL_MAX_KRB5_PRINCIPAL_LENGTH)

(constant unsigned SSL_MAX_SSL_SESSION_ID_LENGTH)
(constant unsigned SSL_MAX_SID_CTX_LENGTH)

(constant unsigned SSL_MIN_RSA_MODULUS_LENGTH_IN_BYTES)
(constant unsigned SSL_MAX_KEY_ARG_LENGTH)
(constant unsigned SSL_MAX_MASTER_KEY_LENGTH)

(constant string SSL_TXT_LOW)
(constant string SSL_TXT_MEDIUM)
(constant string SSL_TXT_HIGH)

(constant string SSL_TXT_kFZA)
(constant string SSL_TXT_aFZA)
(constant string SSL_TXT_eFZA)
(constant string SSL_TXT_FZA)

(constant string SSL_TXT_aNULL)
(constant string SSL_TXT_eNULL)
(constant string SSL_TXT_NULL)

(constant string SSL_TXT_kRSA)
(constant string SSL_TXT_kDHr)
(constant string SSL_TXT_kDHd)
(constant string SSL_TXT_kDH)
(constant string SSL_TXT_kEDH)
(constant string SSL_TXT_kKRB5)
(constant string SSL_TXT_kECDHr)
(constant string SSL_TXT_kECDHe)
(constant string SSL_TXT_kECDH)
(constant string SSL_TXT_kEECDH)
(constant string SSL_TXT_kPSK)
(constant string SSL_TXT_kGOST)
(constant string SSL_TXT_kSRP)

(constant string SSL_TXT_aRSA)
(constant string SSL_TXT_aDSS)
(constant string SSL_TXT_aDH)
(constant string SSL_TXT_aECDH)
(constant string SSL_TXT_aKRB5)
(constant string SSL_TXT_aECDSA)
(constant string SSL_TXT_aPSK)
(constant string SSL_TXT_aGOST94)
(constant string SSL_TXT_aGOST01)
(constant string SSL_TXT_aGOST)

(constant string SSL_TXT_DSS)
(constant string SSL_TXT_DH)
(constant string SSL_TXT_DHE)
(constant string SSL_TXT_EDH)
(constant string SSL_TXT_ADH)
(constant string SSL_TXT_RSA)
(constant string SSL_TXT_ECDH)
(constant string SSL_TXT_ECDHE)
(constant string SSL_TXT_EECDH)
(constant string SSL_TXT_AECDH)
(constant string SSL_TXT_ECDSA)
(constant string SSL_TXT_KRB5)
(constant string SSL_TXT_PSK)
(constant string SSL_TXT_SRP)

(constant string SSL_TXT_DES)
(constant string SSL_TXT_3DES)
(constant string SSL_TXT_RC4)
(constant string SSL_TXT_RC2)
(constant string SSL_TXT_IDEA)
(constant string SSL_TXT_SEED)
(constant string SSL_TXT_AES128)
(constant string SSL_TXT_AES256)
(constant string SSL_TXT_AES)
(constant string SSL_TXT_AES_GCM)
(constant string SSL_TXT_CAMELLIA128)
(constant string SSL_TXT_CAMELLIA256)
(constant string SSL_TXT_CAMELLIA)
(constant string SSL_TXT_CHACHA20)

(constant string SSL_TXT_AEAD)
(constant string SSL_TXT_MD5)
(constant string SSL_TXT_SHA1)
(constant string SSL_TXT_SHA)
(constant string SSL_TXT_GOST94)
(constant string SSL_TXT_GOST89MAC)
(constant string SSL_TXT_SHA256)
(constant string SSL_TXT_SHA384)
(constant string SSL_TXT_STREEBOG256)
(constant string SSL_TXT_STREEBOG512)

(constant string SSL_TXT_DTLS1)
(constant string SSL_TXT_SSLV2)
(constant string SSL_TXT_SSLV3)
(constant string SSL_TXT_TLSV1)
(constant string SSL_TXT_TLSV1_1)
(constant string SSL_TXT_TLSV1_2)

;; (when (defined LIBRESSL_HAS_TLS1_3) (constant string SSL_TXT_TLSV1_3))

(constant string SSL_TXT_EXP)
(constant string SSL_TXT_EXPORT)

(constant string SSL_TXT_ALL)