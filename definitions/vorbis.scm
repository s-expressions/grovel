(include <vorbis/codec.h>)

;; TODO: vorbis_version_string()

(constant signed OV_FALSE)
(constant signed OV_EOF)
(constant signed OV_HOLE)

(constant signed OV_EREAD)
(constant signed OV_EFAULT)
(constant signed OV_EIMPL)
(constant signed OV_EINVAL)
(constant signed OV_ENOTVORBIS)
(constant signed OV_EBADHEADER)
(constant signed OV_EVERSION)
(constant signed OV_ENOTAUDIO)
(constant signed OV_EBADPACKET)
(constant signed OV_EBADLINK)
(constant signed OV_ENOSEEK)

(type-size vorbis_comment)
(type-slot vorbis_comment user_comments)
(type-slot vorbis_comment comment_lengths)
(type-slot vorbis_comment comments)
(type-slot vorbis_comment vendor)