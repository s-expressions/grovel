#!

(import (scheme base))
(import (gambit match))
(import (c-groveler))

(define (deb x)
  (parameterize ((current-output-port (current-error-port)))
    (write x)
    (newline)))

(define (symbol-middle symbol prefix-string suffix-string)
  (let ((string (symbol->string symbol)))
    (and (>= (string-length string)
             (+ (string-length prefix-string)
                (string-length suffix-string)))
         (substring string
                    (string-length prefix-string)
                    (- (string-length string)
                       (string-length suffix-string))))))

(define (main input-filename)
  (let ((input (with-input-from-file input-filename read-all)))
    (let ((g (make-c-groveler)))
      (grovel-c-include g "errno.h")
      (for-each (lambda (form)
                  (match form
                    ((include ,header)
                     (cond ((and (symbol? header)
                                 (symbol-middle header "<" ">"))
                            => (lambda (filename)
                                 (grovel-c-include g filename)))
                           (else
                            (error "Huh include?" form))))
                    ((constant signed ,constant)
                     (grovel-c-constant-integer g constant))
                    ((constant-ifdef signed ,constant)
                     (grovel-c-constant-ifdef-integer g constant))
                    ((call-constant string ,function ,constant)
                     (grovel-c-call-constant-string g function constant))
                    ((call-constant-ifdef string ,function ,constant)
                     (grovel-c-call-constant-ifdef-string g function constant))
                    (,_
                     (error "Huh?" form))))
                input)
      (write-string (c-groveler->string g)))))

(apply main (cdr (command-line)))
