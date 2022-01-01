(define-module (site site)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format))

(define-class <site> ()
  (url #:init-keyword #:url #:accessor site-url)
  (prefix #:init-keyword #:prefix #:accessor site-prefix)
  (description #:init-keyword #:description #:accessor site-description)
  (author #:init-keyword #:author #:accessor site-author)
  (email #:init-keyword #:email #:accessor site-email)
  (year #:init-keyword #:year #:accessor site-year)
  (directories #:init-keyword #:directories #:accessor site-directories #:init-value '())
  (output-directory #:init-keyword #:output-directory #:accessor site-output-directory
                    #:init-value "build/rsync"))

(define-method (site-email-rss (s <site>))
  (format #f "~a (~a)" (site-email s) (site-author s)))

(define (site-prefix/ site . rest)
  (string-join (cons (site-prefix site) rest) "/"))

(define (site-prefix// site . rest)
  (define path (string-join rest "/"))
  (define prefix (string-append (site-output-directory site) "/"))
  (site-prefix/ site
                (if (string-prefix? prefix path)
                  (substring path (string-length prefix))
                  path)))

(define (site-copyright site)
  (define (years start end)
    (if (string=? start end) start (string-append start "–" end)))
  (define page-year (format #f "~a" (site-year site)))
  (define current-year (strftime "%Y" (localtime (current-time))))
  (format #f "© ~a ~a" (years page-year current-year) (site-author site)))

(define (get-output-subdirectory path)
  (let ((components (string-split path #\/)))
    (if (string=? (car components) "src")
      (string-join (drop-right! (cdr components) 1) "/")
      (string-join components "/"))))

(define (site-file-output-directory site path)
  (string-join
    `(,(site-output-directory site)
       ,(get-output-subdirectory path)
       ,(basename path))
    "/"))

;; export all symbols
(module-map
  (lambda (sym var)
    (module-export! (current-module) (list sym)))
  (current-module))

