(define-module (site lang)
  #:use-module (ice-9 regex)
  #:use-module (site site)
  #:use-module (site page)
  #:use-module (site slide)
  #:use-module (srfi srfi-1))

(define %rx-space (make-regexp "\\s+"))

(define (date str)
  (let ((tm (car (strptime %iso-date str))))
    (set-tm:zone tm (tm:zone (localtime (current-time))))
    (cdr (mktime tm))))

(define (make-slides site slides parent)
  (define slides-2
    (map
      (lambda (page number)
        (set! (page-keywords page) (page-keywords parent))
        (set! (page-date page) (page-date parent))
        (set! (page-abstract page) (page-abstract parent))
        (set! (page-title page) (format #f "~a / Слайд ~a" (page-title parent) number))
        (set! (page-author page) (page-author parent))
        (set! (page-image page) (page-image parent))
        (set! (page-uuid page) (format #f "~a-~a" (page-uuid parent) number))
        (set! (page-urls page) `(,(format #f "~a~a/" (page-url parent) number)))
        (set! (page-number page) number)
        (set! (page-parent page) parent)
        page)
      slides
      (iota (length slides) 1)))
  (define slides-3
    (fold
      (lambda (page prev)
        (if (not (null? prev))
          (set! (slide-previous page) (car prev)))
        (cons page prev))
      '()
      slides-2))
  (define slides-4
    (fold
      (lambda (page prev)
        (if (not (null? prev))
          (set! (slide-next page) (car prev)))
        (cons page prev))
      '()
      slides-3))
  (for-each
    (lambda (page) (set! (slide-all page) slides-4))
    slides-4)
  (set! (page-content parent)
    `((h1 (@ (class "text-center mb-2 mt-2")) ,(page-title parent))
      (div (@ (class "row"))
           ,@(map
               (lambda (page num)
                 `(div (@ (class ,(if (page-hidden? page) "opacity-1 col-sm-3" "col-sm-3")))
                       (a (@ (href ,(site-prefix/ site (page-url page))))
                          (img (@ (src "")
                                  (alt ,(page-title page))
                                  (class "rounded img-fluid shadow m-1"))))))
               slides-4
               (iota (length slides-4) 1)))))
  (cons parent slides-4))

(define (read-string-verbatim second-character port)
  (define text
    (call-with-output-string
      (lambda (output-port)
        (define previous-character #\nul)
        (while #t
          (let ((ch (read-char port)))
            (cond
              ((not (char? ch))
               (break))
              ((and (char=? ch second-character)
                    (let ((next-character (peek-char port)))
                      (and (char? next-character) (char=? next-character #\#))))
               (read-char port)
               (break))
              (else
                (display ch output-port))))))))
  (regexp-substitute/global #f %rx-space text 'pre " " 'post))

;; export all symbols
(module-map
 (lambda (sym var)
   (module-export! (current-module) (list sym)))
 (current-module))
