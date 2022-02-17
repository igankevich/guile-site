(define-module (site vtt)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple))

(define-class <caption> ()
  (text #:init-keyword #:text #:accessor caption-text #:init-value "")
  (start-time #:init-keyword #:start-time #:accessor caption-start-time #:init-value 0)
  (end-time #:init-keyword #:end-time #:accessor caption-end-time #:init-value 0))

(define (caption->string caption)
  (format #f "text ~a start-time ~a end-time ~a"
          (caption-text caption)
          (caption-start-time caption)
          (caption-end-time caption)))

(define %rx-webvtt (make-regexp "([0-9]{2}+:[0-9]{2}+:[0-9]{2}\\.[0-9]{3})\\s*-->\\s*([0-9]{2}+:[0-9]{2}+:[0-9]{2}\\.[0-9]{3})"))
(define %rx-vtt-timestamp (make-regexp "([0-9]{2}+):([0-9]{2}+):([0-9]{2})\\.([0-9]{3})"))
(define %rx-time-1 (make-regexp "^([0-9]{1,2})$"))
(define %rx-time-2 (make-regexp "^([0-9]{1,2}):([0-9]{1,2})$"))
(define %rx-time-3 (make-regexp "^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})$"))
(define %rx-time-4 (make-regexp "^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})\\.([0-9]{1,3})$"))

(define-public (list->captions lst)
  (map
    (lambda (item start-time)
      (define end-time (list-ref item 0))
      (define text (list-ref item 1))
      (make <caption>
        #:text text
        #:start-time (vtt-timestamp->seconds start-time)
        #:end-time (vtt-timestamp->seconds end-time)))
    lst
    (cons "00:00:00.000" (map car (drop-right lst 1)))))

(define-public (alist->list lst)
  (map
    (lambda (item start-time)
      (define end-time (car item))
      (define caption (cdr item))
      `(,start-time ,end-time ,caption))
    lst
    (cons "00:00:00.000" (map car (cdr lst)))))

(define-public (alist->vtt port lst)
  (display "WEBVTT\n\n" port)
  (map
    (lambda (item prev)
      (format port "~a --> ~a\n~a\n\n"
              (list-ref item 0)
              (list-ref item 1)
              (list-ref item 2)))
    (alist->list lst)))

(define-public (vtt->alist port)
  (define lines (string-split (get-string-all port) #\newline))
  (define cues
    (reverse
      (fold
        (lambda (line prev)
          (cond
            ((string-null? line) (cons "" prev))
            ((null? prev) `(,line))
            (else
              (let ((str (car prev)))
                (cons (string-append str (if (string-null? str) "" "\n") line) (cdr prev))))))
        '()
        lines)))
  (define objects
    (fold-right
      (lambda (line prev)
        (cond
          ((string-null? line) prev)
          ((string=? line "WEBVTT") prev)
          (else
            (let* ((more-lines (string-split line #\newline))
                   (match (regexp-exec %rx-webvtt (car more-lines))))
              (if match
                (cons (cons ;(vtt-timestamp->seconds (match:substring match 1))
                            (vtt-timestamp->seconds (match:substring match 2))
                            (string-join (cdr more-lines) "\n")) prev)
                prev)))))
      '()
      cues))
  objects)

(define-public (captions->html lst video-id)
  (define last-timestamp (if (null? lst) 0 (caption-start-time (last lst))))
  `(ul (@ (class "video-captions"))
       ,@(map
           (lambda (caption)
             (define start-time (caption-start-time caption))
             (define end-time (caption-end-time caption))
             (define text (caption-text caption))
             (define caption-sxml (cdr (xml->sxml (format #f "<span>~a</span>" text))))
             `(li (a (@ (href "#")
                        (data-time ,(number->string start-time))
                        (data-video ,video-id)
                        (class "video-caption")) ,(seconds->string start-time last-timestamp))
                  " "
                  ,@caption-sxml
                  ))
           lst)))

(define (vtt-timestamp->seconds timestamp)
  (define milliseconds 0)
  (define seconds 0)
  (define minutes 0)
  (define hours 0)
  (define (substring->number match n)
    (string->number (match:substring match n)))
  (define rules
    `((,%rx-time-4 . ,(lambda (match)
                        (set! milliseconds (substring->number match 4))
                        (set! seconds (substring->number match 3))
                        (set! minutes (substring->number match 2))
                        (set! hours (substring->number match 1))))
      (,%rx-time-3 . ,(lambda (match)
                        (set! seconds (string->number (match:substring match 3)))
                        (set! minutes (string->number (match:substring match 2)))
                        (set! hours (string->number (match:substring match 1)))))
      (,%rx-time-2 . ,(lambda (match)
                        (set! seconds (string->number (match:substring match 2)))
                        (set! minutes (string->number (match:substring match 1)))))
      (,%rx-time-1 . ,(lambda (match)
                        (set! seconds (string->number (match:substring match 1)))))))
  (fold
    (lambda (rule prev)
      (cond
        ((not prev)
         (let* ((rx (car rule))
                (proc (cdr rule))
                (match (regexp-exec rx timestamp)))
           (if match
             (begin
               (proc match)
               #t)
             #f)))
        (else
          prev)))
    #f
    rules)
  (exact->inexact
    (+
      (* hours 60 60)
      (* minutes 60)
      seconds
      (* milliseconds 1e-3))))

(define (seconds->string seconds max-timestamp)
  (define s (truncate seconds))
  (define (integer x)
    (inexact->exact (truncate x)))
  (define (get-hours t) (integer (/ t 3600)))
  (define (get-minutes t) (integer (/ (remainder t 3600) 60)))
  (define (get-seconds t) (integer (remainder (remainder t 3600) 60)))
  (cond
    ((> max-timestamp 3600)
     ;(strftime "%H:%M:%S" (gmtime (integer seconds)))
     (format #f "~2,'0d:~2,'0d:~2,'0d"
             (get-hours seconds) (get-minutes seconds) (get-seconds seconds))
     )
    (else
      (strftime "%M:S" (gmtime (integer seconds))))))
