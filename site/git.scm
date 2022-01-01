(define-module (site git)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  )

(define (git . args)
  (define port (apply open-pipe* `(,OPEN_READ "git" ,@args)))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (= exit-code 0) (string-trim-both output) #f))

(define (git-list-files directory commit)
  (lines (git "-C" directory "ls-tree" "--name-only" "-r" commit)))

(define (git-show-file directory commit path)
  (git "-C" directory "show" (format #f "~a:~a" commit path)))

(define (git-list-commits directory commit)
  (lines (git "-C" directory "log" "--pretty=format:%H" commit)))

(define (lines str)
  (if str
    (string-split str #\newline)
    '()))

(define (get-mime-type path)
  (define port (open-pipe* OPEN_READ "file" "--mime-type" "--brief" "--dereference" "-E" path))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (= exit-code 0) (string-trim-both output) #f))

;; export all symbols
(module-map
  (lambda (sym var)
    (force-output)
    (module-export! (current-module) (list sym)))
  (current-module))
