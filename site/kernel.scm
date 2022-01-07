(define-module (site kernel)
  #:use-module (haunt html)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (web uri))

(define-class <kernel> ()
  (name #:init-keyword #:name #:accessor kernel-name #:init-value #f)
  (input-data #:init-keyword #:input-data #:accessor kernel-input-data #:init-value '())
  (input-files #:init-keyword #:input-files #:accessor kernel-input-files #:init-value '())
  (output-files #:init-keyword #:output-files #:accessor kernel-output-files #:init-value '())
  (proc #:init-keyword #:proc #:accessor kernel-proc #:init-value (lambda (kernel) #t))
  (hash? #:init-keyword #:hash? #:accessor kernel-hash? #:init-value #f)
  (target? #:init-keyword #:target? #:accessor kernel-target? #:init-value #f))

(define (kernel-hash kernel)
  (hash kernel 4294967295))

(define (kernel-up-to-date? kernel)
  (define input-files (kernel-input-files kernel))
  (define output-files (kernel-output-files kernel))
  (define now (current-time))
  (define (file->mtime file default-mtime)
    (define status (stat file #f))
    (if (not status) default-mtime (stat:mtime status)))
  (define max-input-mtime
    (fold
      (lambda (file prev)
        (define mtime (file->mtime file now))
        ;(format #t "input-file ~a mtime ~a\n" file mtime)
        (if (> mtime prev) mtime prev))
      0
      input-files))
  (define min-output-mtime
    (fold
      (lambda (file prev)
        (cond
          ((= prev 0) prev)
          (else
            (let ((mtime (file->mtime file 0)))
              ;(format #t "output-file ~a mtime ~a\n" file mtime)
              (if (< mtime prev) mtime prev)))))
      now
      output-files))
  ;(format #t "~a: max-input-mtime ~a min-output-mtime ~a\n"
  ;        (kernel-name kernel) max-input-mtime min-output-mtime)
  (and (not (null? input-files))
       (<= max-input-mtime min-output-mtime)))

(define (path+hash path)
  (define h (hash (call-with-input-file path get-string-all) 4294967295))
  (define filename (basename path))
  (define parts (string-split filename #\.))
  (define name (string-join (drop-right parts 1) "."))
  (define extension (last parts))
  (format #f "~a/~a-~x.~a" (dirname path) name h extension))

(define %kernel-table (make-hash-table))
(define %kernel-mutex (make-mutex))

(define (kernel-run kernel)
  (if (kernel-up-to-date? kernel)
    #t
    (let ()
      (define key (kernel-hash kernel))
      (define ret
        (with-mutex %kernel-mutex
          (hash-ref %kernel-table key 'no-ret)))
      (if (not (eq? ret 'no-ret))
        ret
        (begin
          (format #t "~a:~x: ~a -> ~a\n"
                  (kernel-name kernel) key
                  (kernel-input-files kernel) (kernel-output-files kernel))
          (set! ret ((kernel-proc kernel) kernel))
          (with-mutex %kernel-mutex
            (hash-set! %kernel-table key ret))
          (if (kernel-hash? kernel)
            (set! (kernel-output-files kernel)
              (map
                (lambda (old-path)
                  (define new-path (path+hash old-path))
                  (catch #t
                    (lambda () (delete-file new-path))
                    (lambda _ #t))
                  (symlink (basename old-path) new-path)
                  new-path)
                (kernel-output-files kernel))))
          ret)))))

(define (get-kernel-target)
  (let ((args (command-line)))
    (if (>= (length args) 2)
      (list-ref args 1)
      #f)))

(define* (kernels-process kernels #:optional (generators '()))
  (define target (get-kernel-target))
  (for-each
    (lambda (kernel)
      (catch #t
        (lambda ()
          (let ((ret (kernel-run kernel)))
            (if (or (and (number? ret) (not (= ret 0))) (not ret))
              (begin
                ;; Delete output files that might have been generated.
                (for-each
                  (lambda (file)
                    (catch #t
                      (lambda () (delete-file file))
                      (lambda _ #t)))
                  (kernel-output-files kernel))
                (format #t "error: ~a: ~a failed with exit code ~a\n"
                        (kernel-name kernel) (kernel-output-files kernel) ret)))))
        (lambda (key . parameters)
          (format #t "error: ~a: ~a failed: ~a\n"
                  (kernel-name kernel) (kernel-output-files kernel)
                  (cons key parameters))))
      ;(format #t "~a: ~a is up to date\n" (kernel-name kernel) (kernel-output-files kernel))
      )
    (append-map
      (lambda (kernel)
        (cons kernel (append-map (lambda (generator) (generator kernel)) generators)))
      (filter
        (lambda (kernel)
          (or (and (not target) (not (kernel-target? kernel)))
              (and target (string=? target (kernel-name kernel)))))
        kernels))))

;; export all symbols
(module-map
  (lambda (sym var)
    (module-export! (current-module) (list sym)))
  (current-module))
