(define-module (site kernels)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (site kernel)
  #:use-module (site site)
  #:export (make-symlink-kernel
             make-inkscape-kernel))

(define-public (system-quiet* . rest)
  (define port (apply open-pipe* `(,OPEN_READ ,@rest)))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (not (= exit-code 0))
    (display output))
  exit-code)

(define (mkdir-p dir)
  (define d "")
  (for-each
    (lambda (component)
      (set! d (string-append d component "/"))
      (if (not (file-exists? d)) (mkdir d)))
    (string-split dir #\/)))

(define (list-files path)
  (map
    (lambda (name) (string-append path "/" name))
    (scandir path (lambda (name) (not (string-prefix? "." name))))))

(define-public (make-minify-kernel input-file output-file)
  (make <kernel>
    #:name "minify"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:hash? #t
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (copy-file input-path output-path)
             (system-quiet* "css-html-js-minify" "--quiet" "--overwrite" output-path))))

(define-public (make-uglify-js-kernel input-file output-file)
  (make <kernel>
    #:name "uglify-js"
    #:input-files `(,input-file)
    #:output-files `(,output-file ,(format #f "~a.map" output-file))
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (system* "uglifyjs" "--compress" "--mangle" "--source-map"
                      "--output" output-path input-path))))

(define* (make-symlink-kernel input-file #:optional output-file #:key (site #f))
  (if (not output-file)
    (set! output-file (site-file-output-directory site input-file)))
  (make <kernel>
    #:name "symlink"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define (symlink-exists? path)
               (catch #t
                 (lambda () (lstat path) #t)
                 (lambda _ #f)))
             (for-each
               (lambda (input-file output-file)
                 (mkdir-p (dirname output-file))
                 (system* "ln" "-sfnr" input-file output-file))
               (kernel-input-files kernel)
               (kernel-output-files kernel))
             #t)))

(define-public (make-favicon-kernel input-file output-file)
  (make <kernel>
    #:name "favicon"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (system* "convert"
                      "-background" "transparent" input-path
                      "-resize" "256x256"
                      "-gravity" "center"
                      "-extent" "256x256"
                      "-define" "icon:auto-resize=16,24,32,48,64,72,96,128,256"
                      output-path))))

(define* (make-inkscape-kernel input-file output-file
                               #:key (export "png"))
  (make <kernel>
    #:name "inkscape"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (system* "inkscape" "--without-gui"
                      (cond
                        ((string=? export "png") (format #f "--export-png=~a" output-path))
                        ((string=? export "eps") (format #f "--export-eps=~a" output-path))
                        (else #f))
                      input-path))))

(define-public (make-css-kernels site directory)
  (map
    (lambda (file)
      (define output-file (site-output-path site file))
      (make-minify-kernel file output-file))
    (list-files directory)))

(define-public (make-js-kernels site directory)
  (map
    (lambda (file)
      (define output-file (site-output-path file))
      (make-uglify-js-kernel file output-file))
    (list-files directory)))
