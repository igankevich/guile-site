(define-module (site kernels)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (oop goops)
  #:use-module (site kernel)
  #:use-module (site site)
  #:use-module (srfi srfi-1)
  #:export (make-symlink-kernel
             make-inkscape-kernel
             make-webp-kernel
             make-julia-mono-kernels
             make-ubuntu-font-kernels
             make-rsync-kernel
             make-favicon-kernel
             make-gnuplot-kernel
             make-scour-kernel))

; https://loqbooq.app/blog/add-favicon-modern-browser-guide
(define-public %favicon-sizes '("16x16" "32x32" "48x48" "192x192" "167x167" "180x180"))

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
  (define status (stat path))
  (if (eq? (stat:type status) 'directory)
    (map
      (lambda (name) (string-append path "/" name))
      (scandir path (lambda (name) (not (string-prefix? "." name)))))
    `(,path)))

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
             (chmod output-path #o0644)
             (system-quiet* "css-html-js-minify" "--quiet" "--overwrite" output-path))))

(define-public (make-uglify-js-kernel input-file output-file)
  (make <kernel>
    #:name "uglify-js"
    #:input-files `(,input-file)
    #:output-files `(,output-file ,(format #f "~a.map" output-file))
    #:hash? #t
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

(define* (make-favicon-kernel input-file #:optional (output-file #f)
                              #:key (site #f))
  (define output-prefix (site-output-directory site "favicon/"))
  (make <kernel>
    #:name "favicon"
    #:input-files `(,input-file)
    #:output-files `(,(string-append output-prefix "any.svg")
                      ,(string-append output-prefix "any.ico")
                      ,@(map (lambda (size) (string-append output-prefix size ".png"))
                        %favicon-sizes))
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define tmp-path (site-output-directory site ".favicon.png"))
             (define svg-path (first (kernel-output-files kernel)))
             (define ico-path (second (kernel-output-files kernel)))
             (mkdir-p (dirname tmp-path))
             (system* "inkscape" "--without-gui"
                      (format #f "--export-png=~a" tmp-path)
                      input-path)
             (mkdir-p (dirname svg-path))
             ; svg
             (system* "ln" "-sfnr" input-path svg-path)
             ; png
             (for-each
               (lambda (size output-path)
                 (system* "convert"
                          "-background" "transparent" tmp-path
                          "-resize" size
                          "-gravity" "center"
                          "-extent" size
                          output-path))
               %favicon-sizes
               (drop (kernel-output-files kernel) 2))
             ; ico
             (system* "convert"
                      "-background" "transparent" tmp-path
                      "-resize" "256x256"
                      "-gravity" "center"
                      "-extent" "256x256"
                      "-define" "icon:auto-resize=16,24,32,48,64,72,96,128,256"
                      ico-path))))

(define* (make-inkscape-kernel input-file #:optional output-file #:key
                               (site #f)
                               (export "png")
                               (options '()))
  (if (and site (not output-file))
    (set! output-file (site-output-path site (replace-extension input-file export))))
  (make <kernel>
    #:name "inkscape"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (apply system*
                    `("inkscape" "--without-gui"
                      ,(cond
                         ((string=? export "png") (format #f "--export-png=~a" output-path))
                         ((string=? export "eps") (format #f "--export-eps=~a" output-path))
                         (else #f))
                      ,@options
                      ,input-path)))))

(define* (make-webp-kernel input-file #:optional (output-file #f)
                           #:key
                           (options '("-lossless" "-quiet"))
                           (site #f))
  (cond
    ((and site (not output-file))
     (set! output-file (site-output-directory site (replace-extension input-file "webp"))))
    ((not output-file)
     (set! output-file (replace-extension input-file "webp"))))
  (make <kernel>
    #:name "webp"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (apply system* `("cwebp" ,@options "-o" ,output-path ,input-path)))))

(define-public (make-webp-generator)
  (lambda (kernel)
    (define png-files
      (filter (lambda (path) (string-suffix? ".png" path))
              (kernel-output-files kernel)))
    (map make-webp-kernel png-files)))

(define-public (make-css-kernels site directory)
  (map
    (lambda (file)
      (define output-file (site-output-path site file))
      (make-minify-kernel file output-file))
    (list-files directory)))

(define-public (make-js-kernels site directory)
  (map
    (lambda (file)
      (define output-file (site-output-path site file))
      (make-uglify-js-kernel file output-file))
    (list-files directory)))

(define-public (guix-build site . args)
  (define args-hash (hash args 18446744073709551615))
  (define cache-file (format #f "~a/.guix-build/~x" (site-output-directory site) args-hash))
  (if (file-exists? cache-file)
    (let ((cached-path (string-trim-both (call-with-input-file cache-file get-string-all))))
      (if (file-exists? cached-path)
        cached-path
        (begin
          (delete-file cache-file)
          (apply guix-build `(,site ,@args)))))
    (let* ((port (apply open-pipe* `(,OPEN_READ "guix" "build" ,@args)))
           (output (string-trim-both (get-string-all port)))
           (exit-code (status:exit-val (close-pipe port))))
      (if (= exit-code 0)
        (begin
          (mkdir-p (dirname cache-file))
          (call-with-output-file cache-file
            (lambda (port) (display output port)))
          output)
        #f))))

(define-public (make-katex-css-kernels site)
  (define katex (guix-build site "-e" "(@ (gnu packages site) katex)"))
  (define out (site-output-directory site))
  `(,(make-minify-kernel
       (format #f "~a/katex.css" katex)
       (format #f "~a/katex/katex.css" out))))

(define-public (make-katex-js-kernels site)
  (define katex (guix-build site "-e" "(@ (gnu packages site) katex)"))
  (define out (site-output-directory site))
  `(,(make-uglify-js-kernel
       (format #f "~a/katex.js" katex)
       (format #f "~a/katex/katex.js" out))))

(define-public (make-katex-font-kernels site)
  (define katex (guix-build site "-e" "(@ (gnu packages site) katex)"))
  (define out (site-output-directory site))
  (map
    (lambda (path)
      (make-symlink-kernel path (format #f "~a/katex/fonts/~a" out (basename path))))
    (fold
      (lambda (name prev)
        (if (string-prefix? "." name)
          prev
          (cons (string-append katex "/fonts/" name) prev)))
      '()
      (scandir (format #f "~a/fonts" katex)))))

(define* (make-julia-mono-kernels site #:optional (output-subdirectory "fonts"))
  (define julia-mono (guix-build site "-e" "(@ (gnu packages fonts) font-juliamono)"))
  (define out (site-output-directory site))
  (map
    (lambda (path)
      (make-symlink-kernel path (format #f "~a/~a/~a" out output-subdirectory (basename path))))
    (fold
      (lambda (name prev)
        (if (string-prefix? "." name)
          prev
          (cons (string-append julia-mono "/share/fonts/truetype/" name) prev)))
      '()
      (scandir (format #f "~a/share/fonts/truetype" julia-mono)))))

(define* (make-ubuntu-font-kernels site #:optional (output-subdirectory "fonts"))
  (define julia-mono (guix-build site "-e" "(@ (gnu packages site) font-ubuntu)"))
  (define out (site-output-directory site))
  (map
    (lambda (path)
      (make-symlink-kernel path (format #f "~a/~a/~a" out output-subdirectory (basename path))))
    (fold
      (lambda (name prev)
        (if (string-prefix? "." name)
          prev
          (cons (string-append julia-mono "/share/fonts/truetype/" name) prev)))
      '()
      (scandir (format #f "~a/share/fonts/truetype" julia-mono)))))

(define-public (make-command-kernel name . args)
  (make <kernel>
    #:name name
    #:target? #t
    #:proc (lambda (kernel) (apply system* args))))

(define* (make-rsync-kernel #:key
                            (name "rsync")
                            (options '("-aL" "--exclude=.*" "--delete" "--info=progress2"))
                            (source #f)
                            (site #f)
                            (destination #f))
  (cond
    ((not destination) #f)
    ((and (not source) (not site)) #f)
    (else
      (apply make-command-kernel
             `(,name
                "rsync"
                ,@options
                ,(cond
                   (source source)
                   (site (string-append (site-output-directory site) "/")))
                ,destination)))))

(define-public (make-clean-kernel site)
  (make-command-kernel "clean" "rm" "-rf" "--one-file-system" (site-output-directory site)))

(define* (make-gnuplot-kernel input-files #:optional (output-files '()) #:key (site #f))
  (if site
    (let ((dir (dirname (site-file-output-directory site (first input-files)))))
      (set! output-files
        (map
          (lambda (file) (string-append dir "/" file))
          output-files))))
  (make <kernel>
    #:name "gnuplot"
    #:input-files input-files
    #:output-files output-files
    #:proc (lambda (kernel)
             (define input-path (first (kernel-input-files kernel)))
             (define output-path (first (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (system* "gnuplot"
                      "-e" (format #f "output_directory='~a'" (dirname output-path))
                      "-d" input-path))))

(define* (make-scour-kernel input-file #:optional output-file #:key (site #f))
  (if (not output-file)
    (set! output-file (site-file-output-directory site input-file)))
  (make <kernel>
    #:name "scour"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc
    (lambda (kernel)
      (define input-path (first (kernel-input-files kernel)))
      (define output-path (first (kernel-output-files kernel)))
      (define mime-type (get-mime-type input-path))
      (mkdir-p (dirname output-path))
      (system* "scour"
               "--enable-comment-stripping"
               "--enable-id-stripping"
               "--shorten-ids"
               "--indent=none"
               "--remove-descriptive-elements"
               "-i" input-path
               "-o" output-path))))

(define-public (make-copy-kernels site directories)
  (append-map
    (lambda (dir)
      (define path (string-append "src/" dir))
      (map
        (lambda (name)
          (define output-path (string-append (site-output-directory site) "/" dir "/" name))
          (define input-path (string-append "src/" dir "/" name))
          (make <kernel>
            #:name "copy"
            #:input-files `(,input-path)
            #:output-files `(,output-path)
            #:proc (lambda (kernel)
                     (define input-path (first (kernel-input-files kernel)))
                     (define output-path (first (kernel-output-files kernel)))
                     (define mime-type (get-mime-type input-path))
                     (mkdir-p (dirname output-path))
                     (define ret
                       (cond
                         ((string=? mime-type "image/svg+xml")
                          (let ((output-path-tmp (string-append output-path ".tmp")))
                            (and
                              (= 0 (system* "inkscape"
                                            (format #f "--export-plain-svg=~a" output-path-tmp)
                                            input-path))
                              (= 0 (system* "scour" "-i" output-path-tmp "-o" output-path)))))
                         (else
                           (copy-file input-path output-path)
                           #t)))
                     (chmod output-path #o0644)
                     ret)))
        (scandir path (lambda (name) (not (string-prefix? "." name))))))
    directories))

(define (get-mime-type path)
  (define port (open-pipe* OPEN_READ "file" "--mime-type" "--brief" "--dereference" "-E" path))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (= exit-code 0) (string-trim-both output) ""))
