(define-module (site kernels)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (oop goops)
  #:use-module (site kernel)
  #:use-module (site site)
  #:use-module (site page)
  #:use-module (site video)
  #:use-module (srfi srfi-1)
  #:use-module (haunt html)
  #:export (make-symlink-kernel
             make-inkscape-kernel
             make-webp-kernel
             make-woff-kernel
             make-woff2-kernel
             make-julia-mono-kernels
             make-ubuntu-font-kernels
             make-rsync-kernel
             make-favicon-kernel
             make-gnuplot-kernel
             make-scour-kernel
             make-poster-kernel
             make-videos-kernels
             make-video-kernels
             make-xournalpp-kernel
             make-xournalpp-thumbnail-kernel
             make-pdf-thumbnail-kernel
             make-generate-git-kernel
             make-viewstl-generator
             make-viewstl-js-kernels
             make-viewstl-kernel
             ))

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
                      ,(string-append (site-output-directory site) "/favicon.ico")
                      ,@(map (lambda (size) (string-append output-prefix size ".png"))
                        %favicon-sizes))
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define tmp-path (site-output-directory site ".favicon.png"))
             (define svg-path (first (kernel-output-files kernel)))
             (define ico-path (second (kernel-output-files kernel)))
             (define ico-path-2 (third (kernel-output-files kernel)))
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
                      ico-path)
             (system* "cp" ico-path ico-path-2)
             )))

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

(define* (make-woff-kernel input-file #:optional (output-file #f)
                           #:key
                           (options '())
                           (site #f))
  (cond
    ((and site (not output-file))
     (set! output-file (site-output-directory site (replace-extension input-file "woff"))))
    ((not output-file)
     (set! output-file (replace-extension input-file "woff"))))
  (make <kernel>
    #:name "woff"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (apply system* `("sfnt2woff" ,@options ,input-path)))))

(define-public (make-woff-generator)
  (lambda (kernel)
    (define output-files (kernel-output-files kernel))
    (define ttf-files
      (filter
        (lambda (path)
          (and
            (string-suffix? ".ttf" path)
            (let ((name (substring path 0 (- (string-length path) 4))))
              (not (member (string-append name ".woff") output-files)))))
        output-files))
    (map make-woff-kernel ttf-files)))

(define* (make-woff2-kernel input-file #:optional (output-file #f)
                            #:key
                            (options '())
                            (site #f))
  (cond
    ((and site (not output-file))
     (set! output-file (site-output-directory site (replace-extension input-file "woff2"))))
    ((not output-file)
     (set! output-file (replace-extension input-file "woff2"))))
  (make <kernel>
    #:name "woff2"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (apply system* `("woff2_compress" ,@options ,input-path)))))

(define-public (make-woff2-generator)
  (lambda (kernel)
    (define output-files (kernel-output-files kernel))
    (define ttf-files
      (filter
        (lambda (path)
          (and
            (string-suffix? ".ttf" path)
            (let ((name (substring path 0 (- (string-length path) 4))))
              (not (member (string-append name ".woff2") output-files)))))
        output-files))
    (map make-woff2-kernel ttf-files)))

;(define-public (make-optipng-generator)
;  (define (make-optipng-kernel input-file)
;    (make <kernel>
;      #:name "optipng"
;      #:input-files `(,input-file)
;      #:output-files `(,input-file)
;      #:proc (lambda (kernel)
;               (define input-path (car (kernel-input-files kernel)))
;               (define output-path (car (kernel-output-files kernel)))
;               (mkdir-p (dirname output-path))
;               (apply system* `("cwebp" ,@options "-o" ,output-path ,input-path)))))
;  (lambda (kernel)
;    (define png-files
;      (filter (lambda (path) (string-suffix? ".png" path))
;              (kernel-output-files kernel)))
;    (map make-webp-kernel png-files)))

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

(define-public (make-viewstl-js-kernels site)
  (define viewstl (guix-build site "-e" "(@ (gnu packages site) viewstl)"))
  (define out (site-output-directory site))
  (map
    (lambda (name)
      (define path (string-append viewstl "/" name))
      (make-symlink-kernel path (format #f "~a/viewstl/~a" out (basename path))))
    (scandir viewstl (lambda (name)
                       (or (string-suffix? ".js" name)
                           (string-suffix? ".js.map" name))))))

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
                          (let ((output-path-tmp (string-append output-path ".tmp.svg")))
                            (and
                              (= 0 (system* "inkscape"
                                            "--export-plain-svg"
                                            "--export-type=svg"
                                            (format #f "--export-filename=~a" output-path-tmp)
                                            input-path))
                              (= 0 (system* "scour" "-i" output-path-tmp "-o" output-path)))))
                         (else
                           (copy-file input-path output-path)
                           #t)))
                     (chmod output-path #o0644)
                     ret)))
        (scandir path (lambda (name) (not (string-prefix? "." name))))))
    directories))

(define %make-poster-kernel-arguments
  '("-size" "960x540"
    "-font" "Ubuntu-Regular"
    "-pointsize" "40"
    "-background" "#2e3440"
    "-fill" "#d8dee9"
    "-gravity" "Center"
    "-flatten"))

(define* (make-poster-kernel title output-file
                             #:key
                             (input-files '())
                             (arguments %make-poster-kernel-arguments))
  (make <kernel>
    #:name "poster"
    #:input-files input-files
    #:output-files `(,output-file)
    #:proc (lambda (kernel)
             (define output-file (car (kernel-output-files kernel)))
             (and
               (apply system*
                      `("convert" ,@arguments ,(format #f "caption:~a" title) ,output-file))
               (system* "optipng" "-quiet" output-file)))))

(define* (make-videos-kernels #:key
                              (site #f)
                              (pages-directory "src/notes")
                              (videos-directory "src/videos")
                              (year #f))
  (format (current-error-port) "WARNING: `make-videos-kernels` is deprecated. Please, use `make-video-kernels` instead.\n")
  (define suffix (if year (format #f "-~a" year) ""))
  `(,@(fold
        (lambda (path prev)
          (if (string-suffix? ".scm" path)
            prev
            (cons (make-symlink-kernel path #f #:site site) prev)))
        '()
        (list-files videos-directory))
     ,(make-poster-kernel "Coming soon"
                          (format #f "~a/videos/coming-soon.png" (site-output-directory site)))
     ,@(map
         (lambda (page)
           (make-poster-kernel (page-title page)
                               (format #f "~a/videos/~2,'0d-~a~a.png"
                                       (site-output-directory site)
                                       (page-number page) (page-name page)
                                       suffix)
                               #:input-files `(,(page-input-file page))))
         (all-pages pages-directory))))

(define* (make-video-kernels video #:key (site #f)
                             (poster-more-options '()))
  `(,@(map
        (lambda (input-file output-file)
          (make-symlink-kernel input-file (site-output-directory site output-file)))
        (video-input-files video)
        (video-output-files video))
     ,(let ((poster-output-file (replace-extension (car (video-output-files video)) "png")))
        (make-poster-kernel
          (video-title video)
          (site-output-directory site poster-output-file)
          #:input-files (video-input-files video)
          #:arguments (append %make-poster-kernel-arguments poster-more-options)))
     ))

(define* (make-xournalpp-kernel input-file #:optional output-file #:key (site #f))
  (if (and site (not output-file))
    (set! output-file (site-output-path site (replace-extension input-file "pdf"))))
  (make <kernel>
    #:name "xournalpp"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc
    (lambda (kernel)
      (define input-path (first (kernel-input-files kernel)))
      (define output-path (first (kernel-output-files kernel)))
      (mkdir-p (dirname output-path))
      (system* "xournalpp" (format #f "--create-pdf=~a" output-path) input-path))))

(define* (make-xournalpp-thumbnail-kernel input-file
                                          #:optional output-file
                                          #:key (site #f)
                                          (range "1-1")
                                          (width #f)
                                          (height #f))
  (if (and site (not output-file))
    (set! output-file (site-output-path site (replace-extension input-file "png"))))
  (make <kernel>
    #:name "xournalpp"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc
    (lambda (kernel)
      (define input-path (first (kernel-input-files kernel)))
      (define output-path (first (kernel-output-files kernel)))
      (mkdir-p (dirname output-path))
      (and
        (apply system* `("xournalpp"
                         ,(format #f "--create-img=~a" output-path)
                         ,@(if range
                             `(,(format #f "--export-range=~a" range))
                             '())
                         ,@(if width
                             `(,(format #f "--export-png-width=~a" width))
                             '())
                         ,@(if height
                             `(,(format #f "--export-png-height=~a" height))
                             '())
                         ,input-path))
        (system* "optipng" "-quiet" output-path)))))

(define* (make-pdf-thumbnail-kernel input-file
                                    #:optional output-file
                                    #:key (site #f) (page-number 0) (resize #f))
  (if (and site (not output-file))
    (set! output-file (site-output-path site (replace-extension input-file "png"))))
  (make <kernel>
    #:name "thumbnail"
    #:input-files `(,input-file)
    #:output-files `(,output-file)
    #:proc
    (lambda (kernel)
      (define input-path (first (kernel-input-files kernel)))
      (define output-path (first (kernel-output-files kernel)))
      (mkdir-p (dirname output-path))
      (and
        (apply system* `("convert"
                         ,@(if page-number
                             `(,(format #f "~a[~a]" input-path page-number))
                             `(,input-path))
                         ,@(if resize
                             `("-resize" ,resize)
                             '())
                         ,output-path))
        (system* "optipng" "-quiet" output-path)))))

(define (make-generate-git-kernel input-directory site)
  (define output-directory
    (site-output-directory site (string-append (basename input-directory) ".git")))
  (define input-files '())
  (nftw input-directory
        (lambda (filename statinfo flag base level)
          (if (not (string-prefix? "." (basename filename)))
            (set! input-files (cons filename input-files)))
          #t)
        'mount
        'physical)
  (make <kernel>
    #:name "generate-git"
    #:input-files input-files
    #:output-files `(,(string-append output-directory "/HEAD"))
    #:proc (lambda (kernel)
             (define (git . args)
               (apply system* `("git" "-C" ,output-directory ,@args)))
             (system* "rsync" "-a"
                      "--exclude" "*~"
                      "--exclude" "build"
                      "--delete" "--delete-excluded"
                      input-directory output-directory)
             (git "init" "--quiet")
             (git "add" "--all")
             (git "commit" "-a" "-m" "initial" "--quiet")
             (git "update-server-info")
             (system* "sh" "-c" (format #f "outdir='~a'
rm -rf $outdir/*
mv $outdir/.git/* $outdir
rm -d $outdir/.git" output-directory))
             )))

(define* (make-viewstl-kernel input-file #:optional (output-file #f)
                              #:key (site #f))
  (set! output-file (replace-extension input-file "html"))
  (define stl-output-path (remove-site-output-directory site input-file))
  (define js-output-path (replace-extension output-file "js"))
  (define js-url (basename js-output-path))
  (define css-output-path (replace-extension output-file "css"))
  (define css-url (basename css-output-path))
  (make <kernel>
    #:name "viewstl"
    #:input-files `(,input-file)
    #:output-files `(,output-file ,js-output-path ,css-output-path)
    #:proc (lambda (kernel)
             (define input-path (car (kernel-input-files kernel)))
             (define output-path (car (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (call-with-output-file js-output-path
               (lambda (port)
                 (format port "var stl = document.getElementById('stl');\n")
                 (format port "var config = {models:[{id: 0, filename:'~a/~a'}]};\n"
                         (site-prefix site) stl-output-path)
                 (format port "var stlViewer = new StlViewer(stl, config);\n")
                 (format port "var stlDisplay = document.getElementById('stl-display');\n")
                 (format port "var updateDisplay = function () { stlViewer.set_display(0, stlDisplay.value); }\n")
                 (format port "stlDisplay.addEventListener('change', updateDisplay);\n")
                 ))
             (call-with-output-file css-output-path
               (lambda (port)
                 (format port ".stl-container { width: 100vw; height: 100vh; margin: 0 0 0 0; }")
                 (format port ".stl-buttons { width: auto; height: auto; position: absolute; top: 0px; left: 0px; }")))
             (define sxml
               `((doctype html)
                 (html (@ (lang "ru"))
                       (head
                         (meta (@ (charset "utf-8")))
                         (meta (@ (name "viewport")
                                  (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
                         (meta (@ (http-equiv "X-UA-Compatible") (content "IE=edge")))
                         (link (@ (rel "stylesheet") (href ,css-url)))
                         (title ,(basename input-file)))
                       (body (@ (lang "ru"))
                             (div (@ (id "stl") (class "stl-container")))
                             (div (@ (class "stl-buttons"))
                                  (select (@ (id "stl-display"))
                                          (option (@ (value "flat")) "Плоское тело")
                                          (option (@ (value "smooth")) "Сглаженное тело")
                                          (option (@ (value "wireframe")) "Каркас")))
                             (script (@ (src ,(format #f "~a/viewstl/stl_viewer.min.js"
                                                      (site-prefix site)))))
                             (script (@ (src ,js-url)))))))
             (call-with-output-file output-path
               (lambda (port)
                 (display (sxml->html-string sxml) port))))))

(define-public (make-viewstl-generator site)
  (lambda (kernel)
    (define stl-files
      (filter (lambda (path) (string-suffix? ".stl" path))
              (kernel-output-files kernel)))
    (map
      (lambda (file) (make-viewstl-kernel file #:site site))
      stl-files)))

; TODO https://github.com/pts/pdfsizeopt
