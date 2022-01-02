(define-module (site generator)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (haunt html)
  #:use-module (site site)
  #:use-module (site page)
  #:use-module (site slide)
  #:use-module (site directory)
  #:use-module (site kernel))

(define %output-directory "build/rsync")
(define %rss-date "%a, %d %b %Y %H:%M:%S %z")

(define (write-atom-feed site pages path)
  (define (page->atom-feed-entry page)
    `(entry
       (title ,(page-title page))
       (link (@ (href ,(page-full-url site page))
                (rel "alternate")
                (type "text/html")))
       (id ,(string-append "urn:uuid:" (page-uuid page)))
       (updated ,(strftime %iso-date (page-date page)))
       (summary ,(page-abstract page))
       (content (@ (type "xhtml"))
                (div (@ (xmlns "http://www.w3.org/1999/xhtml"))
                     ,(page-content page)))
       (author
         (name ,(page-author page))
         ;;(email )
         )))
  (format (current-error-port) "write ~a\n" path)
  (call-with-output-file path
    (lambda (port)
      (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
      (sxml->xml `(feed (@ (xmlns "http://www.w3.org/2005/Atom"))
                        (title ,(site-description site))
                        ;;(subtitle ,(site-description site))
                        (link (@ (href ,(string-append (site-url site) "/" %atom-feed-path))
                                 (rel "self")))
                        (link (@ (href ,(site-url site))))
                        (id "urn:uuid:df3a118a-a4e1-486a-a093-606c986a6988")
                        (updated ,(strftime %iso-date (localtime (current-time))))
                        ,@(map page->atom-feed-entry pages))
                 port))))

(define (extension->mime-type url)
  (define extension (string-downcase (last (string-split url #\.))))
  (cond
    ((string=? extension "png") "image/png")
    ((string=? extension "svg") "image/svg+xml")
    ((string=? extension "jpg") "image/jpeg")
    (else (throw 'error (format #f "No MIME type for ~a" url)))))

(define (write-rss-feed site pages path)
  (define (page->rss-feed-entry page)
    (define url (page-full-url site page))
    `(item
       (title ,(page-title page))
       (link ,url)
       (description ,(page-abstract page))
       (author ,(page-author page))
       ;;(category)
       ;;(comments)
       (enclosure (@ (url ,(string-append (site-url site) (page-image page)))
                     (type ,(extension->mime-type (page-image page)))))
       (guid (@ (isPermaLink "true")) ,url)
       (pubDate ,(strftime %rss-date (page-date page)))
       ;;(source)
       ))
  (define current-date (strftime %rss-date (localtime (current-time))))
  (format (current-error-port) "write ~a\n" path)
  (call-with-output-file path
    (lambda (port)
      (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
      (sxml->xml `(rss (@ (version "2.0"))
                       (channel
                        (title ,(site-description site))
                        (link ,(string-append (site-url site) "/"))
                        (description ,(site-description site))
                        (language "en-gb")
                        (copyright ,(site-copyright site))
                        (managingEditor ,(site-email-rss site))
                        (webMaster ,(site-email-rss site))
                        (pubDate ,current-date)
                        (lastBuildDate ,current-date)
                        ;;(category )
                        (generator ,(string-append "Guile " (version)))
                        (docs "https://validator.w3.org/feed/docs/rss2.html")
                        ;;(cloud)
                        (ttl ,60)
                        (image
                          (url)
                          (title ,(site-description site))
                          (link ,(string-append (site-url site) "/"))
                          (width "90")
                          (height "36"))
                        ;;(textInput)
                        ;;(skipHours)
                        ;;(skipDays)
                        ,@(map page->rss-feed-entry pages)))
                 port))))

(define (write-json-feed site pages path)
  (format (current-error-port) "write ~a\n" path)
  (call-with-output-file path
    (lambda (port)
      (display "{}\n" port)))
  #t)

(define (write-sitemap site pages path)
  (format (current-error-port) "write ~a\n" path)
  (call-with-output-file path
    (lambda (port)
      (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
      (sxml->xml `(urlset (@ (xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"))
                          (url
                            (loc ,(string-append (site-url site) "/"))
                            (lastmod ,(strftime %iso-date (localtime (current-time))))
                            (changefreq "daily"))
                          ,@(map
                              (lambda (page)
                                `(url
                                   (loc ,(page-full-url site page))
                                   (lastmod ,(strftime %iso-date (page-date page)))
                                   (changefreq "daily")))
                              pages))
                 port))))

(define (write-robots-txt site path)
  (call-with-output-file path
    (lambda (port)
      (format port "User-agent: *\nAllow: /\nSitemap: ~a/~a\n" (site-url site) %sitemap-path))))

(define (site-generate-directories site)
  (define directories
    (scandir "src"
             (lambda (name)
               (and (eq? (stat:type (stat (string-append "src/" name))) 'directory)
                    (not (string-prefix? "." name))))))
  (set! (site-directories site)
    (sort
      (fold
        (lambda (name prev)
          (define index (format #f "src/~a/index.scm" name))
          (if (file-exists? index)
            (let ((dir (primitive-load index)))
              (set! (directory-path dir) name)
              (if (not (slot-bound? dir 'url))
                (set! (directory-url dir) name))
              (cons dir prev))
            prev))
        '()
        directories)
      (lambda (a b) (< (directory-number a) (directory-number b))))))

(define (make-all-page-kernels site index-page)
  (define pages-and-kernels
    (fold
      (lambda (directory prev)
        (define pages (all-pages directory))
        (define kernels
          (cons
            (make-index-page-kernel site pages directory index-page)
            (map (lambda (page) (make-page-kernel site page)) pages)))
        (cons (append pages (car prev))
              (append kernels (cdr prev))))
      '(() . ())
      (map directory-path (site-directories site))))
  (define pages (car pages-and-kernels))
  (define kernels
    (append
      `(,(make-feeds-kernel site pages)
         ,(make-robots-kernel site))
      (cdr pages-and-kernels)))
  kernels)

(define (write-all-pages site index-page)
  (kernels-process (make-all-page-kernels site index-page)))

(define (make-robots-kernel site)
  (define output-path (string-append %output-directory "/robots.txt"))
  (make <kernel>
    #:name "robots"
    #:input-files '()
    #:output-files `(,output-path)
    #:proc (lambda (kernel)
             (define output-path (first (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (write-robots-txt site output-path)          
             #t)))

(define (make-feeds-kernel site pages)
  (define output-paths
    `(,(string-append %output-directory "/" %atom-feed-path)
       ,(string-append %output-directory "/" %rss-feed-path)
       ,(string-append %output-directory "/" %json-feed-path)
       ,(string-append %output-directory "/" %sitemap-path)))
  (define procedures
    `(,write-atom-feed
       ,write-rss-feed
       ,write-json-feed
       ,write-sitemap))
  (make <kernel>
    #:name "feeds"
    #:input-files (map page-input-file pages)
    #:output-files output-paths
    #:proc (lambda (kernel)
             (for-each
               (lambda (output-path proc)
                 (mkdir-p (dirname output-path))
                 (proc site pages output-path))
               (kernel-output-files kernel)
               procedures)
             #t)))

(define (make-page-kernel site page)
  (define output-path
    (string-append (site-output-directory site) "/"
                   (let ((url (page-url page)))
                     (if (string-suffix? "/" url)
                       (string-append url "index.html")
                       url))))
  (define sxml 
    (if (not (page-foreign? page))
      (page-sxml site page)
      '()))
  (make <kernel>
    #:name "page"
    #:input-files (page-input-files page)
    #:output-files `(,output-path)
    #:proc (lambda (kernel)
             (define output-path (first (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (if (not (page-foreign? page))
               (page-write-sxml sxml site page output-path))
             #t)))

(define (make-index-page-kernel site pages directory index-page)
  (define output-path (string-append %output-directory "/" directory "/index.html"))
  ;;(pretty-print (page-content page))
  (make <kernel>
    #:name "index-page"
    #:input-files (map page-input-file pages)
    #:output-files `(,output-path)
    #:proc (lambda (kernel)
             (define output-path (first (kernel-output-files kernel)))
             (mkdir-p (dirname output-path))
             (page-write site
                         (index-page
                           site
                           (filter
                             (lambda (page)
                               (define slide? (is-a? page <slide>))
                               (or (and slide? (not (page-parent page)))
                                   (and (not slide?) (page-index? page))))
                             pages))
                         output-path)
             #t)))

(define (list-files path)
  (map
    (lambda (name) (string-append path "/" name))
    (scandir path (lambda (name) (not (string-prefix? "." name))))))

;; Deprecated. Use make-copy-kernels istead.
(define (copy-assets)
  (kernels-process
    (make-copy-kernels '("images"))))

(define (get-mime-type path)
  (define port (open-pipe* OPEN_READ "file" "--mime-type" "--brief" "--dereference" "-E" path))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (= exit-code 0) (string-trim-both output) ""))

(define (make-scour-kernel input-file output-file)
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

(define (make-copy-kernels directories)
  (append-map
    (lambda (dir)
      (define path (string-append "src/" dir))
      (map
        (lambda (name)
          (define output-path (string-append "build/rsync/" dir "/" name))
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

;; export all symbols
(module-map
 (lambda (sym var)
   (module-export! (current-module) (list sym)))
 (current-module))
