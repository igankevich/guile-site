(define-module (site video)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (site page)
  #:use-module (site vtt)
  #:use-module (site site)
  )

(define-class <video> ()
  (id #:init-keyword #:id #:accessor video-id #:init-value #f)
  (input-files #:init-keyword #:input-files #:accessor video-input-files #:init-value '())
  (output-files #:init-keyword #:output-files #:accessor video-output-files #:init-value '())
  (captions #:init-keyword #:captions #:accessor video-captions #:init-value '())
  (poster-url #:init-keyword #:poster-url #:accessor video-poster-url #:init-value #f)
  (coming-soon-url #:init-keyword #:coming-soon-url #:accessor video-coming-soon-url #:init-value #f)
  (sxml #:init-keyword #:sxml #:accessor video-sxml #:init-value
        (lambda (site video)
          (define video?
            (fold
              (lambda (path prev) (if prev prev (file-exists? path)))
              #f
              (video-input-files video)))
          (if video?
            `(div
               (video (@ (controls "controls")
                         (poster ,(site-prefix/ site (video-poster-url video)))
                         (id ,(video-id video)))
                      ,@(map
                          (lambda (input-path output-path)
                            `(source (@ (src ,(site-prefix/ site output-path))
                                        (type ,(get-mime-type input-path)))))
                          (video-input-files video)
                          (video-output-files video)))
               ,@(let ((captions (video-captions video)))
                   (cond
                     ((not (null? captions))
                      `(,(captions->html captions (video-id video))))
                     (else
                       '()))))
            `(img (@ (class "image") (alt "Coming soon") (src ,(video-coming-soon-url video)))))
          )))

;; export all symbols
(module-map
 (lambda (sym var)
   (force-output)
   (module-export! (current-module) (list sym)))
 (current-module))
