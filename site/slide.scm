(define-module (site slide)
  #:use-module (oop goops)
  #:use-module (site page)
  #:use-module (site site))

(define-class <slide> (<page>)
  (next #:init-keyword #:next #:accessor slide-next #:init-value #f)
  (previous #:init-keyword #:previous #:accessor slide-previous #:init-value #f)
  (all #:init-keyword #:all #:accessor slide-all #:init-value '()))

(define (slide-head site page)
  `(head
     ,@((page-head-common page) site page)))

(define (slide-tail site page)
  (define footer
    `((nav (@ (class "fixed-bottom"))
              (ul (@ (class "list-inline mb-0"))
                  (li (@ (class "list-inline-item"))
                      (a (@ (href ,(site-prefix/ site (page-url (page-parent page))))
                            (id "terminate")) "✖"))
                  (li (@ (class "list-inline-item"))
                      ,(if (slide-previous page)
                         `(a (@ (href ,(site-prefix/ site (page-url (slide-previous page))))
                                (id "previous")) "⟵")
                         `(span (@ (class "text-muted")) "⟵")))
                  ,@(map
                      (lambda (sub-page)
                        `(li (@ (class "list-inline-item"))
                             ,(if (string=? (page-url page) (page-url sub-page))
                                (page-number sub-page)
                                `(a (@ (href ,(site-prefix/ site (page-url sub-page)))) ,(page-number sub-page)))))
                      (slide-all page))
                  (li (@ (class "list-inline-item"))
                      ,(if (slide-next page)
                         `(a (@ (href ,(site-prefix/ site (page-url (slide-next page))))
                                (id "next")) "⟶")
                         `(span (@ (class "text-muted")) "⟶")))
                  ))
      ,@(map
          (lambda (path)
            `(script (@ (defer "true") (type "text/javascript")
                        (src ,(site-prefix/ site path)))))
          (page-js-footer page))))
  `(body (@ (lang "ru"))
         (article (@ (class "container-fluid"))
                  ,(post-process-sxml (page-content page) site page)
                  ,footer
                  (script (@ (defer "true") (type "text/javascript")
                             (src ,(site-prefix/ site "slides.js")))))))

;; export all symbols
(module-map
 (lambda (sym var)
   (force-output)
   (module-export! (current-module) (list sym)))
 (current-module))
