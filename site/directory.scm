(define-module (site directory)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format))

(define-class <directory> ()
  (url #:init-keyword #:url #:accessor directory-url)
  (name #:init-keyword #:name #:accessor directory-name)
  ;; file system path relative to "src" directory
  (path #:init-keyword #:path #:accessor directory-path)
  (number #:init-keyword #:number #:accessor directory-number))

;; export all symbols
(module-map
 (lambda (sym var)
   (module-export! (current-module) (list sym)))
 (current-module))
