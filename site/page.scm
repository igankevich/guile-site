(define-module (site page)
  #:use-module (haunt html)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (site directory)
  #:use-module (site kernel)
  #:use-module (site site)
  #:use-module (site vtt)
  #:use-module (site video)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (web uri)
  )

;; TODO
(define %iso-date "%Y-%m-%dT%H:%M:%S%z")
(define %atom-feed-path "feed.atom")
(define %rss-feed-path "feed.rss")
(define %json-feed-path "feed.json")
(define %sitemap-path "sitemap.xml")
(define %russian-regex (make-regexp "[а-яА-Я]"))

(define %russian-non-breaking-space-regex
  (make-regexp "\\b([а-яА-Я]{1,3})\\s+([а-яА-Я0-9])"))
(define %russian-non-breaking-space-regex-pre
  (make-regexp "\\s+(—)"))
(define %russian-no-wrap
  (make-regexp "\\b(\\w+\\w+\\w+)\\b"))

(define (russian-text? str)
  (regexp-exec %russian-regex str))

(define (regexp-replace str regex . rest)
  (define match (regexp-exec regex str))
  (if match
    (apply regexp-replace
           `(,(apply regexp-substitute `(,#f ,match pre ,@rest post)) ,regex ,@rest))
    str))

(define (regexp-replace/sxml str regex sxml)
  (define match (regexp-exec regex str))
  (if match
    `(,(match:prefix match)
       ,(pre-post-order sxml
          `((*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
            (*text* . ,(lambda (_ txt)
                         (if (number? txt)
                           (match:substring match txt)
                           txt)))))
       ,@(let ((tmp (regexp-replace/sxml (match:suffix match) regex sxml)))
           (if (string? tmp)
             `(,tmp)
             tmp)))
    str))

(define (typeset-russian str)
  (set! str (regexp-replace str %russian-non-breaking-space-regex 1 "\u00A0" 2))
  (set! str (regexp-replace str %russian-non-breaking-space-regex-pre "\u00A0" 1))
  ;(set! str (regexp-replace/sxml str %russian-no-wrap '(span (@ (class "nowrap")) 1)))
  str)

(define (typeset-english str)
  (set! str (regexp-substitute/global #f "1st" str 'pre "1<sup>st</sup>" 'post))
  (set! str (regexp-substitute/global #f "2nd" str 'pre "2<sup>nd</sup>" 'post))
  (set! str (regexp-substitute/global #f "3rd" str 'pre "2<sup>rd</sup>" 'post))
  (set! str (regexp-substitute/global #f "([0-9]+)th" str 'pre 1 "<sup>th</sup>" 'post))
  str)

(define* (highlight-code kids #:key
                         (keywords '())
                         (keyword-characters "")
                         (macro-prefix "")
                         (comment-character #\null))
  (define code (string-append (string-join kids "") "\n"))
  (define lst '())
  (define word '())
  (define inside-string? #f)
  (define prev-inside-string? #f)
  (define string-delimiters '(#\" #\'))
  (define string-escape-character #\\)
  (define inside-comment? #f)
  (define prev-inside-comment? #f)
  (fold
    (lambda (ch prev)
      (set! prev-inside-string? inside-string?)
      (set! prev-inside-comment? inside-comment?)
      (if (char=? ch comment-character) (set! inside-comment? #t))
      (if (char=? ch #\newline) (set! inside-comment? #f))
      (if (and
            (not (char=? prev string-escape-character))
            (member ch string-delimiters))
        (set! inside-string? (not inside-string?)))
      (cond
        ;; entered commend
        ((and inside-comment? (not prev-inside-comment?))
         (if (not (null? word))
           (set! lst (cons word lst)))
         (set! word '())
         (set! word (cons ch word)))
        (inside-comment?
          (set! word (cons ch word)))
        ;; left comment
        ((and (not inside-comment?) prev-inside-comment?)
         (set! word (apply string (reverse (cons ch word))))
         (set! lst (cons* `(span (@ (class "syntax-comment")) ,word) lst))
         (set! word '()))
        ;; entered string
        ((and inside-string? (not prev-inside-string?))
         (if (not (null? word))
           (set! lst (cons word lst)))
         (set! word '())
         (set! word (cons ch word)))
        ;; left string
        ((and (not inside-string?) prev-inside-string?)
         (set! word (apply string (reverse (cons ch word))))
         (set! lst (cons* `(span (@ (class "syntax-string")) ,word) lst))
         (set! word '()))
        ;; accumulate string characters
        (inside-string?
          (set! word (cons ch word)))
        ;; accumulate keyword characters
        ((and (not (char-alphabetic? ch))
              (not (char-numeric? ch))
              (not (string-index keyword-characters ch)))
         (set! word (apply string (reverse word)))
         ;;(format (current-error-port) "word ~a\n" word)
         (cond
           ((member word keywords)
            (let ((class (if (and
                               (not (string-null? macro-prefix))
                               (string-prefix? macro-prefix word))
                           "syntax-macro"
                           "syntax-keyword")))
              (set! lst (cons* ch `(span (@ (class ,class)) ,word) lst))))
           (else
             (set! lst (cons* ch word lst))))
         (set! word '()))
        (else
          (set! word (cons ch word))))
      ch)
    #\space
    (string->list code))
  (reverse lst)
  )

(define (post-process-sxml sxml site page)
  (define paragraph-number 0)
  (define slide-number 0)
  (define (make-paragraph class tag . kids)
    (set! paragraph-number (+ 1 paragraph-number))
    (let ((name (format #f "~2,'0d" paragraph-number)))
      `((p (@ (class ,class))
           (span (@ (class "number-container"))
                 (a (@ (class "number") (href ,(string-append "#" name)) (id ,name))
                    ,name))
           ,@kids))))
  (define %svg-namespaces
    '((dc . "http://purl.org/dc/elements/1.1/")
      (cc . "http://creativecommons.org/ns#")
      (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      (svg . "http://www.w3.org/2000/svg")
      (sodipodi . "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd")
      (inkscape . "http://www.inkscape.org/namespaces/inkscape")))
  (define video-captions-rule
    (lambda (tag . kids)
      (define num-kids (length kids))
      (define %site (list-ref kids 0))
      (define number (list-ref kids 1))
      (define name (list-ref kids 2))
      (define captions-alist (list->captions (if (>= num-kids 4) (list-ref kids 3) '())))
      (define captions-alist? (not (null? captions-alist)))
      (define captions-url (format #f "videos/~a-~a-captions.vtt" number name))
      (define captions-path (format #f "src/~a" captions-url))
      (define captions? (file-exists? captions-path))
      (define video-url (format #f "videos/~a-~a.mp4" number name))
      (define video-path (format #f "src/~a" video-url))
      (define video? (file-exists? video-path))
      (define webm-url (format #f "videos/~a-~a.webm" number name))
      (define webm-path (format #f "src/~a" webm-url))
      (define webm? (file-exists? webm-path))
      (define video-id (format #f "id-~a-~a" number name))
      (define poster-url (site-prefix/ %site (format #f "videos/~a-~a.png" number name)))
      (define coming-soon-url (site-prefix/ %site "videos/coming-soon.png"))
      (if video?
        `((video (@ (controls "controls")
                    (poster ,poster-url)
                    (id ,video-id))
                 ,@(if webm?
                     `((source (@ (src ,(site-prefix/ %site webm-url)) (type "video/webm"))))
                     '())
                 (source (@ (src ,(site-prefix/ %site video-url)) (type "video/mp4")))
                 ,@(if captions?
                     `((track (@ (src ,(site-prefix/ %site captions-url))
                                 (kind "captions")
                                 (label "Таймкоды")
                                 (srclang "ru"))))
                     '()))
          ,@(cond
              (captions?
                `(,(vtt->html (file->vtt captions-path) video-id)))
              (captions-alist?
                `(,(captions->html captions-alist video-id))))
          (else
            '()))
        `(img (@ (class "image") (alt "Coming soon") (src ,coming-soon-url))))))
  (define (to-valid-html-id s)
    (string-map
      (lambda (ch)
        (cond
          ((char-whitespace? ch) #\-)
          (else ch)))
      s))
  (define (sxml->text sxml)
    (define text "")
    (pre-post-order sxml
      `((*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
        (*text* . ,(lambda (_ s)
                     (set! text (string-append text s))))))
    text)
  (define (process-sxml sxml)
    (pre-post-order sxml
      `((comment . ,(lambda (tag . kids) `(span (@ (class "syntax-comment")) ,@kids)))
        (keyword . ,(lambda (tag . kids) `(span (@ (class "syntax-keyword")) ,@kids)))
        (string . ,(lambda (tag . kids) `(span (@ (class "syntax-string")) ,@kids)))
        (slide . ,(lambda (tag . kids)
                    (set! slide-number (+ 1 slide-number))
                    `(div (@ (class "slide") (id ,(format #f "slide-~a" slide-number)))
                          (div (@ (class "slide-box"))
                               ,@kids))))
        (scala . ,(lambda (tag . kids)
                    (define keywords
                      '("public" "private" "protected"
                        "val" "var" "lazy" "def" "override"
                        "throw" "throws" "try" "catch" "finally"
                        "for" "do" "while"
                        "this" "super" "new" "true" "false"
                        "class" "extends" "implements" "abstract"
                        "package" "import"))
                    (define keyword-characters "_$")
                    (highlight-code kids
                                    #:keywords keywords
                                    #:keyword-characters keyword-characters)
                    ))
        (java . ,(lambda (tag . kids)
                   (define keywords
                     '("public" "private" "protected"
                       "void" "boolean" "char" "short" "int" "long" "float" "double"
                       "throw" "throws" "try" "catch" "finally"
                       "for" "do" "while" "return"
                       "this" "super" "new" "true" "false"
                       "class" "extends" "implements" "abstract"
                       "package" "import"))
                   (define keyword-characters "_$")
                   (highlight-code kids
                                   #:keywords keywords
                                   #:keyword-characters keyword-characters)
                   ))
        (cpp . ,(lambda (tag . kids)
                  (define keywords
                    '("public" "private" "protected"
                      "void" "bool" "char" "short" "int" "long" "float" "double"
                      "unsigned" "signed" "const" "noexcept" "auto"
                      "uint8_t" "uint16_t" "uint32_t" "uint64_t"
                      "int8_t" "int16_t" "int32_t" "int64_t"
                      "size_t" "ptrdiff_t"
                      "reinterpret_cast" "static_cast" "dynamic_cast" "const_cast"
                      "throw" "throws" "try" "catch"
                      "for" "do" "while" "return" "break" "continue" "if" "else"
                      "this" "new" "delete" "true" "false" "nullptr"
                      "class" "virtual" "struct" "inline" "template"
                      "sizeof" "alignof" "typename" "default" "operator"
                      "#include" "#define" "#if" "#else" "#elif" "#endif" "#pragma"
                      "#error"))
                  (define keyword-characters "_$#")
                  (highlight-code kids
                                  #:keywords keywords
                                  #:keyword-characters keyword-characters
                                  #:macro-prefix "#")))
        (sh . ,(lambda (tag . kids)
                 (define keywords
                   '("export" "if" "then" "fi" "elif" "else" "case" "esac" "in" "set"
                     "break" "for" "do" "done" "while" "shift"
                     "cd" "echo" "exit"
                     "source" "alias" "return"))
                 (define keyword-characters "_")
                 (highlight-code kids
                                 #:keywords keywords
                                 #:keyword-characters keyword-characters
                                 #:comment-character #\#)
                 ))
        (meson . ,(lambda (tag . kids)
                    (define keywords
                      '("if" "else" "endif" "foreach" "endforeach"
                        "project" "meson" "subdir" "executable"
                        "files" "import" "dependency"))
                    (define keyword-characters "_")
                    (highlight-code kids
                                    #:keywords keywords
                                    #:keyword-characters keyword-characters
                                    #:comment-character #\#)))
        (pc . ,(lambda (tag . kids)
                 (define keywords
                   '("Name:" "Description:" "Version:" "Libs:" "Cflags:"))
                 (define keyword-characters "_:")
                 (highlight-code kids
                                 #:keywords keywords
                                 #:keyword-characters keyword-characters
                                 #:comment-character #\#)))
        (asm . ,(lambda (tag . kids)
                  (define keywords
                    '("movq" "syscall" "ret" ".global" ".type" ".hidden"))
                  (define keyword-characters "_.")
                  (highlight-code kids
                                  #:keywords keywords
                                  #:keyword-characters keyword-characters
                                  #:comment-character #\#)))
        (pygmentize . ,(lambda (tag . kids)
                         (define lexer (list-ref kids 0))
                         (define code (string-trim-both (string-join (cdr kids) "")))
                         (define tmp (mkstemp! (string-copy "/dev/shm/pygmentize-XXXXXX")))
                         (display code tmp)
                         (force-output tmp)
                         (define tmp-filename (port-filename tmp))
                         (define port (open-pipe* OPEN_BOTH "pygmentize"
                                                  "-f" "html"
                                                  "-l" lexer
                                                  "-O" "nowrap,classprefix=pygmentize-"
                                                  tmp-filename))
                         (close-port tmp)
                         (define output (get-string-all port))
                         (define exit-code (status:exit-val (close-pipe port)))
                         (delete-file tmp-filename)
                         (if (= exit-code 0)
                           (list-ref (xml->sxml
                                       (format #f "<pre class=\"~a\">~a</pre>" lexer output)) 1)
                           '())))
        (man-ru . ,(lambda (tag . kids)
                     (let* ((section (list-ref kids 0))
                            (name (list-ref kids 1))
                            (url (format #f "https://mirror.cmmshq.ru/spc/man/man~a/~a.~a.html"
                                         section name section)))
                       `(a (@ (href ,url)) (code ,name)))))
        (image . ,(lambda (tag . kids)
                    (let* ((path (list-ref kids 0))
                           (text (if (>= (length kids) 2) (list-ref kids 1) #f))
                           (url (if (>= (length kids) 3) (list-ref kids 2) #f))
                           (webp (if (string-suffix? ".png" path)
                                   `((source (@ (srcset ,(site-prefix/ site (replace-extension path "webp")))
                                                (type "image/webp"))))
                                   '())))
                      (page-add-input-files page `(,(string-append "src/" path)))
                      (if (not text)
                        (set! text (site-prefix/ site path)))
                      (define picture
                        (if url
                          `(a (@ (href ,(site-prefix/ site url)))
                              (picture
                                ,@webp
                                (img (@ (src ,(site-prefix/ site path))
                                        (alt ,text)))))
                          `(picture
                             ,@webp
                             (img (@ (src ,(site-prefix/ site path))
                                     (alt ,text))))))
                      (if (not text)
                        picture
                        `(figure (@ (class "center"))
                                 ,picture
                                 (figcaption ,text))))))
        (nbsp . ,(lambda (tag . kids) "\u00a0"))
        (emdash . ,(lambda (tag . kids) "\u00a0—"))
        (math . ,(lambda (tag . kids)
                   `(span (@ (class "math")) ,@kids)))
        (display-math . ,(lambda (tag . kids)
                           `(span (@ (class "display-math")) ,@kids)))
        (href . ,(lambda (tag . kids)
                   (if (not (null? kids))
                     (let ((uri (string->uri (car kids))))
                       (if (string-contains (if uri (uri-path uri) (car kids)) "//")
                         (format (current-error-port) "Warning, double slash in URL: ~a\n" (uri-path uri)))))
                   (cons tag kids)))
        (video-figure *preorder* . ,(lambda (tag . kids)
                                      (let ((path (list-ref kids 0))
                                            (text (list-ref kids 1)))
                                        `(figure (@ (class "text-center"))
                                                 (video (@ (class "img-fluid rounded") (controls "") (loop "loop"))
                                                        (source (@ (src ,path) (type "video/ogg"))))
                                                 (figcaption (@ (class "text-muted")) ,text)))))
        (video/captions . ,video-captions-rule)
        (video-captions . ,video-captions-rule)
        (video-object . ,(lambda (tag . kids)
                           (define site (list-ref kids 0))
                           (define video (list-ref kids 1))
                           ((video-sxml video) site video)))
        (toc-entry . ,(lambda (tag . kids)
                        (define id (to-valid-html-id (sxml->text kids)))
                        `((a (@ (id ,id))) ,@kids)))
        (blackboard . ,(lambda (tag . kids)
                         (define %site (list-ref kids 0))
                         (define number (list-ref kids 1))
                         (define name (list-ref kids 2))
                         (define thumbnail-name (format #f "~a-~a.png" number name))
                         (define thumbnail-path (site-prefix/ %site "xopp" thumbnail-name))
                         (define pdf-name (format #f "~a-~a.pdf" number name))
                         (define path (site-output-path %site (string-append "src/xopp/" thumbnail-name)))
                         ;(format #t "thumbnail-path ~a\n" path)
                         `((a (@ (href ,(site-prefix/ %site "xopp" pdf-name))) 
                              (img (@ (src ,thumbnail-path)
                                      (alt ,(format #f "~a thumbnail." name))
                                      (class "xopp-thumbnail")))))))
        (paragraph . ,(lambda (tag . kids)
                        (apply make-paragraph `("" tag ,@kids))))
        (continue . ,(lambda (tag . kids)
                       `(p (@ (class "no-indent")) ,@kids)))
        (first-paragraph . ,(lambda (tag . kids)
                              (apply make-paragraph `("no-indent" tag ,@kids))))
        (listing . ,(lambda (tag . kids)
                      (apply make-paragraph `("no-indent" tag (pre (code ,@kids))))))
        (paragraph-math . ,(lambda (tag . kids)
                             (apply make-paragraph `("no-indent" tag (span (@ (class "math")) ,@kids)))))
        (section-number . ,(lambda (tag . kids)
                             (define n (page-number page))
                             `(span (@ (class ,(format #f "section-~a" (if (< n 10) 4 5))))
                                    ,(format #f "§\u00a0~a.\u00a0" n))))
        (page-title . ,(lambda (tag . kids) (page-title page)))
        (paragraph-link . ,(lambda (tag . kids)
                             (define n (car kids))
                             `(a (@ (class "paragraph-link")
                                    (href ,(format #f "#~a" n)))
                                 (span (@ (class "no-underline")) "¶")
                                 (span (@ (class "underline")) ,kids))))
        (points . ,(lambda (tag . kids)
                     (define n (first kids))
                     (define m
                       (if (>= (length kids) 2)
                         (second kids)
                         n))
                     (define text
                       (cond
                         ((= (remainder n 10) 1) "балл")
                         ((<= (remainder n 10) 4) "балла")
                         (else "баллов")))
                     `(span (@ (class ,(format #f "points points-~a" m)))
                            ,(format #f "~a ~a" n text))))
        (tasks . ,(lambda (tag . kids)
                    `(h1 (@ (class "tasks") (id "tasks")) ,kids)))
        (task . ,(lambda (tag . kids)
                   `(h2 (span (@ (class "task-number"))) ,kids)))
        (inline-svg . ,(lambda (tag . kids)
                         (define num-kids (length kids))
                         (define site (if (= num-kids 2) (list-ref kids 0) #f))
                         (define path (list-ref kids (if (= num-kids 2) 1 0)))
                         (page-add-input-files page `(,path))
                         `(,(call-with-input-file
                              (if site
                                (site-output-directory site path)
                                path)
                              (lambda (port)
                                (pre-post-order
                                  (cddr (xml->sxml port
                                                   #:trim-whitespace? #t
                                                   #:declare-namespaces? #f
                                                   #:namespaces %svg-namespaces))
                                  `((*default* . ,(lambda (tag . kids)
                                                    (define s (symbol->string tag))
                                                    (if (string-prefix? "svg:" s)
                                                      `(,(string->symbol (substring s 4 (string-length s))) ,@kids)
                                                      `(,tag ,@kids))))
                                    (*text* . ,(lambda (_ txt) txt)))
                                  ))))))
        (link . ,(lambda (tag . kids)
                   (define stylesheet? #f)
                   ;(format #t "link ~a\n" kids)
                   (pre-post-order kids
                     `((rel . ,(lambda (tag . kids)
                                 (if (string=? (car kids) "stylesheet")
                                   (set! stylesheet? #t))
                                 `(,tag ,@kids)))
                       (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                       (*text* . ,(lambda (_ txt) txt))))
                   (if stylesheet?
                     (cons tag
                           (pre-post-order kids
                             `((rel . ,(lambda (tag . kids)
                                         (if (string=? (car kids) "stylesheet")
                                           (set! stylesheet? #t))
                                         `(,tag ,@kids)))
                               (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                               (*text* . ,(lambda (_ txt) txt)))))
                     (cons tag kids))))
        (table-of-contents . ,(lambda (tag . kids)
                                (define toc '())
                                (define (get-text kids)
                                  (define kids-2
                                    (cond
                                      ((null? kids) '())
                                      ((null? (car kids)) kids)
                                      ((not (list? (car kids))) kids)
                                      ((eq? (caar kids) '@) (cdr kids))
                                      (else kids)))
                                  (process-sxml kids-2))
                                (define (get-id kids-2)
                                  (define kids (cdr (process-sxml kids-2)))
                                  (define id
                                    (cond
                                      ((null? kids) '())
                                      ((null? (car kids)) '())
                                      ((not (list? (car kids))) '())
                                      ((eq? (caar kids) '@)
                                       (filter (lambda (pair) (eq? (car pair) 'id)) (cdr (car kids))))
                                      (else '())))
                                  (if (null? id)
                                    id
                                    (car (cdr (car id)))))
                                (pre-post-order sxml
                                  `((toc-entry . ,(lambda (tag . kids)
                                                    (define id (to-valid-html-id (sxml->text kids)))
                                                    (set! toc (append toc `((0 ,id ,(sxml->text kids)))))
                                                    ))
                                    (points . ,(lambda (tag . kids) '()))
                                    (section-number . ,(lambda (tag . kids) '()))
                                    ;(page-title . ,(lambda (tag . kids) (page-title page)))
                                    (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
                                    (*text* . ,(lambda (_ txt) txt))))
                                `((div (@ (class "toc"))
                                       (h1 (@ (class "toc-title")) ,@kids)
                                       (ul (@ (class "toc-list"))
                                           ,(map
                                              (lambda (entry)
                                                (define level (list-ref entry 0))
                                                (define id (list-ref entry 1))
                                                (define text (list-ref entry 2))
                                                `(li ;,(make-string level (integer->char #x00a0))
                                                     ,(if (not (null? id))
                                                        `(a (@ (href ,(format #f "#~a" id))) ,text)
                                                        text)))
                                              toc))))))
        (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
        (*text* . ,(lambda (_ txt)
                     (cond
                       ((and (string? txt) (russian-text? txt)) (typeset-russian txt))
                       (else txt)))))))
  (process-sxml sxml))

(define (page-head-common/default site page)
  (post-process-sxml 
    `((meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
      (meta (@ (http-equiv "X-UA-Compatible") (content "IE=edge")))
      (meta (@ (name "description") (content ,(page-abstract page))))
      ;; PURL
      (meta (@ (property "http://purl.org/dc/terms/title") (content ,(page-title page))))
      (meta (@ (property "http://purl.org/dc/terms/creator") (content ,(page-author page))))
      (meta (@ (property "http://purl.org/dc/terms/date")
               (content ,(strftime %iso-date (page-date page)))))
      (meta (@ (property "http://purl.org/dc/terms/license")
               (content "Creative Commons Attribution-ShareAlike 4.0 International License")))
      (meta (@ (property "http://purl.org/dc/terms/language") (content "ru")))
      ;; CC
      (meta (@ (property "http://creativecommons.org/ns#attributionName")
               (content ,(page-author page))))
      (meta (@ (property "http://creativecommons.org/ns#attributionURL")
               (content ,(page-url page))))
      (meta (@ (property "http://creativecommons.org/ns#license")
               (content "Creative Commons Attribution-ShareAlike 4.0 International License")))
      ;; Twitter
      ;(meta (@ (property "twitter:title") (content ,(page-title page))))
      ;(meta (@ (property "twitter:description") (content ,(page-abstract page))))
      ;(meta (@ (property "twitter:site") (content "@igankevich")))
      ;(meta (@ (property "twitter:creator") (content "@igankevich")))
      ;; Open Graph
      (meta (@ (property "og:title") (content ,(page-title page))))
      (meta (@ (property "og:description") (content ,(page-abstract page))))
      (meta (@ (property "og:url") (content ,(page-full-url site page))))
      (meta (@ (property "og:site_name") (content ,(site-description site))))
      ;; Image/video
      ,@(let ((image (page-image page)))
          (if image
            `(;(meta (@ (property "twitter:image") (content ,image)))
              (meta (@ (property "og:image") (content ,image))))
            '()))
      ,@(let ((video (page-video page)))
          (if video
            `(;(meta (@ (property "twitter:player:stream") (content ,video)))
              (meta (@ (property "og:type") (content "video.other")))
              (meta (@ (property "og:video") (content ,video)))
              (meta (@ (property "og:video:url") (content ,video)))
              (meta (@ (property "og:video:secure_url") (content ,video)))
              )
            '()))
      ;; Custom meta tags
      ,@(map
          (lambda (pair)
            `(meta (@ (property ,(car pair)) (content ,(cdr pair)))))
          (page-meta page))
      ;; Title
      (title ,(page-title page))
      ;; Favicons
      ,@(map
          (lambda (size)
            (define rel
              (cond
                ((member size '("167x167" "180x180")) "apple-touch-icon")
                (else "icon")))
            `(link (@ (rel ,rel)
                      (type "image/png")
                      (sizes ,size)
                      (href ,(site-prefix/ site "favicon" (string-append size ".png"))))))
          (@@ (site kernels) %favicon-sizes))
      (link (@ (rel "icon") (sizes "any") (type "image/svg+xml")
               (href ,(site-prefix/ site "favicon/any.svg"))))
      (link (@ (rel "icon") (type "image/x-icon")
               (href ,(site-prefix/ site "favicon.ico"))))
      ;; Misc
      (link (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/")))
      (link (@ (rel "canonical") (href ,(site-url site))))
      ;; Feeds
      (link (@ (href ,(string-append "/" %atom-feed-path)) (type "application/atom+xml")
               (rel "alternate") (title ,(site-description site))))
      (link (@ (href ,(string-append "/" %rss-feed-path)) (type "application/rss+xml")
               (rel "alternate") (title ,(site-description site))))
      (link (@ (href ,(string-append "/" %json-feed-path)) (type "application/json")
               (rel "alternate") (title ,(site-description site))))
      ;; preload fonts
      ,@(if (page-preload-fonts? page)
          (map
            (lambda (path)
              `(link (@ (rel "preload")
                        (as "font")
                        (crossorigin "anonymous")
                        (href ,(site-prefix/ site path)))))
            (page-fonts page))
          '())
      ;; CSS
      ,@(begin
          (for-each kernel-run (page-css page))
          (append-map
            (lambda (kernel)
              (map (lambda (path)
                     `(link (@ (rel "stylesheet")
                               (href ,(site-prefix// site path)))))
                   (kernel-output-files kernel)))
            (page-css page)))
      ;; JS
      ,@(begin
          (map
            (lambda (x)
              (cond
                ((is-a? x <kernel>)
                 (kernel-run x)
                 (let ((path (car (kernel-output-files x))))
                   `(script (@ (src ,(site-prefix// site path))))))
                (else
                  `(script (@ (src ,(site-prefix// site x)))))))
            (page-js-head page))))
    site
    page))

(define* (page-copyright/default page)
  (define (years start end)
    (if (string= start end) start (string-append start "–" end)))
  (define page-year (strftime "%Y" (page-date page)))
  (define current-year (strftime "%Y" (localtime (current-time))))
  (format #f "© ~a ~a" (years page-year current-year) (page-author page)))

(define (page-tail/default site page)
  (define header
    (let ((logo-path (site-logo site)))
      `(nav 
         (ul
           (li (@ (class "nav-item"))
               (a (@ (href ,(site-prefix/ site)))
                  (picture
                    ,(if (string-suffix? ".png" logo-path)
                       `((source (@ (srcset ,(site-prefix/ site (replace-extension logo-path "webp")))
                                    (type "image/webp"))))
                       '())
                    (img (@ (src ,(site-prefix/ site logo-path))
                            (alt ,(site-description site)))))))
           ,@(map
               (lambda (directory)
                 `(li (@ (class "nav-item"))
                      (a (@ (class "nav-link")
                            (href ,(site-prefix/ site (directory-url directory))))
                         ,(directory-name directory))))
               (site-directories site))))))
  (define footer
    `((footer
        (div (@ (class "center"))
             (small
               (a (@ (rel "license")
                     (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                  "CC-BY-SA 4.0 ")
               ,((page-copyright page) page))))
      ,@(begin
          (map
            (lambda (x)
              (cond
                ((is-a? x <kernel>)
                 (kernel-run x)
                 (let ((path (car (kernel-output-files x))))
                   `(script (@ (defer "") (src ,(site-prefix// site path))))))
                (else
                  `(script (@ (defer "") (src ,(site-prefix// site x))))
                  )))
            (page-js-footer page)))))
  `(body (@ (lang "ru"))
         ,header
         (article ,(post-process-sxml (page-content page) site page))
         ,@footer))

(define-class <page> ()
  (input-files #:init-keyword #:input-files #:accessor page-input-files #:init-value '())
  (name #:init-keyword #:name #:accessor page-name)
  (date #:init-keyword #:date #:accessor page-date)
  (url #:init-keyword #:url #:accessor page-url #:init-value #f)
  (image #:init-keyword #:image #:accessor page-image #:init-value #f)
  (video #:init-keyword #:video #:accessor page-video #:init-value #f)
  (meta #:init-keyword #:meta #:accessor page-meta #:init-value '())
  (title #:init-keyword #:title #:accessor page-title)
  (author #:init-keyword #:author #:accessor page-author)
  (abstract #:init-keyword #:abstract #:accessor page-abstract #:init-value "")
  (content #:init-keyword #:content #:accessor page-content)
  (keywords #:init-keyword #:keywords #:accessor page-keywords #:init-value '())
  (uuid #:init-keyword #:uuid #:accessor page-uuid)
  (foreign? #:init-keyword #:foreign? #:accessor page-foreign? #:init-value #f)
  (hidden? #:init-keyword #:hidden? #:accessor page-hidden? #:init-value #f)
  (index? #:init-keyword #:index? #:accessor page-index? #:init-value #t)
  (number #:init-keyword #:number #:accessor page-number #:init-value 0)
  (parent #:init-keyword #:parent #:accessor page-parent #:init-value #f)
  (head-common #:init-keyword #:head-common #:accessor page-head-common
               #:init-value page-head-common/default)
  (head #:init-keyword #:head #:accessor page-head
        #:init-value (lambda (site page) ((page-head-common page) site page)))
  (tail #:init-keyword #:tail #:accessor page-tail #:init-value page-tail/default)
  (copyright #:init-keyword #:copyright #:accessor page-copyright
             #:init-value page-copyright/default)
  (css #:init-keyword #:css #:accessor page-css #:init-value '())
  (js-head #:init-keyword #:js-head #:accessor page-js-head #:init-value '())
  (js-footer #:init-keyword #:js-footer #:accessor page-js-footer #:init-value '())
  (fonts #:init-keyword #:fonts #:accessor page-fonts #:init-value '())
  (preload-fonts? #:init-keyword #:preload-fonts? #:accessor page-preload-fonts? #:init-value #f)
  (fields #:init-keyword #:fields #:accessor page-fields #:init-value '()))

(define (page-set-field! ref name value)
  (set! (page-fields ref) (assoc-set! (page-fields ref) name value)))

(define (page-remove-field! ref name)
  (set! (page-fields ref) (assoc-remove! (page-fields ref) name)))

(define (page-field-ref ref name)
  (assoc-ref (page-fields ref) name))

(define (page-full-url site page)
  (string-append (site-url site) "/" (page-url page)))

(define (mkdir-p dir)
  (define d "")
  (for-each
    (lambda (component)
      (set! d (string-append d component "/"))
      (if (not (file-exists? d)) (mkdir d)))
    (string-split dir #\/)))

(define (page-sxml site page)
  (list ((page-head page) site page) ((page-tail page) site page)))

(define (page-write site page path)
  (page-write-sxml (page-sxml site page) site page path))

(define (page-write-sxml sxml site page path)
  (mkdir-p (dirname path))
  ;(format (current-error-port) "write ~a\n" path)
  (call-with-output-file path
    (lambda (port)
      (display (sxml->html-string `((doctype html) (html (@ (lang "ru")) ,sxml))) port))))

(define (page-add-input-files page files)
  (set! (page-input-files page) (append (page-input-files page) files)))

(define (page-input-file page)
  (car (page-input-files page)))

(define (page-load input-file)
  (define name (basename input-file))
  (define extension ".scm")
  (define subdir (get-output-subdirectory input-file))
  ;(format #t "load ~a\n" input-file)
  (define data (primitive-load input-file))
  (define pages (if (list? data) data (list data)))
  (for-each
    (lambda (page)
      (let ((short-name (substring name 0 (- (string-length name)
                                             (string-length extension)))))
        (if (not (page-url page))
          (set! (page-url page) (string-append subdir "/" short-name "/")))
        (set! (page-name page) short-name)
        (page-add-input-files page `(,input-file))))
    pages)
  pages)

(define (all-pages subdir)
  (define dir
    (if (string-prefix? "src/" subdir)
      subdir
      (string-append "src/" subdir)))
  (define extension ".scm")
  (if (file-exists? dir)
    (sort
      (append-map
        (lambda (name) (page-load (string-append dir "/" name)))
        (scandir dir (lambda (name)
                       (and (string-suffix? extension name)
                            (not (string=? name "index.scm"))))))
      (lambda (a b)
        (< (page-number a) (page-number b))))
    '()))

(define (get-mime-type path)
  (define port (open-pipe* OPEN_READ "file" "--mime-type" "--brief" "--dereference" "-E" path))
  (define output (get-string-all port))
  (define exit-code (status:exit-val (close-pipe port)))
  (if (= exit-code 0) (string-trim-both output) ""))

;; export all symbols
(module-map
 (lambda (sym var)
   (force-output)
   (module-export! (current-module) (list sym)))
 (current-module))
