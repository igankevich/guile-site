(define-module (site bibliography)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

(define %iso-date "%Y-%m-%dT%H:%M:%S%z")

(define-class <reference> ()
  (name #:init-keyword #:name #:accessor reference-name #:init-value #f)
  (title #:init-keyword #:title #:accessor reference-title #:init-value #f)
  (authors #:init-keyword #:authors #:accessor reference-authors #:init-value '())
  (date #:init-keyword #:date #:accessor reference-date #:init-value #f)
  (publisher #:init-keyword #:publisher #:accessor reference-publisher #:init-value #f)
  (publication #:init-keyword #:publication #:accessor reference-publication #:init-value #f)
  (url #:init-keyword #:url #:accessor reference-url #:init-value #f)
  (abstract #:init-keyword #:abstract #:accessor reference-abstract #:init-value #f)
  (fields #:init-keyword #:fields #:accessor reference-fields #:init-value '())
  (open-access? #:init-keyword #:open-access? #:accessor reference-open-access? #:init-value #f))

(define (string->date format str)
  (let ((tm (car (strptime format str))))
    (if (= (tm:mday tm) 0) (set-tm:mday tm 1))
    (if (not (tm:zone tm)) (set-tm:zone tm (tm:zone (localtime (current-time)))))
    (cdr (mktime tm))))

(define (date->string format str)
  (strftime format str))

(define (reference-set-field! ref name value)
  (set! (reference-fields ref) (assoc-set! (reference-fields ref) name value)))

(define (reference-remove-field! ref name)
  (set! (reference-fields ref) (assoc-remove! (reference-fields ref) name)))

(define (reference-field-ref ref name)
  (assoc-ref (reference-fields ref) name))

(define (reference-bibtex-authors ref)
  (string-join
    (map
      (lambda (lst) (string-join lst " "))
      (reference-authors ref))
    " and "))

(define (reference-bibtex-id ref)
  (define (non-null s) (if s s ""))
  (let ((id (assoc-ref (reference-fields ref) "bibtex.id")))
    (if id id
      (format #f "~x" (hash (string-join
                              (map non-null
                                   `(,(reference-title ref)
                                      ,(reference-publisher ref)
                                      ,(reference-publication ref)
                                      ,(reference-url ref)
                                      ,(reference-bibtex-authors ref)
                                      ,(format #f "~a" (reference-date ref))))
                              "") 18446744073709551615)))))

(define (reference->sxml ref)
  (define (make-field name value)
    (if (and value (not (and (string? value) (string-null? value))))
      `((,(string->symbol name) ,value))
      '()))
  `(reference
     ,@(make-field "title" (reference-title ref))
     (authors ,(reference-authors ref))
     ,@(if (reference-date ref)
         `((date ,(date->string %iso-date (reference-date ref))))
         '())
     ,@(make-field "publisher" (reference-publisher ref))
     ,@(make-field "publication" (reference-publication ref))
     ,@(make-field "url" (reference-url ref))
     ,@(make-field "abstract" (reference-abstract ref))
     ,@(make-field "open-access" (reference-open-access? ref))
     (fields
       ,@(map (lambda (field)
                `(,(string->symbol (car field))
                   ,(cdr field)))
              (reference-fields ref))
       )
     ))

(define (sxml->references sxml)
  (define ref #f)
  (define references '())
  (define fields-rules
    `((*default* . ,(lambda (tag . kids)
                      (reference-set-field! ref (symbol->string tag) (car kids))))
      (*text* . ,(lambda (_ txt) txt))))
  (define reference-rules
    `((name . ,(lambda (tag . kids) (set! (reference-name ref) (car kids))))
      (title . ,(lambda (tag . kids) (set! (reference-title ref) (car kids))))
      (authors . ,(lambda (tag . kids) (set! (reference-authors ref) (car kids))))
      (date . ,(lambda (tag . kids) (set! (reference-date ref) (string->date %iso-date (car kids)))))
      (publisher . ,(lambda (tag . kids) (set! (reference-publisher ref) (car kids))))
      (publication . ,(lambda (tag . kids) (set! (reference-publication ref) (car kids))))
      (url . ,(lambda (tag . kids) (set! (reference-url ref) (car kids))))
      (abstract . ,(lambda (tag . kids) (set! (reference-abstract ref) (car kids))))
      (open-access . ,(lambda (tag . kids) (set! (reference-open-access? ref) (car kids))))
      (fields *preorder* .
              ,(lambda (tag . kids)
                 (pre-post-order kids fields-rules)))
      (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
      (*text* . ,(lambda (_ txt) txt))))
  (pre-post-order sxml
    `((reference *preorder* . ,(lambda (tag . kids)
                                 (set! ref (make <reference>))
                                 (pre-post-order kids reference-rules)
                                 (set! references (cons ref references))))
      (*default* . ,(lambda (tag . kids) `(,tag ,@kids)))
      (*text* . ,(lambda (_ txt) txt))))
  (reverse references))

(define (reference->bibtex ref)
  (define (make-field name value)
    (if (and value (not (and (string? value) (string-null? value))))
      `(,(cons name value))
      '()))
  (define (reference-year ref)
    (define date (reference-date ref))
    (if date (strftime "%Y" date) #f))
  (define (reference-month ref)
    (define date (reference-date ref))
    (if date (strftime "%m" date) #f))
  (define (encode-latex s)
    (list->string
      (fold-right
        (lambda (ch prev)
          (cond
            ((char=? ch #\{) (cons* #\\ #\{ prev))
            ((char=? ch #\}) (cons* #\\ #\} prev))
            ((char=? ch #\") (cons* #\\ #\" prev))
            ((char=? ch #\$) (cons* #\\ #\$ prev))
            ((char=? ch #\%) (cons* #\\ #\% prev))
            ((char=? ch #\&) (cons* #\\ #\& prev))
            ((char=? ch #\“) (cons* #\` #\` prev))
            ((char=? ch #\”) (cons* #\' #\' prev))
            ((char=? ch #\–) (cons* #\- #\- prev))
            ((char=? ch #\—) (cons* #\- #\- #\- prev))
            (else (cons ch prev))))
        '()
        (string->list s))))
  (define bibtex-type
    (let ((type (assoc-ref (reference-fields ref) "bibtex.type")))
      (if type (string-downcase type) "article")))
  (define publication
    (cond
      ((member bibtex-type '("inproceedings" "conference")) "booktitle")
      ((string=? bibtex-type "article") "journal")
      ((string=? bibtex-type "book") "howpublished")
      ((string=? bibtex-type "booklet") "howpublished")
      ((string=? bibtex-type "inbook") "howpublished")
      ((string=? bibtex-type "incollection") "booktitle")
      ((string=? bibtex-type "manual") "howpublished")
      ((string=? bibtex-type "masterthesis") "school")
      ((string=? bibtex-type "misc") "howpublished")
      ((string=? bibtex-type "phdthesis") "school")
      ((string=? bibtex-type "proceedings") "howpublished")
      ((string=? bibtex-type "techreport") "institution")
      ((string=? bibtex-type "unpublished") "howpublished")
      (else "howpublished")))
  (define bibtex-id (reference-bibtex-id ref))
  (call-with-output-string
    (lambda (port)
      (format port "@~a{~a" bibtex-type bibtex-id)
      (define body
        (string-join
          (map
            (lambda (field)
              (format #f "  ~a={~a}"
                      (car field) (string-trim-both (encode-latex (cdr field)))))
            `(,@(make-field "title" (reference-title ref))
               ,@(make-field "author" (reference-bibtex-authors ref))
               ,@(make-field "publisher" (reference-publisher ref))
               ,@(make-field publication (reference-publication ref))
               ,@(make-field "url" (reference-url ref))
               ,@(make-field "year" (reference-year ref))
               ,@(make-field "month" (reference-month ref))
               ,@(fold
                   (lambda (field prev)
                     (define name (car field))
                     (define value (cdr field))
                     (cond
                       ((not (string-prefix? "bibtex." name)) prev)
                       ((string=? name "bibtex.id") prev)
                       ((string-prefix? "bibtex." name) (cons (cons (substring name 7) value) prev))
                       (else (cons field prev))))
                   '()
                   (reference-fields ref))))
          ",\n"))
      (if (not (string-null? body))
        (begin
          (display ",\n" port)
          (display body port)
          (display "\n}\n" port))
        (display "}\n" port)))))

(define (bibtex->references str)
  (if (input-port? str)
    (set! str (get-string-all str)))
  (define (reference-set-bibtex! ref name value)
    (cond
      ((string=? name "title")
       (set! (reference-title ref) value))
      ((string=? name "author")
       (let ((tmp (regexp-substitute/global #f "\\band\\b" value 'pre "|" 'post)))
         (set! (reference-authors ref)
           (map
             (lambda (author-str)
               (if (string-any #\, author-str)
                 (set! author-str
                   (string-join
                     (map string-trim-both
                          (reverse (string-split author-str #\,))) " ")))
               (map string-trim-both (string-split author-str #\space)))
             (map string-trim-both (string-split tmp #\|))))))
      ((string=? name "publisher")
       (set! (reference-publisher ref) value))
      ((member name '("booktitle" "journal"))
       (set! (reference-publication ref) value))
      ((string=? name "url")
       (set! (reference-url ref) value))
      (else
        (reference-set-field! ref (string-append "bibtex." name) value))))
  (define (decode-latex value)
    (fold
      (lambda (pair prev)
        (regexp-substitute/global #f (car pair) prev 'pre (cdr pair) 'post))
      value
      '(("``" . "“")
        ("''" . "”")
        ("--" . "–")
        ("---" . "—")
        ("\\\\&" . "&")
        ("\\\\[a-zA-Z]+\\{([^}]+)\\}" . 1)
        ("\\{" . "")
        ("\\}" . "")
        ("\\s+" . " "))))
  (define (bibtex-month->number month)
    (define m (string-downcase (string-trim-both month)))
    (if (>= (string-length m) 3)
      (set! m (substring m 0 3)))
    (cond
      ((string=? "jan" m) 1)
      ((string=? "feb" m) 2)
      ((string=? "mar" m) 3)
      ((string=? "apr" m) 4)
      ((string=? "may" m) 5)
      ((string=? "jun" m) 6)
      ((string=? "jul" m) 7)
      ((string=? "aug" m) 8)
      ((string=? "sep" m) 9)
      ((string=? "oct" m) 10)
      ((string=? "nov" m) 11)
      ((string=? "dec" m) 12)
      (else (string->number month))))
  (define rx-@ (make-regexp "^\\s*@"))
  (define rx-type (make-regexp "^\\s*\\b([^{]+)\\b"))
  (define rx-open-brace (make-regexp "^\\s*\\{"))
  (define rx-id (make-regexp "^\\s*\\b([^},]+)\\b"))
  (define rx-comma (make-regexp "^\\s*([,}])"))
  (define rx-field-name (make-regexp "^\\s*\\b([a-z_0-9]+)\\b\\s*=\\s*" regexp/icase))
  (define rx-field-value (make-regexp "^\\s*\\{(.*[^\\]|)\\}"))
  (define rx-field-symbol (make-regexp "^(.)"))
  (define field-name "")
  (define field-value "")
  (define current-symbol #\ )
  (define prev-symbol #\ )
  (define first-symbol #f)
  (define brackets-sum 0)
  (define quotes-sum 0)
  (define had-spaces? #f)
  (define rules
    `(("at"
       ,rx-@
       ,(lambda (match lst) (cons (cons (make <reference>) lst) rx-type)))
      ("type"
       ,rx-type
       ,(lambda (match lst)
          (define ref (car lst))
          (reference-set-field! ref "bibtex.type" (string-downcase (match:substring match 1)))
          (cons lst rx-open-brace)))
      ("open-brace"
       ,rx-open-brace
       ,(lambda (match lst) (cons lst rx-id)))
      ("id"
       ,rx-id
       ,(lambda (match lst)
          (define ref (car lst))
          (reference-set-field! ref "bibtex.id" (match:substring match 1))
          (cons lst rx-comma)))
      ("comma"
       ,rx-comma
       ,(lambda (match lst)
          (if (string=? (match:substring match 1) ",")
            (cons lst rx-field-name)
            (cons lst rx-@))))
      ("field-name"
       ,rx-field-name
       ,(lambda (match lst)
          (define ref (car lst))
          (set! field-name (match:substring match 1))
          (set! field-value "")
          (cons lst rx-field-symbol)))
      ("field-symbol"
       ,rx-field-symbol
       ,(lambda (match lst)
          (define prev prev-symbol)
          (set! current-symbol (string-ref (match:substring match 1) 0))
          (set! prev-symbol current-symbol)
          (if (and (not first-symbol)
                   (not (char-whitespace? current-symbol)))
            (set! first-symbol current-symbol))
          (define old-brackets-sum brackets-sum)
          (define old-quotes-sum quotes-sum)
          (cond
            ((and (char=? current-symbol #\{) (not (char=? prev-symbol #\\)))
             (set! brackets-sum (+ brackets-sum 1)))
            ((and (char=? current-symbol #\}) (not (char=? prev-symbol #\\)))
             (set! brackets-sum (- brackets-sum 1)))
            ((and (char=? current-symbol #\") (not (char=? prev-symbol #\\)))
             (set! quotes-sum (+ 1 quotes-sum)))
            ((char-whitespace? current-symbol)
             (set! had-spaces? #t)))
          (if first-symbol
            (set! field-value (string-append field-value (string current-symbol))))
          ;(format #t "current ~a first ~a brackets-sum ~a quotes-sum ~a had-spaces? ~a\n"
          ;        current-symbol first-symbol brackets-sum quotes-sum had-spaces?)
          (cond
            ((and (not had-spaces?)
                  first-symbol
                  (not (char=? first-symbol #\{))
                  (not (char=? first-symbol #\")))
             (if (char=? current-symbol #\,)
               (begin
                 (reference-set-bibtex!
                   (car lst) field-name
                   (decode-latex
                     (string-trim-both
                       (cond
                         ((or
                            (and (char=? first-symbol #\{) (char=? current-symbol #\}))
                            (and (char=? first-symbol #\") (char=? current-symbol #\")))
                          (substring field-value 1 (- (string-length field-value) 1)))
                         (else
                           field-value)))))
                 (cons lst rx-field-name))
               (begin
                 (cons lst rx-field-symbol))))
            ((and (= brackets-sum 0) (= (remainder quotes-sum 2) 0) first-symbol)
             (reference-set-bibtex! (car lst) field-name
                                    (decode-latex (string-trim-both field-value)))
             (cons lst rx-comma))
            (else
              (cons lst rx-field-symbol))
            )))
      ))
  (define (rule-regex rule) (list-ref rule 1))
  (define current-regex rx-@)
  (define last-rule #f)
  (define entries '())
  (set! str (string-trim-both str))
  (while (not (string-null? str))
    (let ()
      (define matching-rules (filter (lambda (rule) (eq? (rule-regex rule) current-regex)) rules))
      (if (null? matching-rules)
        (break))
      (define rule (car matching-rules))
      (define match (regexp-exec (rule-regex rule) str))
      (set! last-rule rule)
      (if match
        (let ((result ((list-ref rule 2) match entries)))
          ;(pretty-print (reference->sxml (car (car result))))
          (set! entries (car result))
          (set! current-regex (cdr result))
          (set! str (match:suffix match)))
        (let ((i (string-index str #\newline)))
          (format (current-error-port) "failed to parse BibTeX with rule \"~a\": ~a\n"
                  (car rule)
                  (if i (substring str 0 i) str))
          (pretty-print (map reference->sxml (reverse entries)))
          (set! str "")
          (set! entries '())))
      ))
  (if (and last-rule (not (eq? current-regex rx-@)))
    (let ((i (string-index str #\newline)))
      (format (current-error-port) "failed to parse BibTeX with rule \"~a\": ~a\n"
              (car last-rule)
              (if i (substring str 0 i) str))
      (pretty-print (map reference->sxml (reverse entries)))
      '())
    (map
      (lambda (ref)
        (define year (assoc-ref (reference-fields ref) "bibtex.year"))
        (define month (assoc-ref (reference-fields ref) "bibtex.month"))
        (cond
          ((and year month)
           (set! (reference-date ref)
             (string->date "%Y-%m" (format #f "~a-~a" year (bibtex-month->number month))))
           (reference-remove-field! ref "bibtex.year")
           (reference-remove-field! ref "bibtex.month"))
          ((and year (not month))
           (set! (reference-date ref) (string->date "%Y" year))
           (reference-remove-field! ref "bibtex.year")))
        ref)
      (reverse entries))))

;; export all symbols
(module-map
  (lambda (sym var)
    (force-output)
    (module-export! (current-module) (list sym)))
  (current-module))
