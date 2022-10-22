(library (xml (0 1))
  (export xml-load
          xml-get-name xml-get-attrs xml-get-eles)
  (import (chezscheme))

  (define-syntax push!
    (syntax-rules ()
      [(_ rl item) (set! rl (cons item rl))]))

  (define-syntax push-cdr!
    (syntax-rules ()
      [(_ pair item) (set-cdr! pair (cons item (cdr pair)))]))

  (define-syntax pop!
    (syntax-rules ()
      [(_ rl) (let ([a (car rl)])
                (set! rl (cdr rl))
                a)]))

  (define-syntax inc!
    (syntax-rules ()
      [(_ x) (set! x (+ x 1))]
      [(_ x a) (set! x (+ x a))]))

  (define (blank-ch-list? ch-list)
    (andmap (lambda (c)
              (memq c '(#\newline #\return #\space #\tab)))
            ch-list))

  (define (rlist->string rlist)
    (list->string (reverse rlist)))

  (define (xml-get-name node)
    (caar node))

  (define (xml-get-attrs node)
    (cdar node))

  (define (xml-get-eles node)
    (cddr node))

  (define (xml-load fp reserve-space)
    (let ([ch-buffer '()])
      (define (next)
        (if (null? ch-buffer)
            (get-char fp)
            (pop! ch-buffer)))
      (define (parse-content class)
        (let ([ch (next)])
          (if (eof-object? ch)
              #f
              (case ch
                [#\< (parse-struct class)]
                [else (parse-string (list ch) #f)]))))

      (define (parse-entity)
        (let loop ([ch-rl '()]
                   [it 0])
          (when (> it 4)
            (error (rlist->string ch-rl)
                   "Parser Error: entity ref without end"))
          (let ([new-ch (next)])
            (when (eof-object? new-ch)
              (error (rlist->string ch-rl)
                     "Parser Error: entity ref not finish"))
            (if (char=? new-ch #\;)
                (case (rlist->string ch-rl)
                  ["lt" #\<]
                  ["gt" #\>]
                  ["amp" #\&]
                  ["apos" #\']
                  ["quot" #\"]
                  [else
                   (error
                    (rlist->string ch-rl)
                    "Parser Error: entity ref not defined")])
                (loop (cons new-ch ch-rl)
                      (+ it 1))))))
      (define (parse-string ch-l is-dquote)
        (let loop ([ch-rl ch-l])
          (let ([new-ch (next)])
            (case new-ch
              [#\<
               (push! ch-buffer #\<)
               (if (or reserve-space
                       (not (blank-ch-list? ch-rl)))
                   (rlist->string ch-rl)
                   'comment)]
              [#\& (loop (cons (parse-entity) ch-rl))]
              [else
               (if is-dquote
                   (cond
                    [(eof-object? new-ch)
                     (error #f
                            "Parser Error: not close ending")]
                    [(char=? new-ch #\")
                     (rlist->string ch-rl)]
                    [else (loop (cons new-ch ch-rl))])
                   (cond
                    [(eof-object? new-ch)
                     (if (or reserve-space
                             (not (blank-ch-list? ch-rl)))
                         (rlist->string ch-rl)
                         'comment)]
                    [else (loop (cons new-ch ch-rl))]))]))))

      (define (parse-struct class)
        (let ([ch (next)])
          (case ch
            [#\? (if (eq? class 'head)
                     (parse-xml-decl)
                     (error
                      '()
                      "Parser Error: xml decl not ahead"))]
            [#\! (let ([ch (next)])
                   (if (char=? ch #\-)
                       (parse-comment)
                       (if (eq? class 'head)
                           (begin
                             (push! ch-buffer ch)
                             (parse-decl))
                           (error
                            #f
                            "Parser Error: decl not ahead"))))]
            [#\> (error ch
                        "Parser Error: empty label")]
            [#\/ (parse-end-label class)]
            [else
             (unless (char-alphabetic? ch)
               (error ch
                      "Parser Error: invalid element name"))
             (push! ch-buffer ch)
             (parse-element)])))
      (define (parse-end-label name)
        (let match-loop ([res-ch (string->list name)])
          (if (null? res-ch)
              (let consume-loop ()
                (let ([ch (next)])
                  (case ch
                    [#\space (consume-loop)]
                    [#\> '()]
                    [else
                     (error
                      ch
                      "Invalid end label")])))
              (let ([ch (next)])
                (if (char=? ch (car res-ch))
                    (match-loop (cdr res-ch))
                    (error name
                           "Not match label"))))))
      (define (parse-comment)
        (unless (char=? (next) #\-)
          ("Parser Error: invalid comment"))
        (let loop ([hyphen-num 0])
          (let ([ch (next)])
            (when (eof-object? ch)
              (error
               #f
               "Parser Error: comment not terminated"))
            (case ch
              [#\- (loop (+ hyphen-num 1))]
              [#\> (if (> hyphen-num 1)
                       'comment
                       (loop 0))]
              [else (loop 0)]))))
      (define (parse-xml-decl)
        (let loop ()
          (let ([ch (next)])
            (when (eof-object? ch)
              (error
               #f
               "Parser Error: xml decl not terminated"))
            (case ch
              [#\?
               (if (char=? (next) #\>)
                   'xml-decl
                   (error
                    ch
                    "Parser Error: xml decl need ?> to end"))]
              [#\>
               (error
                ch
                "Parser Error: xml decl need ?> to end")]
              [else (loop)]))))
      (define (parse-decl)
        (let loop ()
          (let ([ch (next)])
            (when (eof-object? ch)
              (error
               #f
               "Parser Error: decl not terminated"))
            (if (char=? ch #\>)
                'decl
                (loop)))))
      (define (end-single-element el-name attrs)
        (if (and (char=? (next) #\>)
                 (not (string=? el-name "")))
            (list (cons el-name attrs))
            (error el-name
                   "Parser Error: invalid single element")))
      (define (parse-element)
        (define (label-loop ch-rl rl-len)
          (and (= rl-len 3)
               (string-ci=? (rlist->string ch-rl)
                            "xml")
               (error #f "Parser Error: name start with xml"))
          (let ([ch (next)])
            (when (eof-object? ch)
              (error (rlist->string ch-rl)
                     "Parser Error: label not terminated"))
            (case ch
              [(#\newline #\tab #\space #\return #\> #\/)
               (let ([element-name (rlist->string ch-rl)])
                 (case ch
                   [#\/ (end-single-element element-name '())]
                   [#\>
                    (cons (list element-name)
                          (contents-loop element-name))]
                   [else
                    (attrs-loop element-name '())]))]
              [else (label-loop (cons ch ch-rl)
                                (+ rl-len 1))])))
        (define (attrs-loop element-name attrs)
          (let ([ch (next)])
            (when (eof-object? ch)
              (error ch
                     "Parser Error: attrs not terminated"))
            (case ch
              [#\/ (end-single-element element-name attrs)]
              [(#\newline #\tab #\space #\return)
               (attrs-loop element-name attrs)]
              [#\> (cons (cons element-name attrs)
                         (contents-loop element-name))]
              [else
               (unless (char-alphabetic? ch)
                 (error ch "Parser Error: invalid attr name"))
               (push! ch-buffer ch)
               (attrs-loop element-name
                           (cons (get-attr-pair) attrs))])))
        (define (get-attr-pair)
          (let ([key (get-attr-key '())])
            (let loop ()
              (let ([ch (next)])
                (when (eof-object? ch)
                  (error
                   #f
                   "Parser Error: label attrs not terminated"))
                (case ch
                  [(#\newline #\return)
                   (cons key '())]
                  [(#\tab #\space)
                   (loop)]
                  [#\= (cons key (get-attr-value))]
                  [else
                   (push! ch-buffer ch)
                   (cons key '())])))))
        (define (get-attr-key ch-rl)
          (let ([ch (next)])
            (when (or (eof-object? ch)
                      (eq? ch #\<))
              (error (rlist->string ch-rl)
                     "Parser Error: attr key not terminated"))
            (case ch
              [(#\= #\/ #\space #\newline #\tab)
               (push! ch-buffer ch)
               (rlist->string ch-rl)]
              [else (get-attr-key (cons ch ch-rl))])))
        (define (get-attr-value)
          (let ([ch (next)])
            (when (eof-object? ch)
              (error #f
                     "Parser Error: value key not start"))
            (case ch
              [(#\space #\tab) (get-attr-value)]
              [#\" (parse-string '() #t)]
              [else
               (error #f
                      "Parser Error: invalid value")])))
        (define (contents-loop element-name)
          (let ([new-content (parse-content element-name)])
            (cond
             [(null? new-content) '()]
             [(eq? new-content 'comment)
              (contents-loop element-name)]
             [else
              (cons new-content
                    (contents-loop element-name))])))
        (label-loop '() 0))

      (let main-loop ([result '()]
                      [class 'head])
        (let ([content (parse-content class)])
          (if content
              (if (null? result)
                  (apply main-loop
                         (if (or (symbol? content)
                                 (string? content))
                             (list '() 'head)
                             (list content 'tail)))
                  (if (or (eq? content 'comment)
                          (string? content))
                      (main-loop result 'tail)
                      (error
                       #f
                       "Parser Error: struct after root")))
              result))))))
