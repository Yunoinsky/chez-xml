(load "./src/xml.so")
(import (xml))
(import (chezscheme))

(define (assert-equal name expected actual)
  (if (equal? expected actual)
      (begin
        (display "✓ ")
        (display name)
        (newline))
      (begin
        (display "✗ ")
        (display name)
        (newline)
        (display "  expected: ")
        (write expected)
        (newline)
        (display "  got:      ")
        (write actual)
        (newline))))

;; ====================
;; v0.1: basic test
;; ====================
(define fp1 (open-input-file "./test/note.xml"))
(define xml1 (xml-load fp1 #f))
(assert-equal "v0.1 note.xml"
  '(("note")
    (("to" ("post-id" . "100084")) "George")
    (("from") "John")
    (("heading") "Reminder")
    (("body") "Don't forget the meeting!"))
  xml1)

;; ====================
;; v0.2: DTD, comments-in-tag, decl-anywhere
;; ====================
(define fp2 (open-input-file "./test/test-v0.2.xml"))
(define xml2 (xml-load fp2 #f))
(assert-equal "v0.2 features"
  '(("root")
    (("element" ("attr" . "value")))
    (("child") "text & more"))
  xml2)

;; ====================
;; Comprehensive test (preserve-blank = #f)
;; ====================
(define fp3 (open-input-file "./test/test-comprehensive.xml"))
(define xml3 (xml-load fp3 #f))
(assert-equal "comprehensive (no-blank)"
  '(("catalog")
    (("book" ("available" . "yes") ("id" . "bk101"))
      (("author" ("married") ("gender" . "male")) "Jim <The Rock> Smith")
      (("title") "XML Developer's Guide")
      (("price" ("unit" . "USD")) "44.95")
      (("empty"))
      (("nested")
        (("deep")
          (("value") "\"hello & world\"")))
      (("mixed") "text " (("inline") "inner") " more text")))
  xml3)

;; ====================
;; Comprehensive test (preserve-blank = #t)
;; ====================
(define fp4 (open-input-file "./test/test-comprehensive.xml"))
(define xml4 (xml-load fp4 #t))

;; Helper: extract children excluding interleaved whitespace strings
(define (elem-children node) (filter pair? (xml-get-children node)))
(define (text-children node) (filter string? (xml-get-children node)))

(define catalog xml4)
(assert-equal "blank: root name" "catalog" (xml-get-name catalog))

(define book (car (elem-children catalog)))
(assert-equal "blank: book name" "book" (xml-get-name book))
(assert-equal "blank: book attrs"
  '(("available" . "yes") ("id" . "bk101"))
  (xml-get-attrs book))

(define book-elems (elem-children book))

(define author (car book-elems))
(assert-equal "blank: author name" "author" (xml-get-name author))
(define author-text (car (text-children author)))
(assert-equal "blank: author text" "Jim <The Rock> Smith" author-text)

(define title (cadr book-elems))
(assert-equal "blank: title text" "XML Developer's Guide" (car (text-children title)))

(define price (caddr book-elems))
(assert-equal "blank: price unit" "USD" (cdr (car (xml-get-attrs price))))
(assert-equal "blank: price value" "44.95" (car (text-children price)))

(define empty (cadddr book-elems))
(assert-equal "blank: empty element" "empty" (xml-get-name empty))
(assert-equal "blank: empty no children" '() (xml-get-children empty))

(define nested (car (cddddr book-elems)))
(assert-equal "blank: nested" "nested" (xml-get-name nested))

(define deep (car (elem-children nested)))
(assert-equal "blank: deep" "deep" (xml-get-name deep))

(define value (car (elem-children deep)))
(assert-equal "blank: value text" "\"hello & world\"" (car (text-children value)))

(define mixed (cadr (cddddr book-elems)))
(define mixed-elems (elem-children mixed))
(assert-equal "blank: mixed inline name" "inline" (xml-get-name (car mixed-elems)))

;; ====================
;; Edge cases
;; ====================

;; Self-closing root
(define fp-sr (open-input-string "<root/>"))
(define xml-sr (xml-load fp-sr #f))
(assert-equal "self-closing root"
  '(("root"))
  xml-sr)

;; Comment-only document
(define fp-co (open-input-string "<!-- nothing -->"))
(define xml-co (xml-load fp-co #f))
(assert-equal "comment-only doc" '() xml-co)

;; Bare text (no elements)
(define fp-bt (open-input-string "hello"))
(define xml-bt (xml-load fp-bt #f))
(assert-equal "bare text returns empty" '() xml-bt)

;; Multiple <?...?> in a row
(define fp-pi (open-input-string "<?a?><?b?><root/>"))
(define xml-pi (xml-load fp-pi #f))
(assert-equal "multiple PI then element"
  '(("root"))
  xml-pi)

;; Nested self-closing
(define fp-ns (open-input-string "<a><b/><c><d/></c></a>"))
(define xml-ns (xml-load fp-ns #f))
(assert-equal "nested self-closing"
  '(("a") (("b")) (("c") (("d"))))
  xml-ns)

;; Attribute with empty value
(define fp-ev (open-input-string "<e a=\"\"/>"))
(define xml-ev (xml-load fp-ev #f))
(assert-equal "empty attr value"
  '(("a" . ""))
  (xml-get-attrs xml-ev))

;; Multiple standalone attrs
(define fp-sa (open-input-string "<e a b c/>"))
(define xml-sa (xml-load fp-sa #f))
(assert-equal "standalone attrs"
  '(("c") ("b") ("a"))
  (xml-get-attrs xml-sa))

;; Deep DTD with nested brackets
(define fp-dd (open-input-string "<!DOCTYPE a [<!ELEMENT a (b)>]><a/>"))
(define xml-dd (xml-load fp-dd #f))
(assert-equal "DTD with nested brackets"
  '(("a"))
  xml-dd)

;; Comment between attrs (attrs-loop)
(define fp-ca (open-input-string "<e a=\"1\" <!-- c --> b=\"2\"/>"))
(define xml-ca (xml-load fp-ca #f))
(assert-equal "comment between attrs"
  '(("b" . "2") ("a" . "1"))
  (xml-get-attrs xml-ca))

;; Comment between element name and attrs (no space — label-loop)
(define fp-cl (open-input-string "<elem<!-- c --> a=\"v\"/>"))
(define xml-cl (xml-load fp-cl #f))
(assert-equal "comment after name (no space)"
  '(("a" . "v"))
  (xml-get-attrs xml-cl))

;; XML declaration in middle of document
(define fp-xd (open-input-string "<root><?x a?></root>"))
(define xml-xd (xml-load fp-xd #f))
(assert-equal "PI inside root"
  '(("root"))
  xml-xd)

;; Entity refs at boundaries
(define fp-er (open-input-string "<x>&lt;start &amp; end&gt;</x>"))
(define xml-er (xml-load fp-er #f))
(assert-equal "entities at boundaries"
  '(("x") "<start & end>")
  xml-er)

;; Multiple mixed entities
(define fp-me (open-input-string "<x>&lt;&amp;&gt;</x>"))
(define xml-me (xml-load fp-me #f))
(assert-equal "multiple entities only"
  '(("x") "<&>")
  xml-me)

;; Deep nesting
(define fp-dn (open-input-string "<a><b><c><d><e/></d></c></b></a>"))
(define xml-dn (xml-load fp-dn #f))
(assert-equal "deep nesting"
  '(("a") (("b") (("c") (("d") (("e"))))))
  xml-dn)

;; Comment-only with preserve-blank
(define fp-cb (open-input-string "<x><!-- c --></x>"))
(define xml-cb (xml-load fp-cb #t))
(assert-equal "comment-only child (blank)"
  '(("x"))
  xml-cb)

;; Preserve blank: whitespace text around elements
(define fp-ws (open-input-string "<a>  <b/>  </a>"))
(define xml-ws (xml-load fp-ws #t))
(assert-equal "whitespace text preserved"
  "b"
  (xml-get-name (car (filter pair? (xml-get-children xml-ws)))))

;; DTD with quotes inside
(define fp-dq (open-input-string "<!DOCTYPE n [<!ENTITY q '\"'>]><n/>"))
(define xml-dq (xml-load fp-dq #f))
(assert-equal "DTD with quotes"
  '(("n"))
  xml-dq)

;; Attribute with newline inside quoted value
(define fp-nl (open-input-string "<e a=\"
\"/>"))
(define xml-nl (xml-load fp-nl #f))
(assert-equal "attr with newline"
  '(("a" . "\n"))
  (xml-get-attrs xml-nl))

(newline)
(display "All tests complete.")
(newline)
