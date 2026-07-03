(load "./src/xml.so")
(import (xml))

(define xml-ref-answer
  '(("note")
    (("to" ("post-id" . "100084")) "George")
    (("from") "John")
    (("heading") "Reminder")
    (("body") "Don't forget the meeting!")))

(define fp (open-input-file "./test/note.xml"))

(define xml (xml-load fp #f))

(pretty-print xml)

(if (equal? xml-ref-answer
            xml)
    (display "\nRight answer!\n")
    (display "\nSomething wrong!\n"))

;; --- v0.2 test ---
(define v0.2-answer
  '(("root")
    (("element" ("attr" . "value")))
    (("child") "text & more")))

(define fp2 (open-input-file "./test/test-v0.2.xml"))
(define xml2 (xml-load fp2 #f))
(pretty-print xml2)

(if (equal? xml2 v0.2-answer)
    (display "\nλ-0.2 Right answer!\n")
    (display "\nλ-0.2 Something wrong!\n"))
