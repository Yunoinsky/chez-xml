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
