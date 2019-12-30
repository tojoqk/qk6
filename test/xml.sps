(import (rnrs base)
        (qk6 xml)
        (qk6 test))

(test-begin "xml")
(test-equal "<br/>"
  (xml->string '(br)))
(test-equal "<image href=\"http://example.com/image.png\"/>"
  (xml->string '(image ((href "http://example.com/image.png")))))
(test-equal "<p>Hello, world!</p>"
  (xml->string '(p "Hello, world!")))
(test-end "xml")
