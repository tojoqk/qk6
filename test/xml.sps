(import (rnrs base)
        (qk6 xml)
        (qk6 test))

(test-begin "xml")

(test-equal "empty tag"
  "<br/>"
  (xml->string '(br)))

(test-equal "empty tag with attribute"
  "<image href=\"http://example.com/image.png\"/>"
  (xml->string '(image ((href "http://example.com/image.png")))))

(test-equal "simple tag"
  "<p>Hello, world!</p>"
  (xml->string '(p "Hello, world!")))

(test-end "xml")
