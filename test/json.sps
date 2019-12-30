(import (rnrs base)
        (rnrs io simple)
        (rnrs control)
        (rnrs programs)
        (qk6 json)
        (qk6 test))

(test-begin "json")
(define string/string "\"hello, world!\"")
(define number/string "42")
(define empty-object/string "{}")
(define object-1/string "{\"msg\":\"say \\\"hello, world!\\\"\"}")
(define object-3/string "{\"a\":true,\"b\":false,\"c\":null}")
(define nested-object/string "{\"a\": {\"b\": {\"c\": {}}, \"B\": true}, \"A\":42}")
(define normalized-nested-object/string "{\"a\":{\"b\":{\"c\":{}},\"B\":true},\"A\":42}")
(define empty-array/string "[]")
(define empty-array-1/string "[ ]")
(define array-1/string "[\"hello\"]")
(define array-3/string "[42, 42.0, false]")
(define normalized-array-3/string "[42,42.0,false]")
(define nested-array/string "[[],\r[[[]],\n[\"a\", \"b\"]]]")
(define normalized-nested-array/string "[[],[[[]],[\"a\",\"b\"]]]")

(define string/json "hello, world!")
(define number/json 42)
(define empty-object/json '())
(define object-1/json '(("msg" . "say \\\"hello, world!\\\"")))
(define object-3/json '(("a" . #t) ("b" . #f) ("c" . null)))
(define nested-object/json '(("a" . (("b" . (("c" . ())))
                                     ("B" . #t)))
                             ("A" . 42)))
(define empty-array/json '#())
(define array-1/json '#("hello"))
(define array-3/json '#(42 42.0 #f))
(define nested-array/json '#(#() #(#(#()) #("a" "b"))))

(test-equal string/json
  (string->json string/string))
(test-equal number/json
  (string->json number/string))
(test-equal empty-object/json
  (string->json empty-object/string))
(test-equal object-1/json
  (string->json object-1/string))
(test-equal object-3/json
  (string->json object-3/string))
(test-equal nested-object/json
  (string->json nested-object/string))
(test-equal empty-array/json
  (string->json empty-array/string))
(test-equal (string->json empty-array-1/string)
  empty-array/json)
(test-equal array-1/json
  (string->json array-1/string))
(test-equal array-3/json
  (string->json array-3/string))
(test-equal (string->json nested-array/string)
  nested-array/json)

(test-equal string/string
  (json->string string/json))
(test-equal number/string
  (json->string number/json))
(test-equal empty-object/string
  (json->string empty-object/json))
(test-equal object-1/string
  (json->string object-1/json))
(test-equal object-3/string
  (json->string object-3/json))
(test-equal normalized-nested-object/string
  (json->string nested-object/json))
(test-equal empty-array/string
  (json->string empty-array/json))
(test-equal array-1/string
  (json->string array-1/json))
(test-equal normalized-array-3/string
  (json->string array-3/json))
(test-equal normalized-nested-array/string
  (json->string nested-array/json))
(test-end "json")
