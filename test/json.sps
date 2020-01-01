(import (rnrs base)
        (rnrs io simple)
        (rnrs control)
        (rnrs programs)
        (qk6 json)
        (qk6 test))

(test-begin "json")

(test-equal "[string->json] string"
  "hello, world!"
  (string->json "\"hello, world!\""))

(test-equal "[string->json] number"
  42
  (string->json "42"))

(test-equal "[string->json] empty object"
  '()
  (string->json "{}"))

(test-equal "[string->json] object 1"
  '(("msg" . "say \\\"hello, world!\\\""))
  (string->json "{\"msg\":\"say \\\"hello, world!\\\"\"}"))

(test-equal "[string->json] object 3"
  '(("a" . #t) ("b" . #f) ("c" . null))
  (string->json "{\"a\":true,\"b\":false,\"c\":null}"))

(test-equal "[string->json] nested object"
  '(("a" . (("b" . (("c" . ())))
            ("B" . #t)))
    ("A" . 42))
  (string->json "{\"a\": {\"b\"
:
{\"c\":{}}, \"B\":true}, \"A\":  42}"))

(test-equal "[string->json] empty array 1"
  '#()
  (string->json "[]"))

(test-equal "[string->json] empty array 2"
  '#()
  (string->json "[ ]"))

(test-equal "[string->json] array 1"
  '#("hello")
  (string->json "[\"hello\"]"))

(test-equal "[string->json] array 3"
  '#(42 42.0 #f)
  (string->json "[42,  42.0, false]"))

(test-equal "[string->json] nested array"
  '#("a" #("b" #("c") "B" "A"))
  (string->json "[\"a\", [\"b\", [\"c\"], \"B\", \"A\"]]"))

(test-equal "[json->string] string"
  "\"hello, world!\""
  (json->string "hello, world!"))

(test-equal "[json->string] number"
  "42"
  (json->string 42))

(test-equal "[json->string] empty object"
  "{}"
  (json->string '()))

(test-equal "[json->string] object 1"
  "{\"msg\":\"say \\\"hello, world!\\\"\"}"
  (json->string '(("msg" . "say \\\"hello, world!\\\""))))

(test-equal "[json->string] object 3"
  "{\"a\":true,\"b\":false,\"c\":null}"
  (json->string '(("a" . #t) ("b" . #f) ("c" . null))))

(test-equal "[json->string] nested object"
  "{\"a\":{\"b\":{\"c\":{}},\"B\":true},\"A\":42}"
  (json->string '(("a" . (("b" . (("c" . ())))
                          ("B" . #t)))
                  ("A" . 42))))

(test-equal "[json->string] empty array"
  "[]"
  (json->string '#()))

(test-equal "[json->string] array 1"
  "[\"hello\"]"
  (json->string '#("hello")))

(test-equal "[json->string] array 3"
  "[42,42.0,false]"
  (json->string '#(42 42.0 #f)))

(test-equal "[json->string] nested array"
  "[\"a\",[\"b\",[\"c\"],\"B\",\"A\"]]"
  (json->string '#("a" #("b" #("c") "B" "A"))))

(test-end "json")
