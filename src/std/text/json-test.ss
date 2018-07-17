;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; :std/text/json unit test

(import :std/test
        :std/text/json
        :std/sugar)
(export #t)

(def (check-internal-encode-decode obj)
  (let (p (open-output-u8vector))
    (std/text/json#write-json-object obj p)
    (let (q (open-input-u8vector (get-output-u8vector p)))
      (check (std/text/json#read-json-object q) => obj))))

(def (check-internal-encode-decode= obj)
  (let (p (open-output-u8vector))
    (std/text/json#write-json-object obj p)
    (let (q (open-input-u8vector (get-output-u8vector p)))
      (checkf = (std/text/json#read-json-object q) obj))))

(def (check-internal-encode-decode-number num)
  (check-internal-encode-decode= num)
  (check-internal-encode-decode= (- num)))

(def (check-encode-decode-json input)
  (let ((p (open-output-u8vector)))
    (write-json input p)
    (let* ((q (open-input-u8vector (get-output-u8vector p))))
      (checkf json-equal? (read-json q) input))))

(def json-test-internal
  (test-suite "test :std/text/json"

    (test-case "test internal object encoding and decoding"
      (check-internal-encode-decode #t)
      (check-internal-encode-decode #f)
      (check-internal-encode-decode (void))
      (check-internal-encode-decode 0)
      (for-each check-internal-encode-decode-number
                '(1 2 3 4 5 10 20 50 101
                    0.0 0.5 1.0 1.337 2.0 3.5 1e6 1.3337e6 1.234e9))
      (for-each check-internal-encode-decode-number
                '(10. 100. 1e3 1e4 1e5 1e6 1e7 1e8 1e9 1e10
                  .1 .01 1e-3 1e-4 1e-5 1e-6 1e-7 1e-8 1e-9 1e-10))
      (check-internal-encode-decode "a string")
      (check-internal-encode-decode [1 2 3 [4 5] ["six" "seven"]])
      (check-internal-encode-decode (hash-eq (a 1) (b 2) (c (hash-eq (d 3) (e 4) (f 5)))))
      (parameterize ((json-symbolic-keys #f))
        (check-internal-encode-decode (hash ("a" 1) ("b" 2) ("c" (hash ("d" 3) ("e" 4) ("f" 5)))))))
    ))


(defstruct foo (x))
(defclass bar (x))

(def test-json-object
  (test-suite "test external repr :std/text/json"
              (test-case "test object encoding and decoding"
               (check (class->json-object (make-bar x: 1)) ? json-object?)
               (check (struct->json-object (make-foo 1)) ? json-object?)
               (check (hash->json-object (let (ht (make-hash-table))
                                          (hash-put! ht x: 3)
                                          ht))
                      ? json-object?)
               (check (alist->json-object '((x . 4) (y . 5))) ? json-object?)
               (check (class->json-object (make-bar x: (make-foo 2))) ? json-object?)
               (let ((empty-json-string "")
                     (json-str-numbers "{\"foo\":5,\"bar\":5.1}")
                     (json-str-lists "{\"x\":[1,2,3],\"y\":[1,[2,3,4[34,\"a string\"]],2]}")
                     (json-str-object "{\"x\":[1,2,3]}")
                     (empty-json (make-json))
                     (json-basic (make-json))
                     (json-lists (make-json))
                     (json-structs (make-json))
                     (json-classes (make-json))
                     (json-nested-objects (make-json))
                     (struct-foo (make-foo 1))
                     (class-bar (make-bar x: 2))
                     (nested-foobar (make-foo (make-bar x: (make-foo 5)))))
                 (check (json-add! json-basic a: 5) => (void))
                 (check (json-add! json-basic b: [1 2 3]) => (void))
                 (check (json-add! json-basic c: "a string") => (void))
                 (check (json-add! json-lists a: [1 [2 3[4 5]] 3]) => (void))
                 (check (json-add! json-structs 'y struct-foo) => (void))
                 (check (json-add! json-classes 'x class-bar) => (void))
                 (check (json-add! json-nested-objects 'x nested-foobar) => (void))
                 (check (json-object->string json-basic) ? string?)
                 (check json-equal? (make-json) empty-json)
                 (checkf json-equal? json-basic json-basic)
                 (checkf json-equal? json-lists json-lists)
                 (checkf json-equal? json-structs json-structs)
                 (checkf json-equal? json-classes json-classes)
                 (checkf json-equal? json-nested-objects json-nested-objects)
                 (checkf json-equal? (copy-json json-basic) json-basic)
                 (checkf json-equal? (copy-json json-structs) json-structs)
                 (checkf json-equal? (copy-json json-nested-objects) json-nested-objects)
                 (check-encode-decode-json json-basic)
                 (check-encode-decode-json json-lists)
                 (check-encode-decode-json json-structs)
                 (check-encode-decode-json json-classes)
                 (check-encode-decode-json json-nested-objects)

                 ))))

(def (make-json-fill)
  (let (json-obj (make-json))
    (json-add! json-obj foo: 5)
    (json-add! json-obj bar: 5.1)
    (json-add! json-obj foobar: "a string")
    (json-add! json-obj baz: (make-foo 1))
    json-obj))
