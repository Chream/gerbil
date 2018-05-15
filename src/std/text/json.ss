;;; -*- Gerbil -*-
;;; (C) vyzo at hackzen.org
;;; json library
package: std/text

;;; JSON - scheme correspondence:
;;; JSON encoding: the following objects can be converted to json:
;;;  - booleans
;;;  - void (corresponding to js's null)
;;;  - real numbers
;;;  - strings
;;;  - proper lists
;;;  - vectors (encoded as lists)
;;;  - hashes with symbolic or string keys mapping to json encodable objects
;;;  - any object that defines a :json method producing a json encodable objects
;;; JSON decoding produces JSON-encodable objects with the following.
;;; Note that JSON null is decoded as #!void and JSON lists are decoded as
;;;  Scheme lists
(import :gerbil/gambit/ports
        :gerbil/gambit/bits
        :std/error
        :std/sugar
        :std/misc/rtd
        :std/misc/list
        (only-in :std/srfi/1 reverse! append-map append!)
        (only-in :std/misc/list alist? alist->plist plist->alist))
(export read-json write-json
        string->json json->string
        json-symbolic-keys)

;; Needed utils

(def (check-type specifier? obj)
  "Ensures OBJ conforms to the SPECIFIER? procedure.
   Raises error on fail. Retuns void on success."
  (unless (specifier? obj)
    (error "Object ~S does not fulfill type."
      obj specifier?)))

;; Add to :std/misc/repr or :std/misc/rtd ?

(def (slot-names obj)
  "Returns a list of slot names as symbols for OBJ."
  (cdar (type-descriptor-plist (object-type obj))))

(def (struct-object? obj)
  "Returns #t if OBJ is a struct type. #f else."
  (try
   (let (plist (type-descriptor-plist
                (object-type obj)))
     (match plist
       ([[fields: slots ...]] #t)
       (else #f)))
   (catch (e)
     #f)))

(def (class-object? obj)
  "Returns #t if OBJ is a class type. #f else."
  (try
   (let (plist (type-descriptor-plist
                (object-type obj)))
     (match plist
       ([[slots: slots ...]] #t)
       (else #f)))
   (catch (e)
     #f)))

(def (object->list obj)
  "Retuns a shallow list of the slots of OBJ.
   Should accept objects conforming to `object?'.
   Currently class, struct and hash-table objects."
  (cond ((class-object? obj)
         (cons type: (class->list obj)))
        ((struct-object? obj)
         (let* ((type (object-type obj))
                (slot-names (map symbol->keyword (slot-names obj)))
                (slot-values (cdr (struct->list obj))))
           (cons* type: type (append-map list slot-names slot-values))))
        ((hash-table? obj)
         (let* ((type (object-type obj)))
           `(type: ,type ,@(alist->plist (hash->list obj)))))
        (else (error "Not an object!" obj))))


;; Add to :std/misc/hash ?

(def (hash-add! ht key value)
  "Adds KEY . VALUE to the hash-table. Raises an error
   if the key is already present."
  (let ((present? (hash-get ht key)))
    (if present?
      (error "Hash key already present!" key present?)
      (hash-put! ht key value))))

(def (hash-empty? ht)
  (= (hash-length ht) 0))

;;; end Nedded utils.

;; Parameters

;; should decoded hashes have symbols as keys?
(def json-symbolic-keys
  (make-parameter #t))

;; JSON interface

(defstruct json (e) constructor: init!)

(defmethod {init! json}
  (lambda (self (ht (if (json-symbolic-keys)
                 (make-hash-table-eq)
                 (make-hash-table))))
    (json-e-set! self ht)))

(def (read-json (port (current-output-port)))
  "Reads a json object from character PORT."
  (let (ht (read-json-object port #f))
    (make-json ht)))

(def (read-json-file path/options)
  "Read a json object from file correspong to PATH."
  (call-with-input-file path/options
    (lambda (in)
      (read-json in))))

(def (write-json-file path/options)
  "Writes a json object to file correspong to PATH."
  (call-with-output-file path/options
    (lambda (in)
      (read-json in))))

(def (write-json obj (port (current-output-port)))
  "Writes OBJ, a `json' object as a json object to character PORT."
  (write-json-object (json-e obj) port))

(def (json-empty? obj)
  "Return #t if `json' object is empty. #f else."
  (with ((json e) obj)
    (hash-empty? e)))

(def (copy-json obj)
  "Return a deep copy of OBJ, a `json' object."
  (with ((json e) obj)
    (make-json (string->json
                (json->string e)))))

(def (json-input-fn-generator get-fn set-fn! force-set-fn! constructor-fn)
  (lambda (obj . entry-spec)
    (with ((json e) obj)
      (let lp! ((table-1 e)
                (entry-spec-1 entry-spec))
        (cond ((null? entry-spec-1)
               (error "Invalid number of keys. key-length: : " (length entry-spec)))
              ((= 1 (length entry-spec-1))
               (set-fn! table-1 (car entry-spec-1) (constructor-fn))
               obj)
              ((= 2 (length entry-spec-1))
               (set-fn! table-1 (car entry-spec-1) (cadr entry-spec-1))
               obj)
              (else
               (let* ((key (car entry-spec-1))
                      (entry (get-fn table-1 key)))
                 (cond ((hash-table? entry)
                        (lp! entry (cdr entry-spec-1)))
                       (entry
                        (let (ht-1 (make-json))
                          (set-fn! ht-1 "__old-value" entry)
                          (force-set-fn! table-1 key ht-1)
                          (lp! ht-1 (cdr entry-spec-1))))
                       (else
                        (let (ht-2 (make-json))
                          (set-fn! table-1 key ht-2)
                          (lp! ht-2 (cdr entry-spec-1))))))))))))

(def json-add! (json-input-fn-generator hash-get hash-add! hash-put! make-json))
(def json-put! (json-input-fn-generator hash-get hash-put! hash-put! make-json))

(def (json-delete! obj . entry-spec)
  (with ((json e) obj)
    (let lp! ((ht-1 e)
              (entry-spec-1 entry-spec))
      (cond ((null? entry-spec-1)
             (error "Illegal number of arguments Must be odd. length: "
               (length entry-spec-1)))
            ((= 1 (length entry-spec-1))
             (let* ((key (car entry-spec-1))
                    (present? (hash-get ht-1 key)))
               (if present?
                 (begin
                   (hash-remove! ht-1 key)
                   (values present? #t))
                 (values #f #f))))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (hash-get ht-1 key)))
               (if (hash-table? entry)
                 (lp! entry (cdr entry-spec-1))
                 (error "json-delete: key not present!" (car entry-spec-1)))))))))

(def (json-get obj . entry-spec)
  (with ((json e) obj)
    (let lp ((ht-1 e)
             (entry-spec-1 entry-spec))
      (cond ((null? entry-spec)
             (error "illegal arguments."))
            ((= 1 (length entry-spec-1))
             (hash-get ht-1 (car entry-spec-1)))
            (else
             (let* ((key (car entry-spec-1))
                    (entry (hash-get ht-1 (car entry-spec-1))))
               (if (hash-table? entry)
                 (lp entry (cdr entry-spec-1))
                 #f)))))))

(def (json-append! obj . entry-spec)
  "Appends the last element of entry-spec, a LIST, to
   the corresping `json' object. Raises an error if
   the present value is not a list."
  (with ((json e) obj)
    (let lp! ((ht-1 e)
              (entry-spec-1 entry-spec))
      (let (len (length entry-spec-1))
        (cond ((null? entry-spec)
               (error "JSON-APPEND: illegal arguments." len))
              ((= 1 len)
               (error "JSON-APPEND: illegal arguments" len))
              ((= 2 len)
               (let* ((key (car entry-spec-1))
                      (new-list (cadr entry-spec-1))
                      (old-list (or (hash-get ht-1 key) []))
                      (app-list (append! new-list old-list)))
                 (check-type list? old-list)
                 (check-type list? new-list)
                 (json-put! ht-1 key app-list)))
              (else
               (let* ((key (car entry-spec-1))
                      (entry (json-get ht-1 key)))
                 (if (hash-table? entry)
                   (lp! entry (cdr entry-spec-1))
                   (error "json-append!: key not present!" (car entry-spec-1))))))))))

(def (json-merge! obj1 obj2)
  "Merges two `json' objects into OBJ1. Will append
   to already existing json lists."
  (let ((e1 (json-e obj1))
        (e2 (json-e obj2)))
    (hash-for-each
     (lambda (k v)
       (cond ((hash-table? v)
              (unless (hash-table? (json-get e1 k))
                (hash-put! e1 k (make-json)))
              (json-merge! (json-get e1 k) v))
             ((list? v)
              (unless (list? (json-get e1 k))
                (hash-put! e1 k (list)))
              (json-put! e1 k (append (json-get e1 k) v)))
             (else (json-put! e1 k v))))
     e2)))

;; Converters

(def (json<- obj include-meta: (include-meta? #f))
  "This method convert scheme objects to json compatible objects."
  (cond
   ((method-ref "hey" ':json)
    ;; As `call method' does not signal a specific error we cant catch
    ;; the specific error. ie need to check for a method manually.
    {:json obj})
   ((json?   obj)        obj)
   ((number? obj)        obj)
   ((object? obj)        (object->json obj include-meta: include-meta?))
   ((alist?  obj)        (alist->json obj include-meta: include-meta?))
   ((string? obj)        (try (string->json obj) (catch (e) obj)))
   ((list?   obj)        (map json<- obj))
   ((vector? obj)        (list->vector (map json<- (vector->list obj))))
   (else (error "Can not convert to JSON object" obj))))

(def (string-e key)
  "Ensures json key is of correct type."
  (cond
   ((string? key) key)
   ((symbol? key)
    (symbol->string key))
   ((keyword? key)
    (keyword->string key))
   (else
    (error "Illegal hash key; must be symbol, keyword or string" key))))

(def (alist->json alist include-meta: (include-meta? #f) into: (json (make-json)))
  (check-type alist? alist)
  (when include-meta?
    (json-add! json __alist: "null"))
  (let lp ((alist-1 alist))
    (match alist-1
      ([[key . val] . rest]
       (json-add! json
                  (string-e key)
                  (json<- val))
       (lp rest))
      ([] json)
      (alist (error "Not proper alist!" alist)))))

(def (json->alist obj)
  (match obj
    ((json e)
     (let (alist [])
       (hash-for-each
        (lambda (k v)
          (set! alist (cons (cons (if (json-symbolic-keys)
                                    (string->symbol k)
                                    k)
                                  (if (json? v)
                                    (json->alist v)
                                    v))
                            alist)))
        e)
       alist))))

(def (string->json str)
  (read-json (open-input-string str)))

(def (json->string obj)
  (let ((port (open-output-string)))
    (write-json obj port)
    (get-output-string port)))

(def (object->json obj include-meta: (include-meta? #f) into: (json (make-json)))
  (check-type object? obj)
  (cond
   ((hash-table? obj) (hash-table->json obj))
   ((struct-object? obj) (struct->json obj))
   ((class-object? obj) (class->json obj))
   (else (error "Not an object!" obj))))

(def (class->json obj include-meta: (include-meta? #f) into: (json (make-json)))
  (check-type class-object? obj)
  (with ([type: type slots ...] (object->list obj))
    (when include-meta?
      (json-add! json __class: (type-name type)))
    (alist->json (plist->alist slots) include-meta: include-meta? into: json))
  json)

(def (struct->json obj include-meta: (include-meta? #f) into: (json (make-json)))
  (check-type struct-object? obj)
  (with ([type: type slots ...] (object->list obj))
    (when include-meta?
      (json-add! json __struct: (type-name type)))
    (alist->json (plist->alist slots) include-meta: include-meta? into: json))
  json)

(def (hash-table->json obj include-meta: (include-meta? #f) into: (json (make-json)))
  (check-type hash-table? obj)
  (with ([type: type slots ...] (object->list obj))
    (when include-meta?
      (json-add! json __table: (type-name type)))
    (alist->json (plist->alist slots) include-meta: include-meta? into: json))
  json)


;;

;;; implementation
(def (raise-invalid-token port char)
  (if (eof-object? char)
    (raise-io-error 'read-json "Incomplete JSON object; EOF reached" port)
    (raise-io-error 'read-json "Invalid JSON token" port char)))

(def (read-json-object port (raise-eof? #t))
  (skip-whitespace port)
  (let (char (peek-char port))
    (if (eof-object? char)
      (if raise-eof?
        (raise-io-error 'read-json "EOF reached" port)
        #!eof)
      (case char
        ((#\{) (read-json-hash port))
        ((#\[) (read-json-list port))
        ((#\") (read-json-string port))
        ((#\t) (read-json-true port))
        ((#\f) (read-json-false port))
        ((#\n) (read-json-null port))
        ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (read-json-number port))
        (else
         (raise-invalid-token port char))))))

(def (skip-whitespace port)
  (let (char (peek-char port))
    (unless (eof-object? char)
      (when (char-whitespace? char)
        (read-char port)
        (skip-whitespace port)))))

(def (skip-chars chars port)
  (let lp ((rest chars))
    (match rest
      ([char . rest]
       (let (next (read-char port))
         (if (eq? next char)
           (lp rest)
           (raise-invalid-token port next))))
      (else (void)))))

(def (read-json-hash port)
  (read-char port)
  (let (obj (if (json-symbolic-keys)
              (make-hash-table-eq)
              (make-hash-table)))
    (let lp ()
      (let (key (read-json-hash-key port))
        (if key
          ;; If you see a duplicate key, it's as likely an attack as a bug. #LangSec
          (if (hash-key? obj key)
            (error "Duplicate hash key in JSON input" key)
            (let (val (read-json-object port))
              (hash-put! obj key val)
              (skip-whitespace port)
              (let (char (peek-char port))
                (case char
                  ((#\,)
                   (read-char port)
                   (lp))
                  ((#\})
                   (read-char port)
                   obj)
                  (else
                   (raise-invalid-token port char))))))
          obj)))))                   ; empty or trailing #\, [liberal]

(def (read-json-hash-key port)
  (skip-whitespace port)
  (let (char (peek-char port))
    (case char
      ((#\")
       (let (key (read-json-string port))
         (skip-whitespace port)
         (let (char (peek-char port))
           (case char
             ((#\:)
              (read-char port)
              (if (json-symbolic-keys)
                (string->symbol key)
                key))
             (else
              (raise-invalid-token port char))))))
      ((#\})
       (read-char port)
       #f)
      (else
       (raise-invalid-token port char)))))

(def (read-json-list port)
  (read-char port)
  (let lp ((els []))
    (let (next (read-json-list-next port))
      (if next
        (lp (cons next els))
        (reverse! els)))))

(def (read-json-list-next port)
  (skip-whitespace port)
  (let (char (peek-char port))
    (case char
      ((#\])
       (read-char port)
       #f)
      (else
       (let (obj (read-json-object port))
         (skip-whitespace port)
         (let (char (peek-char port))
           (case char
             ((#\,)
              (read-char port)
              obj)
             ((#\])
              obj)
             (else
              (raise-invalid-token port char)))))))))

(def hexes "0123456789abcdef")
(def HEXES "0123456789ABCDEF")
(def hextab
  (let (ht (make-hash-table-eq))
    (def (put str)
      (let (len (string-length str))
        (let lp ((k 0))
          (when (##fx< k len)
            (hash-put! ht (##string-ref str k) k)
            (lp (##fx+ k 1))))))
    (put hexes)
    (put HEXES)
    ht))

(def (read-json-string port)
  (def (read-escape-char port)
    (let (char (read-char port))
      (case char
        ((#\" #\\ #\/) char)
        ((#\b) #\backspace)
        ((#\n) #\newline)
        ((#\f) #\page)
        ((#\r) #\return)
        ((#\t) #\tab)
        ((#\u) (read-escape-unicode port))
        (else
         (raise-invalid-token port char)))))

  (def (read-escape-unicode port)
    (let lp ((n 0) (chars []))
      (if (##fx< n 4)
        (lp (##fx+ n 1) (cons (read-char port) chars))
        (let lp ((rest chars) (val 0) (shift 0))
          (match rest
            ([char . rest]
             (let (n (##fxarithmetic-shift (hex-value char) shift))
               (lp rest (bitwise-ior n val) (##fx+ shift 4))))
            (else
             (if (and (##fx< val ##max-char)
                      (or (##fx< val #xd800)
                          (##fx< #xdfff val)))
               (##integer->char val)
               ;; invalid unicode point; use utf8 replacement instead of bombing
               #\xfffd)))))))

  (def (hex-value char)
    (cond
     ((hash-get hextab char) => values)
     (else
      (raise-invalid-token port char))))

  (read-char port)
  (let lp ((chars []))
    (let (char (read-char port))
      (case char
        ((#\")
         (list->string (reverse! chars)))
        ((#\\)
         (lp (cons (read-escape-char port) chars)))
        (else
         (if (eof-object? char)
           (raise-invalid-token port char)
           (lp (cons char chars))))))))

(def (read-json-number port)
  ;; descend parsing terminals: #\] #\} #\, whitespace
  ;; read until a terminal is encountered and let string->number
  ;; parse it liberally
  (def (parse chars)
    (let (str (list->string (reverse! chars)))
      (or (string->number str)
          (raise-invalid-token port str))))

  (let lp ((chars [(read-char port)]))
    (let (char (peek-char port))
      (if (or (eof-object? char)
              (memq char '(#\] #\} #\,))
              (char-whitespace? char))
        (parse chars)
        (lp (cons (read-char port) chars))))))

(def (read-json-true port)
  (skip-chars '(#\t #\r #\u #\e) port)
  #t)

(def (read-json-false port)
  (skip-chars '(#\f #\a #\l #\s #\e) port)
  #f)

(def (read-json-null port)
  (skip-chars '(#\n #\u #\l #\l) port)
  #!void)

(def (write-json-object obj port)
  (cond
   ((number? obj)
    (cond
     ((and (exact? obj) (integer? obj))
      (write-string (number->string obj) port))
     ((inexact? obj)
      (write-json-inexact obj port))
     ((rational? obj)
      (write-json-inexact (exact->inexact obj) port))
     (else
      (error "Bad JSON object" obj))))
   ((string? obj)
    (write-json-string obj port))
   ((symbol? obj)
    (write-json-string (symbol->string obj) port))
   ((keyword? obj)
    (write-json-string (keyword->string obj) port))
   ((alist? obj)
    (write-json-alist obj port))
   ((list? obj)
    (write-json-list obj port))
   ((vector? obj)
    (write-json-vector obj port))
   ((hash-table? obj)
    (write-json-hash obj port))
   ((eq? #t obj)
    (write-string "true" port))
   ((eq? #f obj)
    (write-string "false" port))
   ((void? obj)
    (write-string "null" port))
   (else
    (write-json-object {:json obj} port))))

(def (write-json-inexact obj port)
  (let* ((mag (abs obj))
         (str (number->string mag)))
    (when (flnegative? obj)
      (write-char #\- port))
    (when (eq? (string-ref str 0) #\.)
      (write-char #\0 port))
    (write-string str port)
    (when (eq? (string-ref str (##fx- (string-length str) 1)) #\.)
      (write-char #\0 port))))

(def (write-json-alist obj port)
  (write-char #\{ port)
  (let lp ((alist obj))
    (match alist
      ([[key . val]]                  ; last one
       (write (string-e key) port)
       (write-char #\: port)
       (write-json-object val port)
       (write-char #\} port))
      ([[key . val] . rest]
       (write (string-e key) port)
       (write-char #\: port)
       (write-json-object val port)
       (write-char #\, port)
       (lp rest))
      ([]                             ; empty
       (write-char #\} port)))))

(def (write-json-list obj port)
  (write-char #\[ port)
  (let lp ((rest obj))
    (match rest
      ([val]                            ; last one
       (write-json-object val port)
       (write-char #\] port))
      ([val . rest]
       (write-json-object val port)
       (write-char #\, port)
       (lp rest))
      ([]                               ; empty
       (write-char #\] port)))))

(def (write-json-vector obj port)
  (let (len (vector-length obj))
    (if (##fxpositive? len)
      (let (last (##fx- len 1))
        (begin
          (write-char #\[ port)
          (let lp ((n 0))
            (if (##fx= n last)
              (begin
                (write-json-object (##vector-ref obj n) port)
                (write-char #\] port))
              (begin
                (write-json-object (##vector-ref obj n) port)
                (write-char #\, port)
                (lp (##fx+ n 1)))))))
      (write-string "[]" port))))

(def (write-json-hash obj port)
  (let (lst (hash->list obj))
    (write-json-object lst port)))

(def (write-json-string obj port)
  (def escape
    '((#\" . #\")
      (#\\ . #\\)
      (#\backspace . #\b)
      (#\newline . #\n)
      (#\page . #\f)
      (#\return . #\r)
      (#\tab . #\t)))

  (def (safe-char? char)
    (let (n (char->integer char))
      (and (##fx>= n 32) (##fx< n 127))))

  (def (write-uchar char port)
    (let (int (char->integer char))
      (write-string "\\u" port)
      (let lp ((n 0) (mask #xf000) (shift -12))
        (when (##fx< n 4)
          (let (char (string-ref hexes (arithmetic-shift (bitwise-and int mask) shift)))
            (write-char char port)
            (lp (##fx+ n 1) (arithmetic-shift mask -4) (##fx+ shift 4)))))))

  (def (write-str obj port)
    (let (len (string-length obj))
      (let lp ((n 0))
        (when (##fx< n len)
          (let (char (string-ref obj n))
            (cond
             ((assq char escape)
              => (lambda (esc)
                   (write-char #\\ port)
                   (write-char (cdr esc) port)))
             ((safe-char? char)
              (write-char char port))
             (else
              (write-uchar char port)))
            (lp (##fx+ n 1)))))))

  (write-char #\" port)
  (write-str obj port)
  (write-char #\" port))
