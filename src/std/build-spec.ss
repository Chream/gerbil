;; -*- Gerbil -*-

(def ldflags (env-ldflags))
(def cppflags (env-cppflags))

(def build-spec
  `("build-config"
    "format"
    "pregexp"
    "sort"
    "sugar"
    "make"
    "error"
    "getopt"
    "logger"
    "generic"
    (gxc: "event" ,@(include-gambit-sharp))
    "coroutine"
    "iter"
    "test"
    ;; SRFI: This is my grandfather's axe; my father replaced the head
    ;;       and i have replaced the handle.
    "srfi/srfi-support"
    "srfi/1"
    "srfi/8"
    "srfi/9"
    "srfi/14"
    (gxc: "srfi/13" "-cc-options" "--param max-gcse-memory=300000000")
    "srfi/19"
    "srfi/41"
    (gxc: "srfi/42" "-cc-options" "--param max-gcse-memory=300000000")
    "srfi/43"
    "srfi/78"
    "srfi/95"
    ;; :std/parser
    "parser/rlang"
    "parser/base"
    (gxc: "parser/stream" ,@(include-gambit-sharp))
    "parser/lexer"
    (gxc: "parser/defparser" "-cc-options" "--param max-gcse-memory=300000000")
    (gxc: "parser/rx-parser" "-cc-options" "--param max-gcse-memory=300000000")
    "parser/deflexer"
    "parser/grammar-reader"
    "parser/grammar"
    "parser"
    ;; :std/text
    "text/utf8"
    "text/hex"
    (gsc: "text/base64")
    (ssi: "text/base64")
    "text/csv"
    "text/json"
    ,@(if config-enable-libyaml
        `((gsc: "text/libyaml"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-lyaml"))
          (ssi: "text/libyaml")
          "text/yaml")
        '())
    ,@(if config-enable-zlib
        `((gsc: "text/_zlib"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-lz"))
          (ssi: "text/_zlib")
          "text/zlib")
        '())
    ;; :std/net
    "net/address"
    "net/uri"
    "net/socks"
    "net/request"
    "net/websocket"
    "net/wamp"
    (gxc: "net/repl" ,@(include-gambit-sharp))
    ;; std/os
    (gxc: "os/error" ,@(include-gambit-sharp))
    (gxc: "os/fd" ,@(include-gambit-sharp))
    (gxc: "os/fdio" ,@(include-gambit-sharp))
    (gxc: "os/fcntl" ,@(include-gambit-sharp))
    (gxc: "os/pipe" ,@(include-gambit-sharp))
    (gsc: "os/_socket" ,@(include-gambit-sharp))
    (ssi: "os/_socket")
    "os/socket"
    ,@(cond-expand
        (linux
         `((gxc: "os/epoll" ,@(include-gambit-sharp))
           (gxc: "os/inotify" ,@(include-gambit-sharp))))
        (else '()))
    ;; :std/net/bio
    "net/bio/input"
    "net/bio/output"
    "net/bio/buffer"
    "net/bio"
    ;; :std/net/socket
    "net/socket/base"
    "net/socket/basic-socket"
    "net/socket/api"
    "net/socket/buffer"
    "net/socket/basic-server"
    ,@(cond-expand
        (linux
         '("net/socket/epoll-server"))
        (else '()))
    "net/socket/server"
    "net/socket"
    ;; :std/net/httpd
    "net/httpd/mux"
    "net/httpd/handler"
    "net/httpd/server"
    "net/httpd"
    ;; :std/xml
    ,@(if config-enable-libxml
        `((gsc: "xml/_libxml"
                "-cc-options" ,(shell-config "xml2-config" "--cflags")
                "-ld-options" ,(shell-config "xml2-config" "--libs")
                ,@(include-gambit-sharp))
          (ssi: "xml/_libxml")
          "xml/libxml"
          "xml/sxml"
          (gsc: "xml/sxml-to-xml")
          (ssi: "xml/sxml-to-xml")
          "xml/print"
          "xml")
        '())
    ;; :std/crypto
    (gsc: "crypto/libcrypto"
          "-cc-options" ,(cppflags "")
          "-ld-options" ,(ldflags "-lcrypto")
          ,@(include-gambit-sharp))
    (ssi: "crypto/libcrypto")
    (gxc: "crypto/etc" ,@(include-gambit-sharp))
    "crypto/digest"
    "crypto/cipher"
    "crypto/hmac"
    "crypto/bn"
    "crypto/dh"
    "crypto"
    ;; :std/misc
    "misc/list"
    "misc/rtd"
    "misc/shuffle"
    "misc/uuid"
    "misc/queue"
    "misc/pqueue"
    "misc/string"
    "misc/sync"
    "misc/completion"
    (gxc:  "misc/ports" ,@(include-gambit-sharp))
    (gxc:  "misc/threads" ,@(include-gambit-sharp))
    "misc/process"
    ;; :std/actor
    (gxc: "actor/message" ,@(include-gambit-sharp))
    (gxc: "actor/xdr"  ,@(include-gambit-sharp))
    "actor/proto"
    "actor/proto/message"
    "actor/proto/null"
    "actor/proto/cookie"
    "actor/proto/cipher"
    "actor/rpc"
    "actor"
    "web/fastcgi"
    "web/rack"
    "db/dbi"
    "db/conpool"
    ,@(if config-enable-sqlite
        `((gsc: "db/_sqlite"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-lsqlite3"))
          (ssi: "db/_sqlite")
          "db/sqlite")
        '())
    ,@(if config-enable-mysql
        `((gsc: "db/_mysql"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-lpthread -lmysqlclient"))
          (ssi: "db/_mysql")
          "db/mysql")
        '())
    ,@(if config-enable-lmdb
        `((gsc: "db/_lmdb"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-llmdb"))
          (ssi: "db/_lmdb")
          "db/lmdb")
        '())
    ,@(if config-enable-leveldb
        `((gsc: "db/_leveldb"
                "-cc-options" ,(cppflags "")
                "-ld-options" ,(ldflags "-lleveldb"))
          (ssi: "db/_leveldb")
          "db/leveldb")
        '())
    ))
