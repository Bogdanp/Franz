#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide
 protect-data
 unprotect-data)

(define (protect-data bs)
  (define in (make-blob bs))
  (define out (make-blob))
  (unless (CryptProtectData in "" #f #f #f 0 out)
    (error 'protect-data "CryptProtectData error"))
  (begin0 (get-blob-bytes out)
    (LocalFree (DATA_BLOB-data out))))

(define (unprotect-data bs)
  (define in (make-blob bs))
  (define out (make-blob))
  (unless (CryptUnprotectData in #f #f #f #f 0 out)
    (error 'unprotect-data "CryptUnprotectData error"))
  (begin0 (get-blob-bytes out)
    (LocalFree (DATA_BLOB-data out))))

(define (make-blob [bs #""])
  (make-DATA_BLOB
   (bytes-length bs)
   (cast bs _bytes (_cpointer _byte))))

(define (get-blob-bytes b)
  (list->bytes
   (ptr-ref
    (DATA_BLOB-data b)
    (_array/list _byte (DATA_BLOB-len b)))))

(module+ test
  (unless (equal? (unprotect-data (protect-data #"hello")) #"hello")
    (error 'roundtrip)))


;; kernel32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ffi-definer define-kernel32
  (ffi-lib "kernel32"))

(define-kernel32 LocalFree
  (_fun _pointer -> _pointer))


;; crypt32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ffi-definer define-crypt32
  (ffi-lib "crypt32"))

;; https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/aa381414(v=vs.85)
(define-cstruct _DATA_BLOB
  ([len  _long]
   [data (_cpointer _byte)]))

;; https://learn.microsoft.com/en-us/windows/win32/api/dpapi/nf-dpapi-cryptprotectdata
(define-crypt32 CryptProtectData
  (_fun _DATA_BLOB-pointer      ;; *pDataIn
        _string                 ;; szDataDescr
        _DATA_BLOB-pointer/null ;; *pOptionalEntropy
        _pointer                ;; pvReserved
        _pointer                ;; *pPromptStruct
        _long                   ;; dwFlags
        _DATA_BLOB-pointer      ;; *pDataOut
        ->
        _bool))

;; https://learn.microsoft.com/en-us/windows/win32/api/dpapi/nf-dpapi-cryptunprotectdata
(define-crypt32 CryptUnprotectData
  (_fun _DATA_BLOB-pointer       ;; *pDataIn
        (_cpointer/null _string) ;; *ppszDataDescr
        _DATA_BLOB-pointer/null  ;; *pOptionalEntropy
        _pointer                 ;; pvReserved
        _pointer                 ;; *pPromptStruct
        _long                    ;; dwFlags
        _DATA_BLOB-pointer       ;; *pDataOut
        ->
        _bool))
