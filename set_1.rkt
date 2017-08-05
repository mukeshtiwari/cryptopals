#lang racket
(require bitsyntax)

;;taken from library
;;https://github.com/racket/racket/blob/master/racket/collects/file/sha1.rkt
;;learn bitsyntax and rewrite this using bit pattern matching

;;hexstring to bytes
(define (hex-string->bytes s)
  (unless (and (string? s) (regexp-match? #px"^([[:xdigit:]]{2})*$" s))
    (raise-argument-error 'hex-string->bytes
                          "(and/c string? #px\"^([[:xdigit:]]{2})*$\")" s))
  
  (define (hex-char->int c)
    (cond ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
          ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
          ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))))
  
  (define bsize (/ (string-length s) 2))
  (define b (make-bytes bsize))
  
  (for ((i (in-range bsize)))
    (define high (hex-char->int (string-ref s (+ i i))))
    (define low  (hex-char->int (string-ref s (+ i i 1))))
    (bytes-set! b i (+ (arithmetic-shift high 4) low)))

  b)

;; bytes to hexstring
(define (bytes->hex-string bstr)
  (let* ([len (bytes-length bstr)]
         [bstr2 (make-bytes (* len 2))]
         [digit
          (lambda (v)
            (if (v . < . 10)
                (+ v (char->integer #\0))
                (+ v (- (char->integer #\a) 10))))])
    (for ([i (in-range len)])
      (let ([c (bytes-ref bstr i)])
        (bytes-set! bstr2 (* 2 i) (digit (arithmetic-shift c -4)))
        (bytes-set! bstr2 (+ (* 2 i) 1) (digit (bitwise-and c #xF)))))
    (bytes->string/latin-1 bstr2)))


;;second challenge

(define (second-challenge fstr sstr)
  (let* ([l (bytes->list (hex-string->bytes fstr))]
         [s (bytes->list (hex-string->bytes sstr))]
         [zipls (map cons l s)])
    (bytes->hex-string (list->bytes (map (lambda (w) (bitwise-xor (car w) (cdr w))) zipls)))))


;;third challenge

(define (bytes-xor fbstr sbstr)
  (let* ([l(bytes->list fbstr)]
        [s (bytes->list sbstr)]
        [zipls (map cons l s)])
    (list->bytes (map (lambda (w) (bitwise-xor (car w) (cdr w))) zipls))))
      
(define (third-challenge str)
  (let* ([l (hex-string->bytes str)]
         [s (map (lambda (v) (make-bytes (bytes-length l) v)) (range 0 255))])
    (map (lambda (v) (bytes-xor l v)) s)))

;; #"Cooking MC's like a pound of bacon" 