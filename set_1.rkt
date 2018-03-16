#lang racket
(require bitsyntax
         redex)

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


;; use bitsyntax to convert bytes into base64 string
;; first challenge

(define (first-challenge byte-list)
  #t)

;;second challenge
(define (second-challenge fstr sstr)
  (let* ([l (bytes->list (hex-string->bytes fstr))]
         [s (bytes->list (hex-string->bytes sstr))]
         [zipls (map cons l s)])
    (bytes->hex-string (list->bytes (map (lambda (w) (bitwise-xor (car w) (cdr w))) zipls)))))

(second-challenge "1c0111001f010100061a024b53535009181c"
                  "686974207468652062756c6c277320657965")

;;third challenge
(define freqs (make-immutable-hash 
               '((#"a" . 0.0651738)
                 (#"b" . 0.0124248)
                 (#"c" . 0.0217339)
                 (#"d" . 0.0349835)
                 (#"e" . 0.1041442)
                 (#"f" . 0.0197881)
                 (#"g" . 0.0158610)
                 (#"h" . 0.0492888)
                 (#"i" . 0.0558094)
                 (#"j" . 0.0009033)
                 (#"k" . 0.0050529)
                 (#"l" . 0.0331490)
                 (#"m" . 0.0202124)
                 (#"n" . 0.0564513)
                 (#"o" . 0.0596302)
                 (#"p" . 0.0137645)
                 (#"q" . 0.0008606)
                 (#"r" . 0.0497563)
                 (#"s" . 0.0515760)
                 (#"t" . 0.0729357)
                 (#"u" . 0.0225134)
                 (#"v" . 0.0082903)
                 (#"w" . 0.0171272)
                 (#"x" . 0.0013692)
                 (#"y" . 0.0145984)
                 (#"z" . 0.0007836)
                 (#"A" . 0.0651738)
                 (#"B" . 0.0124248)
                 (#"C" . 0.0217339)
                 (#"D" . 0.0349835)
                 (#"E" . 0.1041442)
                 (#"F" . 0.0197881)
                 (#"G" . 0.0158610)
                 (#"H" . 0.0492888)
                 (#"I" . 0.0558094)
                 (#"J" . 0.0009033)
                 (#"K" . 0.0050529)
                 (#"L" . 0.0331490)
                 (#"M" . 0.0202124)
                 (#"N" . 0.0564513)
                 (#"O" . 0.0596302)
                 (#"P" . 0.0137645)
                 (#"Q" . 0.0008606)
                 (#"R" . 0.0497563)
                 (#"S" . 0.0515760)
                 (#"T" . 0.0729357)
                 (#"U" . 0.0225134)
                 (#"V" . 0.0082903)
                 (#"W" . 0.0171272)
                 (#"X" . 0.0013692)
                 (#"Y" . 0.0145984)
                 (#"Z" . 0.0007836)
                 (#" " . 0.1918182))))
                 

(define (score-bytes byts)
  (apply + (map (lambda (b) (hash-ref freqs (make-bytes 1 b) 0))
                (bytes->list byts))))
                 
(define (bytes-xor fbstr sbstr)
  (let* ([l (bytes->list fbstr)]
         [s (bytes->list sbstr)]
         [zipls (map cons l s)])
    (list->bytes (map (lambda (w) (bitwise-xor (car w) (cdr w))) zipls))))

(define (third-challenge str)
  (let* ([l (hex-string->bytes str)]
         [s (map (lambda (v) (make-bytes (bytes-length l) v)) (range 0 255))]
         [t (map (lambda (v) (let* ([blv (bytes-xor l v)]
                                    [slv (score-bytes blv)])
                   (cons (cons v blv) slv))) s)])
    (argmax cdr t)))


(third-challenge "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
;; #"Cooking MC's like a pound of bacon"

;; fourth challenge
(define (fourth-challenge file)
  (argmax cdr (map third-challenge (file->lines file))))

(fourth-challenge "4.txt")