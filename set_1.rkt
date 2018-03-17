#lang racket
(require bitsyntax
         redex
         net/base64)

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

(define base64 (make-immutable-hash 
               '((0 . "A")
                 (1 . "B")
                 (2 . "C")
                 (3 . "D")
                 (4 . "E")
                 (5 . "F")
                 (6 . "G")
                 (7 . "H")
                 (8 . "I")
                 (9 . "J")
                 (10 . "K")
                 (11 . "L")
                 (12 . "M")
                 (13 . "N")
                 (14 . "O")
                 (15 . "P")
                 (16 . "Q")
                 (17 . "R")
                 (18 . "S")
                 (19 . "T")
                 (20 . "U")
                 (21 . "V")
                 (22 . "W")
                 (23 . "X")
                 (24 . "Y")
                 (25 . "Z")
                 (26 . "a")
                 (27 . "b")
                 (28 . "c")
                 (29 . "d")
                 (30 . "e")
                 (31 . "f")
                 (32 . "g")
                 (33 . "h")
                 (34 . "i")
                 (35 . "j")
                 (36 . "k")
                 (37 . "l")
                 (38 . "m")
                 (39 . "n")
                 (40 . "o")
                 (41 . "p")
                 (42 . "q")
                 (43 . "r")
                 (44 . "s")
                 (45 . "t")
                 (46 . "u")
                 (47 . "v")
                 (48 . "w")
                 (49 . "x")
                 (50 . "y")
                 (51 . "z")
                 (52 . "0")
                 (53 . "1")
                 (54 . "2")
                 (55 . "3")
                 (56 . "4")
                 (57 . "5")
                 (58 . "6")
                 (59 . "7")
                 (60 . "8")
                 (61 . "9")
                 (62 . "+")
                 (63 . "/"))))
                 

;; use bitsyntax to convert bytes into base64 string
;; bytes to Base64 string
(define (base64-encode bytes)
  (bit-string-case bytes
                   ([(f :: bits 6) (rest :: binary)] (string-append (hash-ref base64 f 0) (base64-encode rest)))
                   ([(rest :: bits 0)] "")
                   ([(rest :: bits 2)] (string-append
                                        (hash-ref
                                         base64
                                         (bit-string->unsigned-integer (bit-string (rest :: bits 2) (0 :: bits 4)) #f) 0) "=="))
                   ([(rest :: bits 4)] (string-append
                                        (hash-ref
                                         base64
                                         (bit-string->unsigned-integer (bit-string (rest :: bits 4) (0 :: bits 2)) #f) 0) "="))))
                                        
                   
;;first challenge
(define (first-challenge hexstr)
  (let* ([bystr (hex-string->bytes hexstr)]
         [bastr (base64-encode bystr)])
    bastr))

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
  (apply + (map (lambda (b) (hash-ref freqs (bytes b) 0))
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

(define (string-repeat n str)
  (if (= n 0) str
      (string-append str (string-repeat (- n 1) str))))

(define (fifth-challenge str key )
  (let* ([len (string-length str)]
         [rkey (string-repeat len key)]
         [byte-str (string->bytes/utf-8 str)]
         [byte-key (string->bytes/utf-8 (substring rkey 0 len))])
    (bytes->hex-string (bytes-xor byte-str byte-key))))
                        

(define str "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal")

;This one is bit crazy because there should be no new line in
; output
(fifth-challenge str "ICE")
(string=? (fifth-challenge str "ICE") "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")


