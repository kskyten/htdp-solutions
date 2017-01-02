;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex73) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct editor (pre post))
; Editor = (make-editor String String)
; interp. (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t

; edit: Editor KeyEvent -> Editor
; Change the state of the editor ed based on the key event ke

; adding text
(check-expect (edit (make-editor "" "") "h") (make-editor "h" ""))
(check-expect (edit (make-editor "hel" "o") "l") (make-editor "hell" "o"))
(check-expect (edit (make-editor "hell" "") "o") (make-editor "hello" ""))
(check-expect (edit (make-editor "hello" "") " ") (make-editor "hello " ""))

; special keys
(check-expect (edit (make-editor "hello" "") "\b") (make-editor "hell" ""))
(check-expect (edit (make-editor "hel" "lo") "\b") (make-editor "he" "lo"))
(check-expect (edit (make-editor "" "hello") "\b") (make-editor "" "hello"))
(check-expect (edit (make-editor "" "") "\b") (make-editor "" ""))

; ignored keys
(check-expect (edit (make-editor "hello" "") "\t") (make-editor "hello" ""))
(check-expect (edit (make-editor "hello" "") "shift") (make-editor "hello" ""))
(check-expect (edit (make-editor "hel" "lo") "\u007F") (make-editor "hel" "lo"))

; movement
(check-expect (edit (make-editor "hell" "") "left") (make-editor "hel" "l"))
(check-expect (edit (make-editor "hel" "lo") "left") (make-editor "he" "llo"))
(check-expect (edit (make-editor "" "hello") "left") (make-editor "" "hello"))
(check-expect (edit (make-editor "" "hello") "right") (make-editor "h" "ello"))
(check-expect (edit (make-editor "hel" "lo") "right") (make-editor "hell" "o"))
(check-expect (edit (make-editor "hello" "") "right") (make-editor "hello" ""))

(define (edit ed ke)
  (cond
    [(valid? ke) (add-char ed ke)]
    [(key=? ke "left") (move-left ed)]
    [(key=? ke "right") (move-right ed)]
    [(key=? ke "\b") (del-char ed)]
    [else ed]
    ))

; valid?: KeyEvent -> Boolean
; Check that input is a valid character

(check-expect (valid? "a") true)
(check-expect (valid? " ") true)
(check-expect (valid? "\b") false)
(check-expect (valid? "\t") false)
(check-expect (valid? "\r") false)
(check-expect (valid? "shift") false)
(check-expect (valid? "\u007F") false)

(define (valid? ke)
  (cond
    [(key=? "\r" ke) false]
    [(key=? "\t" ke) false]
    [(key=? "\b" ke) false]
    [(key=? "\u007F" ke) false]
    [(and (string? ke)
          (= (string-length ke) 1)) true]
    [else false]))

; add-char: Editor KeyEvent -> Editor
; add a character to the left of cursor

(check-expect (add-char (make-editor "" "") "h") (make-editor "h" ""))
(check-expect (add-char (make-editor "hell" "") "o") (make-editor "hello" ""))
(check-expect (add-char (make-editor "hello" "") " ") (make-editor "hello " ""))

(define (add-char ed ke)
  (make-editor
   (string-append (editor-pre ed) ke)
   (editor-post ed)))

; move-left: Editor -> Editor
; move cursor to the left

(check-expect (move-left (make-editor "hell" "")) (make-editor "hel" "l"))
(check-expect (move-left (make-editor "hel" "lo")) (make-editor "he" "llo"))
(check-expect (move-left (make-editor "" "hello")) (make-editor "" "hello"))

(define (move-left ed)
  (make-editor
   (string-remove-last (editor-pre ed))
   (string-append (string-last (editor-pre ed))
                  (editor-post ed))))

; move-right: Editor -> Editor
; move cursor to the right

(check-expect (move-right (make-editor "" "hello")) (make-editor "h" "ello"))
(check-expect (move-right (make-editor "hel" "lo")) (make-editor "hell" "o"))
(check-expect (move-right (make-editor "hello" "")) (make-editor "hello" ""))

(define (move-right ed)
  (make-editor
   (string-append (editor-pre ed) (string-first (editor-post ed)))
   (string-rest (editor-post ed))
   ))

; del-char: Editor -> Editor
; delete the character on the left of the cursor

(check-expect (del-char (make-editor "hello" "")) (make-editor "hell" ""))
(check-expect (del-char (make-editor "hel" "lo")) (make-editor "he" "lo"))
(check-expect (del-char (make-editor "" "hello")) (make-editor "" "hello"))
(check-expect (del-char (make-editor "" "")) (make-editor "" ""))

(define (del-char ed)
  (make-editor
   (string-remove-last (editor-pre ed))
   (editor-post ed)
   ))

; string-first: String -> String
; Extract first character from str

(check-expect (string-first "string") "s")
(check-expect (string-first "test") "t")
(check-expect (string-first "") "")

(define (string-first str)
  (if
   (emptystring? str)
   str
   (substring str 0 1)))

; string-last: String -> String
; Extract last character from str

(check-expect (string-last "string") "g")
(check-expect (string-last "") "")

(define (string-last str)
  (if (emptystring? str)
      str
      (substring str
                 (- 
                  (string-length str) 1))))

; string-rest: String -> String
; Return str with the first character removed

(check-expect (string-rest "string") "tring")
(check-expect (string-rest "") "")

(define (string-rest str)
  (if (emptystring? str)
      str
      (substring str 1)))

; string-remove-last: String -> String
; Return str with the last character removed

(check-expect (string-remove-last "string") "strin")
(check-expect (string-remove-last "") "")

(define (string-remove-last str)
  (if (emptystring? str)
      str
      (substring str 
                 0 
                 (- (string-length str) 1))))

; emptystring?: String -> Boolean
; Check if string is empty

(check-expect (emptystring? "") true)
(check-expect (emptystring? "foo") false)

(define (emptystring? str)
  (= (string-length str) 0))