;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex319) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct emp (name ssn rate))
; An Employee is (make-emp String String Number)
; The employees name, social security number and pay rate

(define bob (make-emp "Bob" "123-foo" 12.0))
(define mary (make-emp "Mary" "789-baz" 12.0))
(define boss (make-emp "The Boss" "456-bar" 100))
(define empl (list bob mary boss))

(define-struct wr (name hours))
; A WorkRecord is (make-wr String Number)

(define bob-wr (make-wr "Bob" 40))
(define mary-wr (make-wr "Mary" 40))
(define boss-wr (make-wr "The Boss" 40))
(define wrl (list bob-wr mary-wr boss-wr))

(define-struct salary (name wage))
; A Salary is (make-salary String Number)
; interp. The name of the employee and their wage for the week

(define bob-salary (make-salary "Bob" (* 40 12)))
(define mary-salary (make-salary "Mary" (* 40 12)))
(define boss-salary (make-salary "The Boss" (* 40 100)))
(define salaries (list bob-salary mary-salary boss-salary))

; wages: [Employee] [WorkRecord] -> [Salary]
; Compute the salaries for the employees based on their
; work records
; ASSUMPTION the two lists are of equal length and ordered properly

(check-expect (wages empty empty) empty)
(check-expect (wages (list bob) (list bob-wr))
              (list bob-salary))
(check-expect (wages empl wrl)
              salaries)

(define (wages loe lowr)
  (cond
    [(empty? loe) empty]
    [else (cons (weekly-salary (first loe) (first lowr))
                (wages (rest loe) (rest lowr)))]))

; weekly-salary: Employee WorkRecord -> Salary
; Compute the weekly salary for an employee based on the work record.

(check-expect (weekly-salary bob bob-wr) bob-salary)

(define (weekly-salary employee workrecord)
  (make-salary (emp-name employee) (* (emp-rate employee)
                                      (wr-hours workrecord))))