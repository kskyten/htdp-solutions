;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex118) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; planets
(define planets (cons "Mercury"
                      (cons "Venus"
                            (cons "Earth"
                                  (cons "Mars"
                                        (cons "Jupiter"
                                              (cons "Saturn"
                                                    (cons "Uranus"
                                                          (cons "Neptune" empty)))))))))

; foods
(cons "Steak"
      (cons "Hamburger"
            (cons "Cake" empty)))

; colors
(cons "Blue"
      (cons "Red"
            (cons "Green" empty)))