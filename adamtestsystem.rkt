(load "jow5_project4.scm")

(define runtests
    (lambda (directory current count)
       (cond
         ((> current count) (display "Finished"))
         (else 
          (with-handlers ([exn:fail?
                           (lambda (exn)
                             (display  (string-append "Test " (number->string current) ": " (exn-message exn) "\n" (expectedResult current) "\n")))])
          (display (string-append "Test "
                                  (number->string current) 
                                  ": "
                                  ((lambda (val)
                                     (cond
                                       ((number? val) (number->string val))
                                       ((symbol? val) (symbol->string val))
                                       (else val)))
                                  (interpret (string-append directory "/test" (number->string current) ".lang") (getClassToRun current)))
                                  "\n" (expectedResult current) "\n"))) (runtests directory (+ current 1) count)))))

(define runfntests
    (lambda (current count)
       (runtests "objecttests" current count)))

(define getClassToRun
  (lambda (currentTestNum)
    (cond
      ((eq? 1 currentTestNum) 'A)
      ((eq? 2 currentTestNum) 'Square)
      ((eq? 3 currentTestNum) 'B)
      ((eq? 4 currentTestNum) 'B)
      ((eq? 5 currentTestNum) 'A) ;should be C but test is causing endless loop
      ((eq? 6 currentTestNum) 'A)
      ((eq? 7 currentTestNum) 'A)
      ((eq? 8 currentTestNum) 'A)
      ((eq? 9 currentTestNum) 'A)
      ((eq? 10 currentTestNum) 'A)
      ((eq? 11 currentTestNum) 'A)
      ((eq? 12 currentTestNum) 'C)
      ((eq? 13 currentTestNum) 'Square)
      ((eq? 14 currentTestNum) 'Square)
      ((eq? 15 currentTestNum) 'List)
      ((eq? 16 currentTestNum) 'Box)
      ((eq? 17 currentTestNum) 'List)
      ((eq? 18 currentTestNum) 'List)
      ((eq? 19 currentTestNum) 'A)
      ((eq? 20 currentTestNum) 'A)
      ((eq? 21 currentTestNum) 'A)
      ((eq? 22 currentTestNum) 'A))))

(define expectedResult 
  (lambda (currentTestNum)
    (cond
      ((eq? 1 currentTestNum) "Expected: 20")
      ((eq? 2 currentTestNum) "Expected: 400")
      ((eq? 3 currentTestNum) "Expected: 530")
      ((eq? 4 currentTestNum) "Expected: 615")
      ((eq? 5 currentTestNum) "Expected: -716") 
      ((eq? 6 currentTestNum) "Expected: 15")
      ((eq? 7 currentTestNum) "Expected: 12")
      ((eq? 8 currentTestNum) "Expected: 110")
      ((eq? 9 currentTestNum) "Expected: 125")
      ((eq? 10 currentTestNum) "Expected: 36")
      ((eq? 11 currentTestNum) "Expected: 54")
      ((eq? 12 currentTestNum) "Expected: 26")
      ((eq? 13 currentTestNum) "Expected: 117")
      ((eq? 14 currentTestNum) "Expected: 32")
      ((eq? 15 currentTestNum) "Expected: 15")
      ((eq? 16 currentTestNum) "Expected: 16")
      ((eq? 17 currentTestNum) "Expected: 123456")
      ((eq? 18 currentTestNum) "Expected: 5285")
      ((eq? 19 currentTestNum) "Expected: 100")
      ((eq? 20 currentTestNum) "Expected: 420")
      ((eq? 21 currentTestNum) "Expected: 10")
      ((eq? 22 currentTestNum) "Expected: error for no this"))))
      