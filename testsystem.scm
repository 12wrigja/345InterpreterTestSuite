(load "jow5_project1.scm")

(define runtests
    (lambda (current count)
       (cond
         ((> current count) (display "Finished"))
         (else 
          (with-handlers ([exn:fail?
                           (lambda (exn)
                             (display  (string-append "Test " (number->string current) ": " (exn-message exn) "\n")))])
          (display (string-append "Test "
                                  (number->string current) 
                                  ": "
                                  ((lambda (val)
                                     (cond
                                       ((number? val) (number->string val))
                                       ((symbol? val) (symbol->string val))
                                       (else val)))
                                  (interpret (string-append "tests/test" (number->string current) ".lang")))
                                  "\n"))) (runtests (+ current 1) count)))))
