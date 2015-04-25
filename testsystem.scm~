(load "jow5_project3.scm")

(define runtests
    (lambda (directory current count)
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
                                  (interpret (string-append directory "/test" (number->string current) ".lang")))
                                  "\n"))) (runtests directory (+ current 1) count)))))

(define runfntests
    (lambda (current count)
       (runtests "fntests" current count)))