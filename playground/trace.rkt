; This file contains several useful debug utils, probably will be filtered out from git history!

#lang racket

(provide trace-to-file show-file-trace multiplex-output-port trace-port)

; Appends contents of the trace file with the given expression.
; Can be then viewed with show-file-trace.
; Contents will be written to /tmp directory.
(define (trace-to-file v file)
  (let ([path (trace-path file)])
    (if (file-exists? path)
        (void)
        (void))
    (display-to-file v path #:exists 'append)
    (display-to-file "\n" path #:exists 'append)))

; Shows contents of the file to a current-output-port and removes it.
(define (show-file-trace file)
  (let ([path (trace-path file)])
    (display (format "-----------------~a-----------------\n" file))
    (if (file-exists? path)
      (begin
        (display (file->string path))
        (delete-file path))
      (void))))

; Returns output port that may be used to collect debug information. 
; Can be then viewed with show-file-trace.
(define (trace-port file) (open-output-file (trace-path file) #:exists 'append))

; Produces output port duplicating data into the given ports.
(define (multiplex-output-port output-port1 output-port2)
  (let ([write-impl (λ (s non-block? port)
                      (if non-block?
                          (write-bytes-avail* s port)
                          (begin
                            (display s port)
                            (bytes-length s))))])
    (make-output-port
     'null
     always-evt
     (λ (s start end non-block? breakable?)
       (begin
         (write-impl s non-block? output-port1)
         (write-impl s non-block? output-port2)))
     (λ ()
       (begin
         (close-output-port output-port1)
         (close-output-port output-port2))))))

; ---------------------- Private section ----------------------

(define (trace-path file) (build-path (find-system-path 'temp-dir) file))
