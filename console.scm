(declare (standard-bindings)
         (extended-bindings)
         (not safe))

(define (console-log str)
  (##inline-host-expression "console.log(gambit_scm2host(@1@))" str))
(define (console-log-js str)
  (##inline-host-expression "console.log(@1@)" str))



;;----------------------------------------------------------------------------
;; Utility 
;;----------------------------------------------------------------------------
(define (foldr fn acc lst)
  (if (null? lst)
      acc
      (fn (car lst) (foldr fn acc (cdr lst)))))

(define (foldl fn acc lst)
  (if (null? lst)
      acc
      (foldl fn (fn acc (car lst)) (cdr lst))))

(define (append-strings . strs)
  (foldl string-append "" strs)) 

(define-macro (ml-str . str)
  (define (append-strings lst acc)
   (cond
    ((null? lst) acc)
    ((and (pair? lst) (null? (cdr lst)))
     (string-append acc (car lst)))
    (else
     (append-strings
       (cdr lst)
       (string-append acc
                      (string-append (car lst) "\n"))))))
  (append-strings str ""))


;;----------------------------------------------------------------------------
;; Javascript / SVG interface
;;----------------------------------------------------------------------------

(##inline-host-declaration 
"var svg = document.documentElement;
var svgNS = svg.namespaceURI")

(define svg (##inline-host-expression "svg"))

(define (create-element el)
  (##inline-host-expression
   "document.createElementNS(svgNS, @1@)" el))

(define (create-text-node str)
  (##inline-host-expression
   "document.createTextNode(@1@)" str))

(define (set-attribute element attribute value)
  (##inline-host-expression
   "(@1@).setAttribute(@2@, @3@)" element attribute value))

(define (append-child parent child)
  (##inline-host-expression
   "(@1@).appendChild(@2@)" parent child))

(define (set-text-content el text)
  (##inline-host-expression "(@1@).textContent = gambit_scm2host(@2@)" el text))

(define (set-onkeypress fn)
  (##inline-host-expression "document.onkeypress = @1@" fn))

(define (set-onkeydown fn)
  (##inline-host-expression "document.onkeydown = @1@" fn))

(define from-char-code 
  (##inline-host-expression "gambit_host2scm(String.fromCharCode)"))



;;----------------------------------------------------------------------------
;; TTY LOGIC
;;----------------------------------------------------------------------------

(define-type input val prefix update)

(define input-set! input-val-set!)

(define (input-get i) (string-append (input-prefix i) (input-val i)))

(define (input-add! i c)
  (input-val-set! i (string-append (input-val i) c)))

(define (input-pop! i)
  (let ((str (input-val i)))
    (input-val-set! i (substring str 0 (- (string-length str) 1)))))

(define-type tty size history update)

(define (tty-add-line str tty)
  (let ((history (tty-history tty)))
   (tty-history-set! tty (cons str history))))



;;----------------------------------------------------------------------------
;; TTY INTERFACE
;;----------------------------------------------------------------------------

(define (make-tty-line y)
  (define input (create-element "text"))
  (define input-text (create-text-node ""))
  (append-child input input-text)
  (set-attribute input "font-size" 20)
  (set-attribute input "font-family" "Courier")
  (set-attribute input "x" 25)
  (set-attribute input "y" y)
  (append-child svg input)
  input-text)

(define (make-tty-lines n)
  (define tty (make-vector tty-size n))
  (let loop ((m 0))
    (if (< m n)
        (begin
          (vector-set! tty
                       m
                       (make-tty-line (* 25 (+ m 1))))
          (loop (+ m 1)))
        tty)))



;;----------------------------------------------------------------------------
;; TTY START
;;----------------------------------------------------------------------------

(define input (make-input "" ">>> " #f))

(define tty (make-tty 31 '() #f))

(define history-interface (make-tty-lines (tty-size tty)))

(define input-interface (make-tty-line (* 25 (+ (tty-size tty) 1))))

(input-update-set! input
  (lambda ()
    (set-text-content
      input-interface
      (string-append (input-get input)
                     "_"))))

(tty-update-set! tty
  (lambda ()
    (let loop ((i (- (tty-size tty) 1)) (content (tty-history tty)))
       (if (< i 0)
           #!void
           (if (null? content)
               (begin
                (set-text-content (vector-ref history-interface i) "")
                (loop (- i 1) content))
               (begin
                (set-text-content (vector-ref history-interface i)
                                  (car content))
                (loop (- i 1) (cdr content))))))))

((input-update input))



;;----------------------------------------------------------------------------
;; Reader and printer
;;----------------------------------------------------------------------------

(define (tty-println str tty)
  (tty-add-line str tty))

;; note: use primitives for faster loop
(define (tty-print str tty)
  (let ((len (string-length str)))
   (let loop ((i 0) (last-substring 0) (last-space 0))
    (cond

     ((##fx> (##fx- i last-substring) 79)
      (if (eq? (string-ref str i) #\space)
          (begin
           (tty-println (substring str last-substring last-space) tty)
           (loop (##fx+ i 1) (+ last-space 1) last-space))
          (if (##fx> last-space last-substring)
              (begin
               (tty-println (substring str last-substring last-space) tty)
               (loop i (+ last-space 1) last-space))
              (begin
               (tty-println (substring str last-substring i) tty)
               (loop i i last-space)))))

     ((eq? (string-ref str i) #\newline)
      (tty-println (substring str last-substring i) tty)
      (loop (##fx+ i 1) i last-space))

     ((eq? (string-ref str i) #\space)
      (loop (##fx+ i 1) last-substring i))

     ((##fx= len i)
      (tty-println (substring str last-substring i) tty))

     (else
      (loop (##fx+ i 1) last-substring last-space))))))



;;---------------------------------
;; Event handling
;;---------------------------------

; Note: these functions return #f to prevent the browser from
; handling the default key binding.

(define (keypress-handle k)
  (console-log k)
  (cond
   ((= k 13)
    (let ((str (input-val input)))
      (tty-print (input-get input) tty)
      ((tty-update tty))
      (input-val-set! input "")
      ((input-update input))
      (console-log "before")
      (let ((result (eval (string->object str))))
        (console-log "after")
        (if (not (eq? #!void result))
            (tty-print (object->string result) tty)))
        ((tty-update tty))))
   ((= (string-length (from-char-code k)) 1)
    (input-add! input (from-char-code k))
    ((input-update input))))
  #f)

(define (keydown-handle k)
  (case k

   ((40) ; DOWN key
    (console-log "DOWN")
    #f)

   ((38) ; UP key
    (console-log "UP")
    #f)

   ((8)  ; BACKSPACE key
    (if (> (string-length (input-val input)) 0)
        (begin (input-pop! input)
               ((input-update input))))
    #f)

   (else #t)))


(set-onkeypress
  (##inline-host-expression
    "function(e) { return gambit_scmfn2host(@1@)((e || window.event).which); }" 
    keypress-handle))

(set-onkeydown
  (##inline-host-expression
    "function(e) { return gambit_scmfn2host(@1@)((e || window.event).which); }" 
    keydown-handle))


;;
;; My console function
;;
(define (println str)
  (tty-print str tty)
  #!void)

(define (clear)
  (tty-history-set! tty '())
  ((tty-update tty))
  #!void)


(define (help)
 (tty-print
  (ml-str

"This is a Gambit Scheme console based on the Gambit scheme universal"
"librairy (univ-lib)."
"Note: The console is far from feature complete. Errors are currently"
"handled silently. The univ-lib isn't complete which explains why many"
"functions don't work."
""
"Commands"
"========"
" * (clear)"
"   Clears the console history. There is no limit on the history size so"
"   a clear once in a while should prevent the console from using to much"
"   memory."
""
" * (println <string>)"
"   Prints a string to the console."
""
"To do"
"====="
" * Use the univ-lib repl implementation to provide functionalities"
"   similar to the Gambit interpreter"
" * Remove global states so that multiple consoles could be embeded"
"   in a web page"
" * Maybe use html instead of svg..."
" * Add command history (might come with repl functionalities)."
  )
  tty)
 #!void)

(help)
((tty-update tty))
