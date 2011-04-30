;;;;;;;;;;;;;;;;;
;; global vars ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;;;; Chapter 1 ;;;;

(defparameter *nodes* '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
						(garden (you are in a beautiful garden.
									 there is a well in front of you.))
						(attic (you are in the attic.  there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
						(attic (living-room downstairs ladder))
						(garden (living-room east door))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))

(defparameter *location* 'living-room)


;;;;;;;;;;;;;;;;;;;
;;;; Chapter 2 ;;;;

(defparameter *allowed-commands* '(look walk pickup inventory))




;;;;;;;;;;;;;;;;;;;
;; function defs ;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;;;; Chapter 1 ;;;;

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
	  (labels ((at-loc-p (obj)
				(eq (cadr (assoc obj obj-locs)) loc)))
		(remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
						 `(you see a ,obj on the floor.)))
	(apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describe-location *location* *nodes*)
		  (describe-paths *location* *edges*)
		  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
	(if next (progn (setf *location* (car next)) (look))
	  '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
		 (push (list object 'body) *object-locations*)
		 `(you are now carrying the ,object))
		(t '(you cannot get that))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;;;;;;;;;;;;;;;;;;
;;;; Chapter 2 ;;;;

;(defun say-hello ()
;  (print "Please type your name: ")
;  (let ((name (read)))
;	(print "Nice to meet you, ")
;	(print name)))

(defun add-five (num)
  (print "Please enter a number: ")
  (let ((num (read)))
	(print "When I add five I get ")
	(print (+ num 5))))

(defun test-princ ()
  (progn (princ "This sentence will be interrupted")
		 (princ #\newline)
		 (princ "by an annoying newline character.")))

(defun say-hello ()
  (princ "Please state your name: ")
  (let ((name (read-line)))
	(princ "Nice to meet you, ")
	(princ name)))


(defun game-read ()
  (let ((cmd (read-from-string
			   (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
					 (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
	(eval sexp)
	'(i do not know that command.)))


(defun tweak-text (1st caps lit)
  (when 1st
	(let ((item (car 1st))
		  (rest (cdr 1st)))
	  (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
			((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
			((eq item '#\") (tweak-text rest caps (not lit)))
			(lit (cons item (tweak-text rest nil lit)))
			((or caps lit) (cons (char-upcase item) (tweak-text rest nil nil)))
			(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (1st)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string 1st))
									 'list)
							 t
							 nil)
				 'string))
  (fresh-line))


;(defun game-repl ()
;  (loop (print (eval (read)))))

(defun game-repl ()
  (let ((cmd (game-read)))
	(unless (eq (car cmd) 'quit)
	  (game-print (game-eval cmd))
	  (game-repl))))

	  



