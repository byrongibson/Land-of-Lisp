;; Graph visualization lib

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

(defparameter *max-label-length* 30)

;; test params
(defparameter *wizard-nodes* '((man-cave (you are in the man-cave. a wizard is snoring loudly on the couch.))
							   (cemetary (you are in a beautiful cemetary. there is a well in front of you.))
							   (tower-dungeon (you are in the tower dungeon. there is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((man-cave (cemetary west door)
											(tower-dungeon upstairs ladder))
							   (cemetary (man-cave east door))
							   (tower-dungeon (man-cave downstairs ladder))))

;;;;;;;;;;;;;;;;
;;; Functions ;;
;;;;;;;;;;;;;;;;

;; Remove DOT-invalid chars from node 
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; Create text label for node
(defun dot-label (exp)
  (if exp
	(let ((s (write-to-string exp :pretty nil)))
	  (if (> (length s) *max-label-length*)
		(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") 
		s))
	""))

;; Encode nodes into DOT format
(defun nodes->dot (nodes)
  (mapc (lambda (node)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "[label=\"")
		  (princ (dot-label node))
		  (princ "\"];"))
		nodes))

;;;;;;;;;;;;;;;;;;;;
;; Directed Graph ;;

;; Encode edges into DOT format
(defun dedges->dot (edges)
  (mapc (lambda (node)
			   (mapc (lambda (edge)
					   (fresh-line)
					   (princ (dot-name (car node)))
					   (princ "->")
					   (princ (dot-name (car edge)))
					   (princ "[label=\"")
					   (princ (dot-label (cdr edge)))
					   (princ "\"];"))
					 (cdr node)))
		edges))

;; Combine encoded nodes and edges into DOT format 
(defun dgraph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (dedges->dot edges)
  (princ "}"))

;; Write dot data to .dot and .png
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
					(concatenate 'string fname ".dot") 
					:direction :output
					:if-exists :supersede)
	(funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O" fname ".dot")))

;; wrapper 
(defun dgraph->png (fname nodes edges)
  (dot->png fname 
			(lambda () 
			  (dgraph->dot nodes edges))))

;test
;(defun dgraph-png (fname nodes edges)
;  (dot-png fname (graph->dot nodes edges)))

;;;;;;;;;;;;;;;;;;;;;;
;; Undirected Graph ;;

(defun uedges->dot (edges)
  (maplist (lambda (lst)
		   (mapc (lambda (edge)
				   (unless (assoc (car edge) (cdr lst))
					 (fresh-line)
					 (princ (dot-name (caar lst)))
					 (princ "--")
					 (princ (dot-name (car edge)))
					 (princ "[label=\"")
					 (princ (dot-label (cdr edge)))
					 (princ "\"];")))
				 (cdar lst)))
		   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot-png fname 
		   (lambda ()
			 (ugraph->dot nodes edges))))


