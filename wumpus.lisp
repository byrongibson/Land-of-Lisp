;; Grand Theft Wumpus (or, Hunt the Wumpus Ultra Renegades)
;; LoL pg. 130, Chp 8
;;
;; Rules:
;; 1.  The Wumpus is hiding in the city, you have to find and kill him.
;; 2.  The wumpus is stationary, will not move once the game starts.
;; 3.  You have a gun with one bullet.  The Wumpus has a loaded AK-47.
;; 4.  To kill the wumpus, you must charge his location from one node away.  Charging the wrong node uses up your bullet and loses the game.  Walking (not charging) into the wumpus's node = death & game over.
;; 5.  There are cops scattered through the city, running into an edge with cops = arrested & game over
;; 6.  There are glow-worm gangs throughout the city.  Running into one gets you beaten, robbed, and teleported to another random node.  If you land on the node with the wumpus = death & game over (b/c you didn't charge it).
;;

(load "graph-util.lisp")

;; game parameters
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; generate random edge list to connect nodes
(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
	(list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect (edge-pair (random-node) (random-node)))))

;; connect any unconnected islands from the above list 
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
				   (eql (car x) node))
				 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
	(labels ((traverse (node)
					   (unless (member node visited)
						 (push node visited)
						 (mapc (lambda (edge)
								 (traverse (cdr edge)))
							   (direct-edges node edge-list)))))
	  (traverse node))
	visited))

(defun find-islands (node edge-list)
  (let ((islands nil))
	(labels ((find-island (nodes)
						  (let* ((connected (get-connected (car nodes) edge-list))
								 (unconnected (set-difference nodes connected)))
							(push connected islands)
							(when unconnected 
							  (find-island unconnected)))))
	  (find-island nodes))
	islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
	(append (edge-pair (caar islands) (caadr islands))
			(connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; Build final edges: convert from edge list to alist and add cops to random edges
(defun make-city-edges ()
  let* ((nodes (loop for i from 1 to *node-num*
					 collect i))
		(edge-list (connect-all-islands nodes (make-edge-list)))
		(cops (remove-if-not (lambda (x)
							   (zerop (random *cop-odds*)))
							 edge-list)))
  (add-cops (edges-to-alist (edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1) 
		  (cons node1
				(mapcar (lambda (edge)
						  (list (cdr edge)))
						(remove-duplicates (direct-edges node1 edge-list)
										   :test #'equal))))
  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
			(let ((node1 (car x))
				  (node1-edges (cdr x)))
			  (cons node1
					(mapcar (lambda (edge)
							  (let ((node2 (car edge)))
								(if (intersection (edge-pair node1 node2)
												  edges-with-cops
												  :test #'equal)
								  (list node2 'cops)
								  edge)))
							node1-edges))))
		  edge-alist))



