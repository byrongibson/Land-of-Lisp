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
(defparameter *player-pos* nil)
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

(defun find-islands (nodes edge-list)
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
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
		(edge-list (connect-all-islands nodes (make-edge-list)))
		(cops (remove-if-not (lambda (x) (declare (ignore x)) (zerop (random *cop-odds*))) edge-list)))
  (add-cops (edges-to-alist edge-list) cops)))

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


;; build city nodes; may contain wumpus or glow-worm gangs, or clues like blood, glow-lights, sirens
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
	  (some (lambda (x) (declare (ignore x))
			  (within-one a b edge-alist))
			(neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
		(glow-worms (loop for i below *worm-num* collect (random-node))))
	(loop for n from 1 to *node-num* 
		  collect (append (list n)
							 (cond ((eql n wumpus) '(wumpus))
								   ((within-two n wumpus edge-alist) '(blood!)))
							 (cond ((member n glow-worms) '(glow-worm!))
								   ((some (lambda (worm)
											(within-one n worm edge-alist))
										  glow-worms)
									'(lights)))
							 (when (some #'cdr (cdr (n edge-alist))) '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
	(if (cdr (assoc x *congestion-city-nodes*))
	  (find-empty-node)
	  x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


