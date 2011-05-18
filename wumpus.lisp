;; Grand Theft Wumpus (or, Hunt the Wumpus Ultra Renegades)
;; LoL pg. 130, Chp 8
;;
;; Rules:
;; 1.  The Wumpus is hiding in the city, you have to find and kill him.
;; 2.  You have a gun with one bullet.  The Wumpus has a loaded AK-47.
;; 3.  The wumpus is stationary, will not move once the game starts.
;; 4.  To kill the wumpus, you must charge his location from one node away.  Charging the wrong node uses up your bullet and loses the game.  Walking (not charging) into the wumpus's node = death & game over.
;; 5.  There are cops scattered through the city, running into an edge with cops = arrested & game over
;; 6.  There are glow-worm gangs throughout the city.  Running into one gets you beaten, robbed, and teleported to another random node. 
;;

(load "graph.util")

;; game parameters
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; random edge list to connect nodes
(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eq (a b))
	list (cons a b) (cons b a)))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect (edge-pair (random-node) (random-node)))))


