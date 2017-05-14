;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file contains the code for an implementation of a genetic algorithm and a
;;;;; simulated annealing algorithm, along with two problem domains. The program is 
;;;;; written in Common Lisp syntax. 
;;;;; Completed: 05/07/2017
;;;;; Author: Stanley C. Small <stanley.small1@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Class: individual
;;; Slots:  
;;;    - state:  	the current state of the problem
;;;    - fitness: 	an evaluated fitness of the current state based on a fitness-fn
;;; Description:
;;;    	This class holds all information necessary to represent an indidivdual in a 
;;;		population or a neighboring state in the simulated annealing algorithm, both 
;;;		candidate solutions for the given problem. 
;;;

(defclass individual () (
	(state :initform nil :initarg :state :accessor state)
	(fitness :initform nil :initarg :fitness :accessor fitness)))

;;;
;;; Functions: getfitness, getstate
;;; Arguments:  
;;;    - an individual: used to retrive a state or fitness value
;;; Description:
;;;    	These are accessor methods for the individual class. Storing the fitness value
;;;		along with the state drastically reduces computation time. NOTE: the acessor 
;;;		does not compute the fitness fucntion if it is not availible. It must be used
;;;		at discression of the user. 
;;;

(defmethod getfitness ((i individual))
	(slot-value i 'fitness))

(defmethod getstate ((i individual))
	(slot-value i 'state))

;;;
;;; Class: problem
;;; Slots:  
;;;    - fitness-fn: a problem-specific fitness function
;;;    - mutate-fn: a problem-specific fucntion used for mutation
;;;	   - reproduce-fn: a problem-specific fucntion for reproduction in a ga
;;;	   - population-fn: a function used to create an initial population
;;; Description:
;;;    	This class holds specific functions for each problem to be used in the 
;;;		domain independent algorithms. Each problem must be tuned to achieve best
;;;		performance during search. 
;;;

(defclass problem () (
	(fitness-fn :initform nil :initarg :fitness-fn :accessor fitness-fn)
	(mutate-fn :initform nil :initarg :mutate-fn :accessor mutate-fn)
	(reproduce-fn :initform nil :initarg :reproduce-fn :accessor reproduce-fn)
	(population-fn :initform nil :initarg :population-fn :accessor population-fn)))

;;;
;;; Problem: 8queens
;;; Description:
;;;    	The 8-queens problem has a goal of arranging 8 queens on a chessboard in a 
;;;		way so that they do not attack each other. These domain-specific functions 
;;;		are designed to optimise perfomance of the local search algorithms. 
;;;

(setq 8queens (make-instance 'problem 
	:fitness-fn 'queensfitness
	:mutate-fn 'mutate
	:reproduce-fn 'reproduce
	:population-fn 'queenspop))

;;; 
;;; Method: queensfitness
;;; Arguments: 
;;;		- i: an individual
;;;	Description:
;;;		This fucntion counts the number of non-attacking pairs of queens on a chessboard.
;;;		A solution to the problem has 28 pairs of non-attacking queens. 
;;;

(defmethod queensfitness ((i individual))
	(let ((attacked 0))
	(with-slots (state fitness) i
	(loop for i from 0 to 6 do 
		(loop for j from (+ 1 i) to 7 do
			(if (eql (nth i state) (nth j state)) 
				(setq attacked (+ attacked 1)))
			(if (eql (- (nth j state) (nth i state)) (or (- j i) (+ j i)))
				(setq attacked (+ attacked 1)))
			(if (eql (- (nth i state) (nth j state)) (or (- j i) (+ j i)))
				(setq attacked (+ attacked 1)))))
	(setq fitness (- 28 attacked)))))

;;;
;;; Fucntion: queenspop
;;; Arguments: 
;;;		- size: the size of the poplation
;;;		- fitness-fn: a domain-dependent evaluation for the fitness of an individual
;;; Description:
;;;    	This function creates a population of a specified size for the 8queens problem. 
;;;

(defun queenspop (size fitness-fn)
  	(let ((g nil) (p nil))
  	(dotimes (i size)
	    (dotimes (j 8)
	       	(setf g (cons (+ (random 8) 1) g)))
	    (setf p (cons (make-instance 'individual :state g ) p))
		(funcall fitness-fn (car p))
  		(setf g nil)) p))

;;;
;;; Problem: tsp
;;; Description:
;;;    	The traveling salesperson problem is an NP-hard problem with a goal of 
;;;		finding the shortest path between a number of cities with given distances. 
;;;		These domain-specific functions are designed to optimise perfomance of the 
;;;		local search algorithms. 
;;;

(setq tsp (make-instance 'problem 
	:fitness-fn 'tspfitness
	:mutate-fn 'swap
	:reproduce-fn 'tspreproduce
	:population-fn 'tsppop))

;;;
;;; Fucntion: generate tsp
;;; Arguments: 
;;;		- cityquantity: the number of cities
;;;		- mindistance: the minimum number of distance units between cities
;;;		- maxistance: the maximum number of distance units between cities
;;; Description:
;;;    	This function will generate a random TSP problem given the number of cities and 
;;; 	min and max link costs.
;;;

(defun generatetsp (cityquantity mindistance maxdistance)
	(defvar *cities* cityquantity)
	(defvar *tspdist* (loop for i from 1 to (- cityquantity 1) append
		(loop for j from (+ 1 i) to cityquantity collect
			(list i j (+ mindistance (random (- (+ maxdistance 1) mindistance))))))))

;;; 
;;; Method: tspfitness
;;; Arguments: 
;;;		- i: an individual
;;;	Description:
;;;		This fucntion calculates the cost of the current path, but returns the reciprocal so 
;;;		shorter paths are evaluated as more optimal than longer paths. If a path does not begin 
;;;		and end at the same place, then it’s penalized, as it is if a city occurs more than once 
;;;		(other than start and end). NOTE: This fucntion was inspired by Dr. Roy Turner's work. 
;;;

(defmethod tspfitness ((i individual))
	(with-slots (state fitness) i
		(setq total 0)
		(if (eql (first state) (car (last state))) t (setq total most-positive-fixnum))
		(if (duplicates (cdr state)) (setq total most-positive-fixnum))
		(loop for j from 0 to (- (length state) 2) do 
			(setq total (+ total 
				(or (third (car (member (list (nth j state) (nth (+ j 1) state)) *tspdist*
					  :test #'(lambda (target candidate)
						    (or (and 	(eql (car target) (car candidate))
							    		(eql (cadr target) (cadr candidate)))
								(and 	(eql (car target) (cadr candidate))
							     		(eql (cadr target) (car candidate))))))))
		      most-positive-fixnum))))
		(setq fitness (float (/ 1 total)))))

;;;
;;; Fucntion: duplicates
;;; Arguments: 
;;;		- state: a list
;;; Description:
;;;    	This function determines if a list contains duplicates. 
;;;

(defun duplicates (state)
	(cond 	((null state) nil)
			((member (car state) (cdr state)) (cons (car state) (duplicates (cdr state))))
			(t (duplicates (cdr state)))))

;;;
;;; Fucntion: tsppop
;;; Arguments: 
;;;		- size: the size of the poplation
;;;		- fitness-fn: a domain-dependent evaluation for the fitness of an individual
;;; Description:
;;;    	This function creates a population of a specified size for the tsp problem. 
;;;

(defun tsppop (size fitness-fn)
	(let ((g nil) (p nil))
  	(dotimes (i size)
	    (loop for j from 1 to *cities* do 
	    	(setf g (cons j g)))
	    (setf p (cons (make-instance 'individual :state (cons '1 g)) p))
		(funcall fitness-fn (car p))
  		(setf g nil)) p))

;;;
;;; Fucntion: annealing
;;; Arguments: 
;;;		- p: an problem
;;;		- schedule: a schedule for the temperature to cool in the function
;;; Description:
;;;    	The simulated annealing algorithm is similar to the hill-climbing algorithm, 
;;;		aside from the fact that the agorithm allows the selection of sub-optimal moves
;;;		to avoid the possibility of finding a local maximum in favor of a global one. 
;;;		As time passes, errors are less likely to occur. When the temperature reaches 0
;;;		The current individual is returned. 
;;;

(defun annealing ((p problem) schedule)
	(let ((current nil) (next nil) (temperature nil))
	(with-slots (fitness-fn mutate-fn intital-state population-fn) p
	(setf current (car (funcall population-fn 1 fitness-fn)))
	(funcall fitness-fn current)
	(loop for timestep from 1 to most-positive-fixnum do
		(setf temperature (funcall schedule timestep))
		(if (eql 0 temperature) (return current))
		(setf next (getneighbor current mutate-fn))
		(funcall fitness-fn next)
		(setf deltaE (- (getfitness next) (print (getfitness current))))
		(if (> deltaE 0) (setq current next))
			(if (> (random 1.0) (exp (/ deltaE temperature))) (setf current next))))))

;;;
;;; Fucntion: getneighbor
;;; Arguments: 
;;;		- i: an individual
;;;		- mutate-fn: a domain-specific function used to mutate an individual state
;;; Description:
;;;    	This function copies an inidivual, then modifies that individual using a 
;;;		mutate-fn, then returns that individual. 
;;;

(defun getneighbor ((i individual) mutate-fn)
	(let ((neighbor nil))
	(with-slots (state) i
		(setq neighbor (make-instance 'individual :state (copy-list state)))
		(funcall mutate-fn neighbor)
		neighbor)))

;;;
;;; Fucntion: schedule
;;; Description:
;;;    	A sample schedule function from the AIMA website.  
;;;

(defun schedule (timestep)
	(let ((k 20) (lam .005) (lim 100))
	(if (< (* k (exp (* lam timestep))) lim) (* k (exp (* lam timestep))) 0)))

;;;
;;; Function: ga 
;;; Arguments:  
;;;     - p: a problem (either 8queens or tsp)
;;;		- popsize: the size of the population
;;;		- stopfitness: the desired fitness set to stop the search
;;;		- mutationchance: a mutation rate percentage (0 to 100)
;;;		- weight: 	a factor to which fitness plays a role
;;;					(i.e. a higher weight will select fit individuals more often)
;;;	Description: 
;;;		This is a domain-independent implementation of a genetic algorithm inspired 
;;;		by pseudocode written by Stuart Russell and Peter Norvig. The fucntion is
;;;		passed a series of domain-dependent functions as specified by the CLOS problem
;;;		object. Once an initial population is created, parents are selected with a 
;;;		random weighted selection, where the weight factor can be adjusted. The 
;;;		parents undergo a crossover, producing a child which has a chance of mutation.
;;;		Each child is added to the new population and the new-population replaces the
;;;		old population. The search stops when an individual reaches a fitnes passed as
;;;		a parameter to the function. 
;;;

(defun ga ((p problem) popsize stopfitness mutationchance weight)
	(let ((population nil) (new-population nil) (x nil) (y nil) (child nil))
	(with-slots (fitness-fn mutate-fn reproduce-fn population-fn intital-state) p
		(setf population (funcall population-fn popsize fitness-fn))	; new pop
		(loop 
			(setf new-population nil)
			(loop for individual from 0 to (length population) do
				(setf x (randomweighted population fitness-fn weight))	; select
				(setf y (randomweighted population fitness-fn weight))	; parents
				(setf child (funcall reproduce-fn x y))
				(if (< (random 100) mutationchance) 					; mutation
					(funcall mutate-fn child))
				(print (funcall fitness-fn child))						; display
				(setf new-population (cons child new-population))		; fitness
				(setf population new-population))
		(when (> (getfitness child) stopfitness) (return child))))))	; stop cond

;;;
;;; Function: randomweighted
;;; Arguments:  
;;;     - population: a population
;;;     - fitness-fn: a domain-dependent fitness-fn
;;;		- weight: a factor to which the weights will be amplified
;;;	Description: 
;;;		This fucntion randomly selects an individual from a population with a greater
;;;		preference for individuals with a higher fitness value. The factor to which 
;;;		the fitness of an indivudal impacts the choice can be modified by the weight
;;;		argument. The fitness of each individul is summed, then a threshold value is 
;;;		chosen. Indivduals are then added until the threshold is reached, then that 
;;;		individual is returned. 
;;;

(defun randomweighted (population fitness-fn weight)
	(let ((fitnesssum 0) (i 0) (z 0))	
	(loop for i in population do
		(setf fitnesssum (+ (expt (getfitness i) weight) fitnesssum)))
	(setf z (random fitnesssum))										; threshold
		(loop for j in population do
			(setq i (+ (expt (getfitness j) weight) i))
			(if (> i z) (return j)))))

;;;
;;; Method: mutate
;;; Arguments:  
;;;     - i: an individual
;;;	Description: 
;;;		This fucntion modifies the state of an indivdual by randomly replacing one 
;;;		value in the state by a random number up to the length of the state. 
;;;		NOTE: This function does not update the fitness value for the state. 
;;;

(defmethod mutate ((i individual))
	(with-slots (state fitness) i
		(setf (nth (random (length state)) state) (+ (random (length state)) 1)) 
		i))																; individual

;;;
;;; Method: swap
;;; Arguments:  
;;;     - i: an individual
;;;	Description: 
;;;		This fucntion modifies the state of an indivdual by swapping two values. 
;;;		NOTE: This function does not update the fitness value for the state. 
;;;

(defmethod swap ((i individual))
	(with-slots (state fitness) i
		(rotatef	(nth (+ (random (- (length state) 3)) 1) state) 
					(nth (+ (random (- (length state) 3)) 1) state))
		i))																; individual

;;;
;;; Function: reproduce  
;;; Arguments:  
;;;     - x: a parent individual
;;;     - y: a parent individual
;;;	Description: 
;;;		This fucntion takes two indivudals as arguments and performs a crossover 
;;;		between the two genomes. An index is randomly chosen to cut the indivduals,
;;;		the the two states are combined to form a child CLOS individual. 
;;;

(defun reproduce ((x individual) (y individual))
	(let ((n nil) (c nil))												; length &
	(with-slots ((x state)) x											; cut index
	(with-slots ((y state)) y
		(setf n (- (length x) 1))										; end of state
		(setf c (random n))												; cut index
		(make-instance 'individual :state 								; new child
			(append (sublist x 0 c) (sublist y (+ c 1) n)))))))

;;;
;;; Function: sublist  
;;; Arguments:  
;;;     - state: a list describing the state of an individual
;;;		- start: staring index
;;;		- end: ending index
;;;	Description: 
;;;		This fucntion takes the state of an indivudal and creates a sublist when
;;;		provided the starting and ending index. 
;;;

(defun sublist (state start end)
	(loop for i from start to end
		collect (nth i state)))

;;;
;;; Function: tspreproduce  
;;; Arguments:  
;;;     - x: a parent individual
;;;     - y: a parent individual
;;;	Description: 
;;;		This domain-dependent fucntion takes two indivudals as arguments and performs 
;;;		a crossover between the two genomes. A sublist is chosen with random start 
;;;	 	and end indexes. The sublist of the x genome is inserted into the state, with 
;;;		values of y added in a way which ensures no cities are duplicated in the child
;;;		state. 
;;;

(defun tspreproduce ((x individual) (y individual))
	(let ((state nil) (start nil) (end nil) (sublist nil))	
	(with-slots ((x state)) x
	(with-slots ((y state)) y
	(setf start (+ (random (- (length x) 3)) 1))						; ensure 
	(setf end (+ (random (- (- (length x) 2) start)) 1 start))			; first & last
	(setf sublist (sublist x start end))								; remain 
	(loop for i in y do 												; unchanged
		(if (not (member i sublist)) (setq state (cons i state)))
		(if (eql (position i y) start) (setq state (append sublist state))))
	(make-instance 'individual :state state)))))
