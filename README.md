# Local Search Techniques
This implementation project evaluated the ability of local search with metaheuristics, simulated annealing and a genetic algorithm, to optimize solutions for specific problem domains. Domain-independent search algorithms were implemented used lisp, then were tested for accuracy and performance. The results indicate the genetic algorithm implementation often finds a more optimal solution than simulated annealing, but with a much greater computational cost. Simulated annealing can provide high-quality solutions with much less time and computation. Nevertheless, further testing must be completed to better understand the differences between these search implementations.  
# Local Search 
Local search algorithms, unlike agent-based search, focus on finding a solution to a problem without regards to a path. Local search algorithms optimize states of a problem by modifying old states with some action to create new states which can be evaluated using an objective function. 

Steepest ascent hill-climbing algorithms look for any state better than the current state. However, an inability to choose a state less optimal than the current state can often lead to a suboptimal solution. If the search algorithm did not explore suboptimal states, an optimal solution would not be found. For this reason, a simple hill-climbing algorithm is incomplete. 

# Inspiration from Interdisciplinary Fields
Simulated annealing and genetic algorithms use techniques from “statistical physics and evolutionary biology”, respectively, to strategically allow suboptimal states to be chosen as a successor to the current state. This implementation project is an exploration of these local searches inspired by interdisciplinary fields in efforts to optimise a solution to problems where the path taken to reach the solution is irrelevant. This project explores similarities and differences between the algorithms and compares their performance in two different domains. 

# Genetic Algorithms
Genetic algorithms were inspired by evolutionary biology, beginning with a set of states from which new states are selectively bred from the population. Genetic algorithms can be used to generate excellent solutions to optimization problems through the process of reproduction, crossover, and mutation (Mitchell 2). As new individuals are selected to replace the original population, fitness of the overall population increases. Nevertheless, genetic algorithms can have some disadvantages. Genetic algorithms must evaluate the fitness of individuals frequently, and performance largely depends on the efficiency of the objective function. Moreover, genetic algorithms do not “scale up” well, leading to an exponential growth in runtime. Additionally, genetic algorithms cannot solve any problem without an objective function. 

# Simulated Annealing 
The simulated annealing algorithm is an effort to optimise hill-climbing algorithms using methods derived from statistical physics. Simulated annealing attempts to optimize an approximate solution in a large search space. Annealing “is the process used to temper or harden metals and glass by heating them to a high temperature and then gradually cooling them, thus allowing the material to reach a low-energy crystalline state” (Russell, 125). Simulated annealing allows states with a lesser evaluated fitness to be chosen over a predecessor. It performs better than hill-climbing because it allows for the selection of suboptimal states at a decreasing rate of frequency. 

# The 8-Queens Problem
The 8-queens problem involves placing eight queens on a chessboard with no queen attacking another. The puzzle can be computationally demanding, as there are 4,426,165,368 possible arrangements of eight queens on a standard chessboard, but only 92 solutions. 

The problem can be solved using a hill-climbing algorithm, but “steepest-ascent hill climbing gets stuck 86% of the time, solving only 14% of problem instances” (Russell, 123). For this reason, the local searches must allow for the selection of suboptimal states at least part of the time. Both simulated annealing and genetic algorithms serve as good approaches to the problem because each allows the propagation of suboptimal states with some frequency. Additionally, the algorithms change and evaluate the current state, rather than evaluating paths to reach a solution. For the 8-queens problem, “what matters is the final configuration of queens, not the order in which they are added” (Russell, 120). 

# The Traveling Salesperson Problem
The traveling salesperson problem involves finding the shortest path between a certain number of cities given the length between each city. The problem is NP hard, and like the 8-queens problem can be very computationally demanding. For example, the evaluation of 20 routes would require 2,432,902,008,176,640,000 paths to be checked. 

Both simulated annealing and a genetic algorithm serve as good approaches to the traveling salesperson problem because an optimal solution is unknown at runtime. Therefore, the algorithms require no goal state or evaluation of a path cost to reach a solution. Moreover, the algorithms can often find reasonable solutions to complex problems like the traveling salesperson problem. 

# Lisp
The local search algorithms were implemented using the Lisp programming language in Common Lisp syntax. Lisp is a functional and interpreted language, good for rapid prototyping. Other languages such as Python were also considered for this project. 

# Pseudocode for a Genetic Algorithm (Russell, 129)
function GENETIC-ALGORITHM (population,FITNESS-FN) returns an individual
 	inputs: population, a set of individuals
    	 FITNESS-FN, a function that measures the fitness of an individual

 	repeat
   		new_population ← empty set
   for i = 1 to SIZE(population) do
     x ← RANDOM-SELECTION(population,FITNESS-FN)
     y ← RANDOM-SELECTION(population,FITNESS-FN)
     child ← REPRODUCE(x,y)
     if (small random probability) then child ← MUTATE(child)
     add child to new_population
   population ← new_population
 until some individual is fit enough, or enough time has elapsed
 return the best individual in population, according to FITNESS-FN

function REPRODUCE(x, y) returns an individual
 	inputs: x,y, parent individuals

 	n ← LENGTH(x); c ← random number from 1 to n
 	return APPEND(SUBSTRING(x, 1, c),SUBSTRING(y, c+1, n))

# Pseudocode for Simulated Annealing (Russell, 126)

function SIMULATED-ANNEALING(problem,schedule) returns a solution state
inputs: problem, a problem
    	 schedule, a mapping from time to "temperature"

 	current ← MAKE-NODE(problem.INITIAL-STATE)
 	for t = 1 to ∞ do
T ← schedule(t)
   if T = 0 then return current
   next ← a randomly selected successor of current
   ΔE ← next.VALUE - current.VALUE
   if ΔE > 0 then current ← next
   else current ← next only with probability eΔE/T
   
# Methods
Both simulated annealing and a genetic algorithm, closely emulating algorithms described by Stuart J. Russell and Peter Norvig, were implemented with Common Lisp using CLOS. The algorithms are domain-independent and were applied to two problems: the 8-queens problem and the traveling salesperson problem. 

The individual class holds all information necessary to represent candidate solutions for a given problem in a population or a neighboring state in the simulated annealing algorithm. Slots for a current state and the evaluation of the state given a domain-specific fitness function are held. 

The class includes two accessor methods to retrieve the slot values, as computing the fitness for each state repeatedly remains computationally expensive. These accessor methods do not call the fitness functions if the state has not been evaluated, and the user must ensure each individual is evaluated after creation. 
Population
This population class holds names of specific functions for each problem to be used in the domain independent algorithms. Each problem was tuned to achieve best performance during search. A more elaborate discussion of methods to optimize these algorithms is discussed below. 

# A Genetic Algorithm Implementation
The domain-independent genetic algorithm ga is inspired by pseudocode written by Stuart Russell and Peter Norvig (1992). The function is passed domain-dependent functions as specified by the CLOS problem object. Once an initial population is created, parents are selected with a random weighted selection, where the weight factor can be adjusted in the arguments list. The parents undergo a crossover, producing a child which has a chance of mutation, again passed as an argument to the function. Each child is added to the new population and the new population replaces the old population. The search stops when an individual reaches a fitnes passed as an argument to the function. 

# Simulated Annealing Implementation
The simulated annealing algorithm annealing is similar to the hill-climbing algorithm,  but allows the selection of suboptimal moves to avoid the possibility of finding a local maximum in the objective function as opposed to a global one. The current state is modified using a domain-dependent mutation function passed to the getneighbor function. As time passes, errors are less likely to occur because the probability of choosing a suboptimal state is eΔE/T, where ΔE is the difference between state fitness, and T is temperature. When the temperature reaches 0, as determined by the schedule function, the current individual is returned. 

8-Queens Problem
States for the 8-queens problem are represented as a list of eight digits, with each place representing a row (as each queen must occupy a different row for a solution), and each digit representing the column of the chessboard a queen occupies on that row. 

Traveling Salesperson Problem
States for the traveling salesperson problem are represented as a list of cities in the order they will be travelled to. For example, a route 123541would be represented as the list (1 2 3 5 4 1). This configuration allows the total distance of the route to be easily summed. 

Fitness Functions
Each problem has a designated fitness function to evaluate new states. If one were to implement the domain-independent local search algorithms on different domains, one would likely have to write new functions depending on the problem specifications. 

The 8-queens problem uses a fitness function, queensfitness which counts the number of non-attacking pairs of queens on a chessboard. A solution to the problem has 28 pairs of non-attacking queens. For each queen, the function checks if another queen exists in the same row, column, or diagonal. 

The traveling salesman problem determines the total length of a route given the distances between each city using the tspfitness function. The cost of each segment is summed, then the reciprocal of the sum is returned so that shorter paths may have a higher fitness value. Routes with duplicate cities and routes which do not start and end at the same location are given the lowest path cost possible. 

Population Functions
The population functions create a list of a specified size containing individuals for the specified problem, ensuring generated states reflect each of the problems at hand. The queenspop function generates a set of states in which queens can be distributed among rows of the chessboard. The tsppop function returns a set of states containing each city and the same starting and ending city to ensure the generated path is valid. 

Mutation Functions
Domain-specific mutation functions allow a new state to be created from an old one. For the 8-queens problem, this involves randomly changing the column for one of the queens on the chessboard using the mutate function. The traveling salesperson problem uses a swap function which exchanges the locations of two cities within the state, excluding the first and last location in the list to ensure the first and last locations remain consistent.     

Crossover Functions 
When producing a new state as a combination of two parent states, sublists of each state are combined to create a child. The 8-queens problem uses a traditional reproduce function, whereas the traveling salesperson problem uses a customized function tspreproduce which ensures the crossover does not produce a state with duplicate cities. 

Schedules
The schedule function determines the temperature for a given timestep. Many different mathematical functions can be used, but the one provided in the implementation is an exponential function given as an example by Stuart Russell and Peter Norvig. The equation as written is 20e.005t <100. Parameters of the function may be changed to achieve better results. 
