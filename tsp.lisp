;Copyright (c) 2016 Vidhya

(defstruct city
	x
	y
	city-id
)

(defstruct tour
   	path
	distance
	fitness
)


(defun new-tour (newPath)
	(setq tour (make-tour :path newPath :distance 0 :fitness 0))
)


(defun Population-size (pop)
	(list-length pop))

  
(defun shuffle (data)
  (setq n (list-length data))
	(setq lst (make-list n))
  (loop for i from 0 to (- n 1)do
    (setf (nth i lst) (nth i data))
	)
  (loop for i from 0 to (- n 1) do
		(setq j (random n))
  	(setq temp (nth j lst))
    (setf (nth j lst) (nth i lst))
		(setf (nth i lst) temp)
  )
  (return-from shuffle lst)
)

(defun generateTour (num)
	(new-tour (shuffle TourManagerCities))
)


(defun getNewPopulation (num initialize)
  (setq genPop ())
  (if initialize
    (loop for i from 1 to num do
      (push (new-tour (shuffle TourManagerCities)) genPop)
    )
    (setq genPop (make-list num))
	)
	(return-from getNewPopulation genPop)
  )

(defun getFittest (pop)
  (setq fittest (nth 0 pop))
  (loop for i from 1 to  (- (list-length pop) 1)  do
    (setq thisTour (nth i pop))
    (if (> (getFitness thisTour) (getFitness fittest))
      (setq fittest thisTour)
    )
	)
	(return-from getFittest fittest)
)

(defun distanceTo (src dest)
	(setq xDist (abs (- (city-x src) (city-x dest))))
	(setq yDist (abs (- (city-y src) (city-y dest))))
	(setq dist (sqrt (coerce (+ (* xDist xDist) (* yDist yDist))'float)))
)

(defun getDistance(tour)
	(if (= (tour-distance tour) 0)
    (progn 
      (setq tourDistance 0)
      (setq thisPath (tour-path tour))
      (setq n (list-length thisPath))
      (loop for i from 0 to (- n 1) do
        (setq fromCity (nth i thisPath))
        (if (< i (- n 1))
          (setq destCity (nth (+ i 1) thisPath))
          (setq destCity (nth 0 thisPath))
        )
        (setq tourDistance (+ tourDistance (distanceTo fromCity destCity)))
      )
      (setf (tour-distance tour) tourDistance)
    )
	(tour-distance tour))
)

(defun getFitness (tour)
    (if (= (tour-fitness tour) 0)
		(progn
			(setq dist (getDistance tour))
			(if (= dist 0)
				(setf (tour-fitness tour) most-positive-fixnum)
				(setf (tour-fitness tour) (coerce (/ 1 (getDistance tour)) 'float)))
		))
	(tour-fitness tour)
)
(defvar tournamentSize 5)

(defun tournamentSelection (pop)
	(setq num (Population-size pop))
	(setq tournament (getNewPopulation tournamentSize nil))
	(loop for i from 0 to (- tournamentSize 1) do
		(setq j (random num))
		(setf (nth i tournament) (nth j pop))
	)
	(setq fittest (getFittest tournament))
)

(defun inPath (child city)
	(find city child)
)

(defun crossover(a b)
	(setq num (list-length TourManagerCities))
	(setq path (make-list num))
	(setq startPos (random num))
	(setq endPos (random num))
	(loop for i from 0 to (- num 1) do
		(if (and (and (< startPos endPos) (> i startPos)) (< i endPos))
			(setf (nth i path) (nth i (tour-path a)))
			(if (> startPos endPos)
				(if (not (and (< i startPos) (> i endPos)))
						(setf (nth i path) (nth i (tour-path a)))
				)
			)
		)
	)
	(loop for i from 0 to (- num 1) do
		(if (not (inPath path (nth i (tour-path b)) ))
			(loop for j from 0 to num do
				(if (not (nth j path))
					(progn
						(setf (nth j path) (nth i (tour-path b)))
						(return)
          )
				)
			)
		)
	)
	(setq child (new-tour path))
	(return-from crossover child)
)

(defvar mutationRate 0.015)

(defun mutate (child)
	(setq thisTour (tour-path child))
	(setq num (list-length thisTour))
	(loop for tourPos1 from 0 to (- num 1) do
		(setq j (random 1.00))
		(if (< j mutationRate)
			(progn
				(setq tourPos2 (random num))
				(setq city1 (nth tourPos1 thisTour))
				(setq city2 (nth tourPos2 thisTour))
				(setf (nth tourPos1 thisTour) city2)
				(setf (nth tourPos2 thisTour) city1)
			)
		)	
	)
	(return-from mutate child)
)


(defun evolvePopulation (pop)
  (setq newNum (Population-size pop))
	(setq newPop (getNewPopulation newNum nil))
  (setf (nth 0 newPop) (getFittest pop))
	(setq elitismOffset 1)
	(loop for i from elitismOffset to (- newNum 1) do
    (setq parent1 (tournamentSelection pop))
		(setq parent2 (tournamentSelection pop))
		(setq child (crossover parent1 parent2))
		(setf (nth i newPop) child)
	)
	(loop for i from elitismOffset to (- newNum 1) do
  	(setf (nth i newPop) (mutate (nth i newPop)))
	)
	(return-from evolvePopulation newPop)
)

(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
				collect line)
  )
)
(defun findCity (start)		
	(loop for n in TourManagerCities do		
		(if (= (city-city-id n) start)		
			(setq thisCity n)
    )		
	)		
	(return-from findCity thisCity)		
)

(defun printTour (tour)
	(setq tourPath (tour-path tour))
	(setq lstAfter ())
	(setq actlst ())
	(setq foundStart nil)
	(loop for x in tourPath do
		(if (not foundStart )
      (if (= (city-city-id startCity) (city-city-id x))
        (setq foundStart t)
        (push x lstAfter)
      )
		)
		(if (not (not foundStart))	
			(push x actLst)
		)
  ) 
	(setq actLst (append (reverse actLst) (reverse lstAfter)))
	(setq tourLst ())
	(loop for x in actLst do
		(push (city-city-id x) tourLst)
	)
	(setq tourLst (reverse tourLst))
	(format t "~%tour by city IDs:~%")
	(printList tourLst)
	(format t "~%tour by city coordinates:~%")
 	(printCities actLst)
)

(defun printList (lst)
    (loop for x in lst do
	(format t "~d -> " x))
	(format t "~d~%" (first lst))
)	
(defun printCities (lst)
	(loop for x in lst do
		(format t "(~d, ~d) -> " (city-x x) (city-y x) ))
	(format t "(~d, ~d)~%~%"(city-x (first lst)) (city-y (first lst)))

)

(defun main (filename)
	(setf *random-state* (make-random-state t))
	(if (not (probe-file filename))
		(princ "Cannot find file. Enter a Valid file name")
    (progn
      (setq line (get-file filename))
      (setq N (parse-integer (first line)))
      (if (< N 1)
        (princ "Cannot process with less than 1 city")
        (if (= N 1)
          (princ "Distance = 0")
          
          (progn
            (setq cityList ())
			(setq cityIDList ())
            (loop for i from 1 to N do
              (setq val (nth i line))
              (setq dat (with-input-from-string (in val)
                 (loop for dat = (read in nil nil) while dat collect dat))
              )
              (setq city (make-city :x (second dat) :y (third dat)))
              (setf (city-city-id city) (first dat))
			  (setq cityIDList (cons (first dat) cityIDList))
              (setq cityList (cons city cityList))
            )
            (defvar TourManagerCities cityList)

            (setq Start (parse-integer (nth (+ N 1) line)))
            (if (not (find Start cityIDList))      ;(> Start N)
              (princ "Start city ID not in list of cities provided")
              (progn
              
                (defVar startCity (findCity start))
                (setq pop (getNewPopulation 50 t))

                (format t "Initial distance = ~d~%" (getDistance (getFittest pop)))
                (setq pop (evolvePopulation pop))

                (loop for i from 0 to 100 do
                  (setq pop (evolvePopulation pop))
                )
                (setq finalTour (getFittest pop))
                (format t "Final distance = ~d~%" (getDistance finalTour)) ;(getFittest pop)))
                (format t "final tour:~%" )
                (printTour finalTour)
              )
            )
          )
        )
      )
    )
  )
)

(setq filename  (first *args*))
(main filename)
