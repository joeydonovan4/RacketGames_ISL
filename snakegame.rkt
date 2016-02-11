(require 2htdp/image)
(require 2htdp/universe)

(define-struct world (snake food))
(define-struct snake (dir segs))

;;; SNAKE WORLD

;;; World is (make-world Snake Food)
;;; Food is Posn
;;; Snake is (make-snake Direction Segs)
;;;   A snake's Segs may not be empty.
;;;   The first element of the list is the head.
;;; Direction is one of: 'up 'down 'left 'right
;;; Segs is one of:
;;;  -- empty
;;;  -- (cons Posn Segs)
;;; NESegs is one of:
;;;  -- (cons Posn empty)
;;;  -- (cons Posn NESegs)
;;; Coordinates are in "grid" units, with X running left-to-right,
;;; and Y running bottom-to-top.

;; --- CONSTANTS : DESCRIBE PROPERTIES THAT ARE ALWAYS THE SAME 

(define GRID-SIZE 10) ; width of a game-board square
(define BOARD-HEIGHT 20) ; height in grid squares
(define BOARD-WIDTH  30) ; width  in grid squares
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT))
(define BOARD-WIDTH-PIXELS  (* GRID-SIZE BOARD-WIDTH))

(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))

(define SEGMENT-RADIUS (quotient GRID-SIZE 2))
(define SEGMENT-IMAGE  (circle SEGMENT-RADIUS 'solid 'red))
(define FOOD-RADIUS (floor (* 0.9 SEGMENT-RADIUS)))
(define FOOD-IMAGE  (circle FOOD-RADIUS 'solid 'green))

(define Snake1 (make-snake 'right (list (make-posn 5 3))))
(define Food1  (make-posn 8 12))
(define World1 (make-world Snake1 Food1))

;; --- FUNCTIONS

;;; Image-painting functions

;;; world->scene : World -> Scene
;;; Build an image of the given world
(define (world->scene w)
  (snake+scene (world-snake w)
               (food+scene (world-food w)
                           BACKGROUND)))

;;; food+scene : Food Scene -> Scene
;;; Add image of food to the given scene.
(define (food+scene f scene)
  (place-image/grid FOOD-IMAGE (posn-x f) (posn-y f) scene))

;;; place-image/grid : Image Number Number Scene -> Image
;;; Just like place-image, but use grid coordinates
(define (place-image/grid img x y scene)
  (place-image img
               (round (* GRID-SIZE (+ 1/2 x)))
               (round (- BOARD-HEIGHT-PIXELS
                         (* GRID-SIZE (+ 1/2 y))))
               scene))

;;; snake+scene : Snake Scene -> Scene
;;; Add an image of the snake to the scene.
(define (snake+scene snake scene)
  (segments+scene (snake-segs snake) scene))

;;; segments+scene : Segs Scene -> Scene
;;; Add an image of the snake segments to the scene
(define (segments+scene segs scene)
  (cond [(empty? segs) scene]
        [else (segment+scene (first segs)
                             (segments+scene (rest segs) scene))]))

;;; segment+scene : Posn Scene -> Scene
;;; Add one snake segment to a scene.
(define (segment+scene seg scene)
  (place-image/grid SEGMENT-IMAGE (posn-x seg) (posn-y seg) scene))

;;; Snake motion & growth functions

;;; snake-slither: Snake -> Snake
;;; Move the snake by one step in the appropriate direction
(define (snake-slither s)
  (make-snake (snake-dir s)
              (cons (move-posn (snake-dir s) (first (snake-segs s)))
                    (all-but-last (snake-segs s)))))

;;; all-but-last : NESegs -> Segs
;;; All but the last element of the list.
;;; If the first element is also the last, return empty list.
;;; If there is more than one element left in the list, create a new list with the first element,
;;; then recall the function.
(define (all-but-last nesegs)
  (cond [(empty? (rest nesegs)) empty]
        [else (cons (first nesegs) (all-but-last (rest nesegs)))]))

;;; move-posn : Direction Posn -> Posn
;;; The Posn one over from input Posn in given Direction.
(define (move-posn dir p)
  (cond [(symbol=? dir 'up) (make-posn (posn-x p) (+ (posn-y p) 1))]
        [(symbol=? dir 'down) (make-posn (posn-x p) (- (posn-y p) 1))]
        [(symbol=? dir 'left) (make-posn (- (posn-x p) 1) (posn-y p))]
        [(symbol=? dir 'right) (make-posn (+ (posn-x p) 1) (posn-y p))]))

;;; new-direction : Snake Direction -> Snake
;;; Changes Direction of Snake
;;; If the current direction of the snake is equal to the inputted key direction, output the same snake.
;;; If it is a different direction, make a new snake with the inputted key direction.
(define (new-direction s dir)
  (cond [(current-dir? dir s) s]
        [else (make-snake dir (snake-segs s))]))

;;; current-dir? : Direction Snake -> Boolean
;;; Checks if snake is already moving in the indicated direction
(define (current-dir? dir s)
  (symbol=? dir (snake-dir s)))

;;; next-world : World -> World
;;; Returns next state of world
;;; If the snake is eating then make a new world with a larger snake.
;;; If the snake is not eating and the snake is dead, restart the game.
;;; If neither of those, continue to move the snake.
(define (next-world w)
  (cond [(snake-eating? w) (snake-eat w)]
        [(and (not (snake-eating? w)) (snake-death? (world-snake w))) World1]
        [else (make-world (snake-slither (world-snake w)) (world-food w))]))

;;; key-handler: World KeyEvent -> World
;;; Handle user input in the game
(define (key-handler w ke)
  (cond [(key=? ke "up") (make-world (new-direction (world-snake w) 'up) (world-food w))]
        [(key=? ke "down") (make-world (new-direction (world-snake w) 'down) (world-food w))]
        [(key=? ke "left") (make-world (new-direction (world-snake w) 'left) (world-food w))]
        [(key=? ke "right") (make-world (new-direction (world-snake w) 'right) (world-food w))]
        [else w]))

;;; snake-eat : World -> World
;;; Produce new World after Snake has eaten food
;;; Creates a new world with a growing snake and a new random position of food.
(define (snake-eat w)
  (make-world (snake-grow (world-snake w)) (make-posn (random BOARD-WIDTH) (random BOARD-HEIGHT))))

;;; snake-grow : Snake -> Snake
;;; Create new snake with an added segment
;;; Continues to move the snake but with an added segment on the end now.
(define (snake-grow s)
  (snake-slither (make-snake (snake-dir s) (add-segment (snake-segs s)))))

;;; add-segment : NESegs -> NESegs
;;; Add one segment to NESegs
;;; Creates a list with the first element, then checks if that was the last element in the list.
;;; If so, it creates another segment with the same position as the previous.
;;; If not, it recalls the function on the rest of the list.
(define (add-segment nesegs)
  (cons (first nesegs) (cond [(empty? (rest nesegs)) (cons (first nesegs) empty)]
                             [else (add-segment (rest nesegs))])))

;;; snake-eating? : World -> Boolean
;;; Returns true if Snake head shares same position as Food
;;; If the position of the first segment of the snake is equal to the position of the food return true.
(define (snake-eating? w)
  (and (= (posn-x (first (snake-segs (world-snake w)))) (posn-x (world-food w)))
       (= (posn-y (first (snake-segs (world-snake w)))) (posn-y (world-food w)))))

;;; snake-death? : Snake -> Boolean
;;; Returns true if Snake hits dies
;;; If the snake self-collides or collides with the wall return true.
(define (snake-death? s)
  (or (snake-self-collide? s) (snake-wall-collide? s)))

;;; snake-self-collide? : Snake -> Boolean
;;; Returns true if Snake runs into itself
;;; If the first segment of the snake shares the same position as another segment of the snake,
;;; return true.
(define (snake-self-collide? s)
  (member? (first (snake-segs s)) (rest (snake-segs s))))

;;; snake-wall-collide? : Snake -> Boolean
;;; Returns true if Snake runs into the wall
;;; If the x position of the first segment is greater than the width of the board or less than zero,
;;; or the y position of the first segment is greater than the height of the board or less than zero
;;; return true.
(define (snake-wall-collide? s)
  (or (> (posn-x (first (snake-segs s))) BOARD-WIDTH) (< (posn-x (first (snake-segs s))) 0)
      (> (posn-y (first (snake-segs s))) BOARD-HEIGHT) (< (posn-y (first (snake-segs s))) 0)))

(big-bang World1
          [on-tick next-world .25]
          [to-draw world->scene]
          [on-key key-handler])

