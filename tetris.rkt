;;; ------------------ PROBLEM SET 8 -------------------------------
;;; NUMBER 1

; Number -> [List-of Number]
; Creates list starting from 0 to Number - 1.
(define (list-to-n-1 n)
  (build-list n (λ (x) (+ x 0))))

(check-expect (list-to-n-1 5) (list 0 1 2 3 4))
(check-expect (list-to-n-1 10) (list 0 1 2 3 4 5 6 7 8 9))

; Number -> [List-of Number]
; Creates list starting from 1 to Number.
(define (list-to-n n)
  (build-list n add1))

(check-expect (list-to-n 5) (list 1 2 3 4 5))
(check-expect (list-to-n 10) (list 1 2 3 4 5 6 7 8 9 10))

; Number -> [List-of Number]
; Creates list of fractions divided by a increasing square of 10.
(define (list-of-fractions n)
  (build-list n (λ (x) (/ 1 (expt 10 x)))))

(check-expect (list-of-fractions 3) (list 1 .1 .01))
(check-expect (list-of-fractions 5) (list 1 .1 .01 .001 .0001))

; Number -> [List-of Number]
; Creates the list of the first n even numbers.
(define (list-of-evens n)
  (build-list n (λ (x) (* 2 x))))

(check-expect (list-of-evens 0) empty)
(check-expect (list-of-evens 1) (list 0))
(check-expect (list-of-evens 5) (list 0 2 4 6 8))

; Number -> [List-of [List-of Number]]
; Creates a list of lists of 0 and 1 in a diagonal arrangement.
(define (diagonal n)
  (build-list n (λ (x)
                  (build-list n (λ (y)
                                  (if (= x y) 1 0))))))

(check-expect (diagonal 0) empty)
(check-expect (diagonal 1) (list (list 1)))
(check-expect (diagonal 5)
              (list
               (list 1 0 0 0 0)
               (list 0 1 0 0 0)
               (list 0 0 1 0 0)
               (list 0 0 0 1 0)
               (list 0 0 0 0 1)))

; Number [Number -> Number] -> [List-of Number]
; Applies function to list of numbers and tabulates results in a list.
(define (tabulate n f)
  (build-list (add1 n) (λ (x) (f (- n x)))))

(check-expect (tabulate 1 sqr) (list 1 0))
(check-expect (tabulate 2 sqr) (list 4 1 0))
(check-expect (tabulate 2 add1) (list 3 2 1))

;;; --------------------------- NUMBER 2 -----------------------------------

(require 2htdp/image)

; Number -> Boolean
; Decides whether it is less than 10.
(λ (x) (< x 10))

; Number Number -> String
; Multiplies numbers and turns result into a string.
(λ (x y) (number->string (* x y)))

; An IR is a (make-ir String Number)
(define-struct ir [name price])

; IR IR -> Boolean
; Compares inventory records by price.
; Returns true if first price is more expensive than the second.
; False if it is less expensive.
(λ (x y) (cond [(> (ir-price x) (ir-price y)) true]
               [else false]))

; Number -> Number
; Produces 0 if Number is even, produces 1 if Number is odd.
(λ (x) (if (even? x) 0 1))

; Posn Image -> Image
; Adds a red dot to Image at Posn.
(λ (x y) (place-image (circle 3 'solid 'red)
                      (posn-x x) (posn-y x) y))

;;; ------------------------------------- NUMBER 3 ----------------------------------------------

(require 2htdp/image)

; append-from-foldr : [List-of X] [List-of X] -> [List-of X]
; Appends two lists using foldr function.
(define (append-from-foldr l1 l2)
  (foldr (λ (x r) (cons x r)) l2 l1))

(check-expect (append-from-foldr (list 10 20 30) (list 1 2 3))
              (list 10 20 30 1 2 3))
(check-expect (append-from-foldr (list 1 2 3) (list 10 20 30))
              (list 1 2 3 10 20 30))

; append-from-foldl : [List-of X] [List-of X] -> [List-of X]
; Appends two lists using foldl function.
(define (append-from-foldl l1 l2)
  (foldl (λ (x r) (cons x r)) l2 l1))

(check-expect (append-from-foldl (list 10 20 30) (list 1 2 3))
              (list 30 20 10 1 2 3))
(check-expect (append-from-foldl (list 1 2 3) (list 10 20 30))
              (list 3 2 1 10 20 30))

; sum-of-numbers : [List-of Number] -> Number
; Computes sum of list of numbers.
(define (sum-of-numbers l)
  (foldr + 0 l))

(check-expect (sum-of-numbers (list 1 2 3 4)) 10)
(check-expect (sum-of-numbers (list 10 20 30)) 60)

; product-of-numbers : [List-of Number] -> Number
; Computes product of list of numbers.
(define (product-of-numbers l)
  (foldr * 1 l))

(check-expect (product-of-numbers (list 1 2 3 4)) 24)
(check-expect (product-of-numbers (list 10 20 30)) 6000)

; compose-images : [List-of Image] -> Image
; Horizontally composes a list of images.
(define (compose-images.v1 l)
  (foldr (λ (x r) (beside x r)) empty-image l))

(check-expect (compose-images.v1 (list (circle 10 "solid" "red")
                                       (rectangle 10 20 "solid" "green")
                                       (triangle 10 "solid" "blue")))
              (beside (circle 10 "solid" "red")
                      (rectangle 10 20 "solid" "green")
                      (triangle 10 "solid" "blue")))

; compose-images : [List-of Image] -> Image
; Using foldl to do same thing.
(define (compose-images.v2 l)
  (foldl (λ (x r) (beside r x)) empty-image l))

(check-expect (compose-images.v2 (list (circle 10 "solid" "red")
                                       (rectangle 10 20 "solid" "green")
                                       (triangle 10 "solid" "blue")))
              (beside (circle 10 "solid" "red")
                      (rectangle 10 20 "solid" "green")
                      (triangle 10 "solid" "blue")))

; vertical-images : [List-of Image] -> Image
; Vertically stacks images.
(define (vertical-images l)
  (foldr (λ (x r) (above x r)) empty-image l))

(check-expect (vertical-images (list (circle 10 "solid" "red")
                                     (rectangle 10 20 "solid" "green")
                                     (triangle 10 "solid" "blue")))
              (above (circle 10 "solid" "red")
                     (rectangle 10 20 "solid" "green")
                     (triangle 10 "solid" "blue")))

;;; -------------------------------------- TETRIS GAME -------------------------------------------------

(require 2htdp/image)
(require 2htdp/universe)

;;; ----------------- DATA DEFINITIONS -----------------------

;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates when it spins.
(define-struct tetra (center blocks))

;; A Set of Blocks (BSet) is one of:
;;  - empty
;;  - (cons Block BSet)
;; Order does not matter.

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;; --------------------- CONSTANTS -----------------

(define GRID-SIZE 20) ; width of a grid square in pixels
(define BOARD-HEIGHT 20) ; height in grid squares
(define BOARD-WIDTH 10) ; width in grid squares
(define HALF-BOARD-WIDTH (/ BOARD-WIDTH 2))
(define HALF-BOARD-HEIGHT (/ BOARD-HEIGHT 2))
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT))
(define BOARD-WIDTH-PIXELS (* GRID-SIZE BOARD-WIDTH))
(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))
(define SQUARE-OUTLINE (square GRID-SIZE 'outline 'black))
(define CENTER (make-posn HALF-BOARD-WIDTH -1))
(define GAME-OVER (text "GAME OVER" (round (/ BOARD-HEIGHT-PIXELS 15)) 'black))
(define SCORE (text "Score" 20 'black))

(define POSN-TEST (make-posn 10 20))
(define BLOCK-1-TEST (make-block 10 20 'red))
(define BLOCK-2-TEST (make-block 50 50 'red))
(define BSET-TEST (list BLOCK-1-TEST BLOCK-2-TEST))
(define TETRA-TEST (make-tetra POSN-TEST BSET-TEST))
(define PILE-TEST (list BLOCK-1-TEST BLOCK-2-TEST))
(define WORLD-TEST (make-world TETRA-TEST PILE-TEST))

;;; ------------------------------------ SEVEN TYPES OF TETRA --------------------------------------------

(define tetra-O (make-tetra CENTER (list (make-block (- HALF-BOARD-WIDTH 1) -1 'green)
                                         (make-block HALF-BOARD-WIDTH -1 'green)
                                         (make-block (- HALF-BOARD-WIDTH 1) -2 'green)
                                         (make-block HALF-BOARD-WIDTH -2 'green))))

(define tetra-J (make-tetra CENTER (list (make-block HALF-BOARD-WIDTH -1 'cyan)
                                         (make-block (- HALF-BOARD-WIDTH 1) -1 'cyan)
                                         (make-block (- HALF-BOARD-WIDTH 1) -2 'cyan)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -1 'cyan))))

(define tetra-L (make-tetra CENTER (list (make-block HALF-BOARD-WIDTH -1 'purple)
                                         (make-block (- HALF-BOARD-WIDTH 1) -1 'purple)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -1 'purple)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -2 'purple))))

(define tetra-S (make-tetra CENTER (list (make-block HALF-BOARD-WIDTH -1 'red)
                                         (make-block (- HALF-BOARD-WIDTH 1) -1 'red)
                                         (make-block HALF-BOARD-WIDTH -2 'red)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -2 'red))))

(define tetra-T (make-tetra CENTER (list (make-block HALF-BOARD-WIDTH -1 'orange)
                                         (make-block (- HALF-BOARD-WIDTH 1) -1 'orange)
                                         (make-block HALF-BOARD-WIDTH -2 'orange)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -1 'orange))))

(define tetra-Z (make-tetra CENTER (list (make-block HALF-BOARD-WIDTH -1 'pink)
                                         (make-block (- HALF-BOARD-WIDTH 1) -2 'pink)
                                         (make-block HALF-BOARD-WIDTH -2 'pink)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -1 'pink))))

(define tetra-I (make-tetra CENTER (list (make-block (- HALF-BOARD-WIDTH 2) -1 'blue)
                                         (make-block (- HALF-BOARD-WIDTH 1) -1 'blue)
                                         (make-block HALF-BOARD-WIDTH -1 'blue)
                                         (make-block (+ HALF-BOARD-WIDTH 1) -1 'blue))))

;;; ------------------------------------------- GAMEPLAY FUNCTIONS --------------------------------------

;;; score-of-game : World -> Number
;;; Outputs the score of the game
(define (score-of-game w)
  (length (world-pile w)))

(check-expect (score-of-game WORLD-TEST) 2)

;;; ------------------------------------------- IMAGE RENDERING FUNCTIONS --------------------------------

;;; place-image/grid : Image Number Number Scene -> Scene
;;; Places image onto a scene in grid coordinates.
(define (place-image/grid img x y scene)
  (place-image img
               (round (* GRID-SIZE (+ 1/2 x)))
               (round (* GRID-SIZE (+ 1/2 y)))
               scene))

(check-expect (place-image/grid SQUARE-OUTLINE 10 20 BACKGROUND)
              (place-image SQUARE-OUTLINE 210 410 BACKGROUND))

;;; game-over-scene : World -> Image
;;; Shows "GAME OVER" on screen.
(define (game-over-scene w)
  (place-image/grid GAME-OVER HALF-BOARD-WIDTH HALF-BOARD-HEIGHT (world->scene w)))

(check-expect (game-over-scene WORLD-TEST) (place-image/grid GAME-OVER HALF-BOARD-WIDTH
                                                             HALF-BOARD-HEIGHT (world->scene WORLD-TEST)))

;;; -------------------------------------------- TETRA MOTION ----------------------------------------

;;; move-tetra : Direction Tetra -> Tetra
;;; Move Tetra one grid unit
(define (move-tetra dir tet)
  (local [(define (move-center dir p)
            (cond [(symbol=? dir 'left) (make-posn (- (posn-x p) 1) (posn-y p))]
                  [(symbol=? dir 'right) (make-posn (+ (posn-x p) 1) (posn-y p))]
                  [(symbol=? dir 'down) (make-posn (posn-x p) (+ (posn-y p) 1))]))
          (define (move-block dir b)
            (cond [(symbol=? dir 'left) (make-block (- (block-x b) 1) (block-y b) (block-color b))]
                  [(symbol=? dir 'right) (make-block (+ (block-x b) 1) (block-y b) (block-color b))]
                  [(symbol=? dir 'down) (make-block (block-x b) (+ (block-y b) 1) (block-color b))]))
          (define (move-bset dir bset)
            (map (λ (b)
                   (move-block dir b))
                 bset))]
    (make-tetra (move-center dir (tetra-center tet)) (move-bset dir (tetra-blocks tet)))))

(check-expect (move-tetra 'down TETRA-TEST) (make-tetra (make-posn 10 21)
                                                        (list (make-block 10 21 'red) (make-block 50 51 'red))))

;;; -------------------------------------- ROTATION FUNCTIONS ---------------------------------------------

;; block-rotate-cw : Posn Block -> Block
;; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

(check-expect (block-rotate-cw POSN-TEST BLOCK-1-TEST) BLOCK-1-TEST)
(check-expect (block-rotate-cw POSN-TEST BLOCK-2-TEST) (make-block -20 60 'red))

;;; block-rotate-ccw : Posn Block -> Block
;;; Rotate the block 90 degrees counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (block-rotate-cw c (block-rotate-cw c (block-rotate-cw c b))))

(check-expect (block-rotate-ccw POSN-TEST BLOCK-1-TEST)
              (block-rotate-cw POSN-TEST
                               (block-rotate-cw POSN-TEST
                                                (block-rotate-cw POSN-TEST BLOCK-1-TEST))))

;;; bset-rotate : Posn BSet [Posn Block -> Block] -> BSet
;;; Rotate BSet 90 degrees around a posn in a given direction.
(define (bset-rotate c bset f)
  (map (λ (b)
         (f c b))
       bset))

(check-expect (bset-rotate POSN-TEST BSET-TEST block-rotate-cw)
              (list (block-rotate-cw POSN-TEST BLOCK-1-TEST) (block-rotate-cw POSN-TEST BLOCK-2-TEST)))
(check-expect (bset-rotate POSN-TEST BSET-TEST block-rotate-ccw)
              (list (block-rotate-ccw POSN-TEST BLOCK-1-TEST) (block-rotate-ccw POSN-TEST BLOCK-2-TEST)))

;;; -------------------------------------- TETRA BOUNDARY FUNCTIONS --------------------------------------------

;;; tetra-bottom? : Tetra -> Boolean
;;; Checks if Tetra is at bottom of the grid
(define (tetra-bottom? tet)
  (local [(define (block-bottom? b)
            (>= (+ (block-y b) 1) BOARD-HEIGHT))
          (define (bset-bottom? bset)
            (ormap (λ (b)
                     (block-bottom? b))
                   bset))]
    (bset-bottom? (tetra-blocks tet))))

(check-expect (tetra-bottom? (make-tetra (make-posn 1 2) (list (make-block 1 2 'red)
                                                               (make-block 1 3 'red)
                                                               (make-block 1 4 'red))))
              false)

; bset-border? : BSet Symbol -> Boolean
; Is BSet on the right, left, or top border?
(define (bset-border? bset dir)
  (local [(define (block-right? b)
            (>= (block-x b) (- BOARD-WIDTH 1)))
          (define (block-left? b)
            (<= (block-x b) 0))
          (define (block-top? b)
            (<= (block-y b) 0))]
    (ormap (λ (b)
             (cond [(symbol=? dir 'top)
                    (block-top? b)]
                   [(symbol=? dir 'left)
                    (block-left? b)]
                   [(symbol=? dir 'right)
                    (block-right? b)]))
           bset)))

(check-expect (bset-border? BSET-TEST 'right) true)
(check-expect (bset-border? BSET-TEST 'left) false)

;;; pile-top? : World -> Boolean
;;; Checks if Pile of blocks in the world reaches the top
(define (pile-top? w)
  (bset-border? (world-pile w) 'top))

(check-expect (pile-top? WORLD-TEST) (bset-border? PILE-TEST 'top))

;;; -------------------Check if touching blocks-----------------------------

;;; tetra-on-pile? : World -> Boolean
;;; Checks if the tetra is on top of the world's pile
(define (tetra-on-pile? w)
  (bset-touching-bset? (tetra-blocks (world-tetra w)) (world-pile w) 'top))

(check-expect (tetra-on-pile? WORLD-TEST) false)

;;; bset-touching-bset? : BSet BSet Symbol -> Boolean
;;; Checks if BSet is touching BSet.
(define (bset-touching-bset? bset1 bset2 side)
  (local [(define (block-above-block? b1 b2) ;;; Checks if b1 is above b2.
            (and (= (- (block-y b2) (block-y b1)) 1)
                 (= (block-x b1) (block-x b2))))
          (define (block-touching-block? b1 b2) ;;; Checks if b1 and b2 touch from left or right side.
            (and (= (abs (- (block-x b2) (block-x b1))) 1)
                 (= (block-y b1) (block-y b2))))]
    (ormap (λ (x)
             (ormap (λ (y)
                      (cond [(symbol=? side 'top) (block-above-block? x y)]
                            [(symbol=? side 'side) (block-touching-block? x y)]))
                    bset2))
           bset1)))

(check-expect (bset-touching-bset? BSET-TEST empty 'top) false)
(check-expect (bset-touching-bset? BSET-TEST (list (make-block 30 20 'red)
                                                   (make-block 51 50 'red))
                                   'side) true)
(check-expect (bset-touching-bset? BSET-TEST (list (make-block 10 21 'red)
                                                   (make-block 10 22 'red))
                                   'top) true)

;;; tetra-touching? : World -> Boolean
;;; Checks if Tetra is touching the world's pile on left or right
(define (tetra-touching? w)
  (bset-touching-bset? (tetra-blocks (world-tetra w)) (world-pile w) 'side))

(check-expect (tetra-touching? WORLD-TEST) (bset-touching-bset? BSET-TEST PILE-TEST 'side))

;;; -------------------------------------- AFTER COLLISION FUNCTIONS -----------------------------------

;;; new-tetra : Number -> Tetra
;;; Creates new Tetra
(define (new-tetra num)
  (cond [(= num 1) tetra-T]
        [(= num 2) tetra-Z]
        [(= num 3) tetra-S]
        [(= num 4) tetra-O]
        [(= num 5) tetra-I]
        [(= num 6) tetra-L]
        [(= num 7) tetra-J]
        [(= num 0) (new-tetra (random 8))]))

(check-expect (new-tetra 1) tetra-T)
(check-expect (new-tetra 7) tetra-J)

;;; world-after-collision : World -> World
;;; Creates new world after collision occurs
(define (world-after-collision w)
  (local [(define (collapse? lob) 
            (not (empty? lob)))
          (define (add-to-pile tet bset) ;;; Adds tetra to world's pile
            (append (tetra-blocks tet) bset))
          (define (row-lengths bset lon) ;;; Creates new list of BSets with same y-value.
            (map (λ (x)
                   (filter (λ (b)
                             (= (block-y b) x))
                           bset))
                 lon))
          (define (complete-rows lob) ;;; Creates list of BSets that have full rows.
            (filter (λ (l)
                      (= (length l) 10))
                    lob))
          (define (y-values lob) ;;; Creates list of y-values.
            (map (λ (bset)
                   (block-y (first bset)))
                 lob))
          (define (row-clear bset lon) ;;; Removes completed row from world's pile.
            (filter (λ (b)
                      (not (ormap (λ (x)
                                    (= (block-y b) x))
                                  lon)))
                    bset))
          (define (pile-drop bset lon) ;;; Drops pile down by number of completed rows.
            (map (λ (b)
                   (cond [(< (block-y b) (first lon))
                          (make-block (block-x b)
                                      (+ (block-y b) (length lon))
                                      (block-color b))]
                         [else b]))
                 bset))
          (define (collapse bset) ;;; Collapses world's pile.
            (pile-drop (row-clear bset
                                  (y-values
                                   (complete-rows
                                    (row-lengths
                                     bset
                                     (build-list BOARD-HEIGHT add1)))))
                       full-rows))
          (define full-rows (y-values (complete-rows
                                       (row-lengths
                                        (add-to-pile (world-tetra w) (world-pile w))
                                        (build-list BOARD-HEIGHT add1)))))]
    (if (collapse? full-rows)
        (make-world (new-tetra (random 8))
                    (collapse (add-to-pile (world-tetra w) (world-pile w))))
        (make-world (new-tetra (random 8))
                    (add-to-pile (world-tetra w) (world-pile w))))))

(check-random (world-after-collision WORLD-TEST)
              (make-world (new-tetra (random 8))
                          (append (tetra-blocks (world-tetra WORLD-TEST)) (world-pile WORLD-TEST))))

;;; --------------------------------------- WORLD FUNCTIONS ---------------------------------------------

(define WORLD1 (make-world (new-tetra (random 8)) empty))

;;; world->scene : World -> Image
;;; Creates Image of World on Scene
(define (world->scene w)
  (local
    [(define (draw-block b)
       (overlay SQUARE-OUTLINE (square GRID-SIZE 'solid (block-color b))))
     (define (bset->scene bset scene)
       (foldr (λ (b1 b2)
                (place-image/grid (draw-block b1)
                                  (block-x b1)
                                  (block-y b1)
                                  b2))
              scene bset))
     (define (tetra->scene tet scene)
       (bset->scene (tetra-blocks tet) scene))
     (define (score->scene w)
       (place-image/grid (overlay/xy SCORE 25 20
                                     (text (number->string (score-of-game w)) 15 'black))
                         (* BOARD-WIDTH .8) (* BOARD-HEIGHT .025) BACKGROUND))]
    (tetra->scene (world-tetra w) (bset->scene (world-pile w) (score->scene w)))))

(check-expect (world->scene WORLD-TEST) (place-image/grid (overlay SQUARE-OUTLINE (square GRID-SIZE 'solid 'red)) 10 20
                                                          (place-image/grid (overlay SQUARE-OUTLINE (square GRID-SIZE 'solid 'red)) 50 50
                                                                            (place-image/grid (overlay/xy SCORE 25 20
                                                                                                          (text (number->string (score-of-game WORLD-TEST)) 15 'black)) 8 .5 BACKGROUND))))

;;; next-world : World -> World
;;; Creates next world
(define (next-world w)
  (cond [(or (tetra-bottom? (world-tetra w))
             (tetra-on-pile? w)) (world-after-collision w)]
        [else (make-world (move-tetra 'down (world-tetra w))
                          (world-pile w))]))

(check-expect (next-world (make-world (make-tetra
                                       (make-posn 1 2) (list (make-block 1 2 'red)
                                                             (make-block 1 3 'red)
                                                             (make-block 1 4 'red)))
                                      (list (make-block 3 4 'blue) (make-block 3 5 'blue))))
              (make-world (make-tetra (make-posn 1 3) (list (make-block 1 3 'red)
                                                            (make-block 1 4 'red)
                                                            (make-block 1 5 'red)))
                          (list (make-block 3 4 'blue)
                                (make-block 3 5 'blue))))

;;; key-handler : World KeyEvent Number -> World
;;; Handle user input in game
(define (key-handler w ke)
  (local [(define (off-screen? b)
            (or (< (block-x b) 0)
                (> (block-x b) (- BOARD-WIDTH 1))
                (> (block-y b) BOARD-HEIGHT)))
          (define (clipping? tet dir)
            (ormap off-screen?
                   (bset-rotate (tetra-center tet)
                                (tetra-blocks tet)
                                (cond [(symbol=? dir 'cw)
                                       block-rotate-cw]
                                      [(symbol=? dir 'ccw)
                                       block-rotate-ccw]))))
          (define (key-on-border? tet ke)
            (or (and (on-border? tet 'right)
                     (key=? ke "right"))
                (and (on-border? tet 'left)
                     (key=? ke "left"))))
          (define (on-border? tet dir)
            (bset-border? (tetra-blocks tet) dir))
          (define (tetra-O? w)
            (equal? (color-of-tetra (world-tetra w)) 'green))
          (define (color-of-tetra tet)
            (block-color (first (tetra-blocks tet))))]
    (cond [(or (tetra-bottom? (world-tetra w))
               (tetra-on-pile? w)
               (pile-top? w)
               (key-on-border? (world-tetra w) ke)
               (and (tetra-touching? w)
                    (not (key=? ke "down")))) w]
          [(or (key=? ke "left")
               (key=? ke "right")
               (key=? ke "down"))
           (make-world (move-tetra (string->symbol ke)
                                   (world-tetra w))
                       (world-pile w))]
          [(key=? ke "s") (cond [(or (clipping? (world-tetra w) 'cw) (tetra-O? w)) w] ;;; Restricts movement for a tetra-O when rotated.
                                [else (make-world
                                       (make-tetra
                                        (tetra-center (world-tetra w))
                                        (bset-rotate (tetra-center (world-tetra w))
                                                     (tetra-blocks (world-tetra w))
                                                     block-rotate-cw))
                                       (world-pile w))])]
          [(key=? ke "a") (cond [(or (clipping? (world-tetra w) 'ccw) (tetra-O? w)) w] ;;; Restricts movement for a tetra-O when rotated.
                                [else (make-world
                                       (make-tetra
                                        (tetra-center (world-tetra w))
                                        (bset-rotate (tetra-center (world-tetra w))
                                                     (tetra-blocks (world-tetra w))
                                                     block-rotate-ccw))
                                       (world-pile w))])]
          [else w])))

(big-bang WORLD1
          (on-tick next-world 0.5)
          (to-draw world->scene)
          (on-key key-handler)
          (stop-when pile-top? game-over-scene))
