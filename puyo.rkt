;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puyo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;------------------ Constants -----------------------------------

(define CELL-SIZE 50)
(define GRID-HEIGHT 12) ; in number of cells
(define GRID-WIDTH 6)   ; in number of cells
(define PLAY-SPEED 1)  ; in seconds/tick
; Beginner: 1, Intermediate: .5, Expert: .25
(define FONT-SIZE 16);
(define BACKGROUND-EMPTY (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))
(define SCOREBOARD (empty-scene (/ (* CELL-SIZE GRID-WIDTH) 2)
                                (* CELL-SIZE GRID-HEIGHT)))
(define RADIUS (floor (/ CELL-SIZE 2.5)))
(define INIT-X 2) ; new puyos will start in this column of the grid (0-indexed)

; to draw gridlines

; add-line-v : Number Image -> Image
; draw a vertical line at the given x-coordinate
(define (add-line-v x img)
  (scene+line img (* x CELL-SIZE) 0 (* x CELL-SIZE) (* GRID-HEIGHT CELL-SIZE) "black"))

; add-line-h : Number Image -> Image
; draw a horizontal line at the given y-coordinate
(define (add-line-h y img)
  (scene+line img  0 (* y CELL-SIZE) (* GRID-WIDTH CELL-SIZE) (* y CELL-SIZE) "black"))

(define BACKGROUND-H (foldr add-line-h BACKGROUND-EMPTY (build-list GRID-HEIGHT add1)))
(define BACKGROUND (foldr add-line-v BACKGROUND-H (build-list GRID-WIDTH add1)))

(define FINAL-WORLD-IMG
  (overlay (text "GAME OVER" 20 "black") BACKGROUND-EMPTY))

;------------------ Data Definitions ----------------------------

; A World is one of:
; - PairWorld
; - SplitWorld
; - GroundWorld
; - PoppingWorld

; A PairWorld is a (make-world-p [List-of Puyo] [PuyoPair] Score)
(define-struct world-p [grounded floating score])
; interpretation: the given PuyoPair is descending down the screen and can be
; rotated, dropped, split, etc.

; A SplitWorld is a (make-world-s [List-of Puyo] [Puyo] Score)
(define-struct world-s [grounded floating score])
; interpretation: the PuyoPair has been caused to split in half, one half is grounded, the other
; descends to the top of its column

; A GroundWorld is a (make-world-g [List-of Puyo] Score)
(define-struct world-g [grounded score])
; interpretation: all the puyo are grounded and program is checking for blocks and chains

; A PoppingWorld is a (make-world-pop [List-of [List-of Puyo]] [List-of Puyo] Score)
(define-struct world-pop [blocks grounded score])
; interpretation: shows the blocks in a different color before they disappear

; A Score is a (make-score Number Number Number Number)
(define-struct score [curr curr-chain long-chain high])
; keep track of current score, current chain, longest chain, and all-time high score

; A Puyo is a (make-puyo Color Number Number)
(define-struct puyo [color x y])
; interpretation: color is the color the puyo will be rendered in,
; x and y are the x- and y-coordinates of the cell from the top left (0,0)
; constraint: x and y must be within the grid

; A PuyoPair is a (make-pair Puyo Puyo Number)
(define-struct pair [p1 p2 rot-idx])
; constraint: Puyo's in a pair must be adjacent horizontally or vertically
; rot-idx is a number between 0 and 3 representing the 4 possible orientations of a Puyo Pair
; as follows:
; 0 -> p1 is above p2
; 1 -> p1 is to the left of p2
; 2 -> p1 is below p2
; 3 -> p1 is to the right of p2

; pairs can be moved, created, displayed, rotated, and split

;------------------- Templates -----------------------------------
; puyo-temp : Puyo -> ??
(define (puyo-temp p)
  (... (puyo-color p) ... (puyo-x p) ... (puyo-y p) ...))

; lop-temp : [List-of Puyo] -> ??
(define (lop-temp lop)
  (cond
    [(empty? lop) ...]
    [(cons? lop) (... (puyo-temp (first lop)) ...
                      (lop-temp (rest lop)) ...)]))

; pair-temp : PuyoPair -> ??
(define (pair-temp p)
  (... (puyo-temp (pair-p1 p)) ... (puyo-temp (pair-p2 p)) ... (pair-rot-idx p) ...))

; world-temp : World -> ??
(define (world-temp w)
  (cond
    [(world-p? w) (world-p-temp w)]
    [(world-s? w) (world-s-temp w)]
    [(world-g? w) (world-g-temp w)]))

; world-p-temp : PairWorld -> ??
(define (world-p-temp w)
  (... (lop-temp (world-p-grounded w)) ... (pair-temp (world-p-floating w)) ...
       (world-p-score w) ...))

; world-s-temp : SplitWorld -> ??
(define (world-s-temp w)
  (... (lop-temp (world-s-grounded w)) ... (puyo-temp (world-s-floating w)) ...
       (world-s-score w) ...))

; world-g-temp : GroundWorld -> ??
(define (world-g-temp w)
  (... (lop-temp (world-g-grounded w)) ... (world-g-score w) ...))

; world-pop-temp : PoppingWorld -> ??
(define (world-pop-temp w)
  (... (world-pop-blocks w) ... (lop-temp (world-pop-grounded w)) ... (world-pop-score w) ...))

; score-temp : Score -> ??
(define (score-temp s)
  (... (score-curr s) ... (score-curr-chain s) ...
       (score-long-chain s) ... (score-high s) ...))

;-------------------- Examples  -----------------------------------

; Puyos
(define red-puyo (make-puyo "red" 0 0))
(define blue-puyo (make-puyo "blue" 5 11))
(define puyo-a (make-puyo "green" 1 0))
(define puyo-b (make-puyo "green" 5 10))
(define puyo-c (make-puyo "blue" 3 0))
(define puyo-d (make-puyo "purple" 3 1))
(define puyo-e (make-puyo "red" 3 0))
(define puyo-f (make-puyo "red" 3 1))
(define puyo-g (make-puyo "blue" 3 1))
(define puyo-h (make-puyo "purple" 3 2))

; Pairs
(define pair-a (make-pair red-puyo puyo-a 0))
(define pair-b (make-pair blue-puyo puyo-b 1))
(define start-pair-a (make-pair puyo-c puyo-d 0))
(define start-pair-b (make-pair puyo-f puyo-e 2))
(define down-1 (make-pair puyo-g puyo-h 1))

; Lists
(define lop-1 (list (make-puyo "blue" 0 11) (make-puyo "green" 1 11) (make-puyo "blue" 3 11)))
(define lop-2 (list (make-puyo "purple" 0 10) (make-puyo "purple" 0 9) (make-puyo "blue" 3 10)))
(define lop-3 (append lop-2 lop-1))
(define gameover-lop (list (make-puyo "red" 2 0) (make-puyo "green" 2 1) (make-puyo "blue" 2 2)
                           (make-puyo "red" 2 3) (make-puyo "purple" 2 4) (make-puyo "red" 2 5)
                           (make-puyo "red" 2 6) (make-puyo "red" 2 7) (make-puyo "blue" 2 8)
                           (make-puyo "purple" 2 9) (make-puyo "green" 2 10) (make-puyo "red" 2 11)))

; Worlds
(define blank-world (make-world-g '() (make-score 0 0 0 0)))

;-------------------- Functions ----------------------------------


;-------------------- Main -----------------------------------
; play-puyo : World -> World
; allows a user to play a game of puyo-puyo
(define (play-puyo init-world)
  (big-bang init-world
            [to-draw render-world]
            [on-key process-key]
            [on-tick update-world PLAY-SPEED]
            [stop-when game-over? (λ (x) FINAL-WORLD-IMG)]))



;------------------- Render Functions ---------------------------

; render-world : World -> Image
; displays the given World
(define (render-world w)
  (cond
    [(world-p? w) (render-world-p w)]
    [(world-s? w) (render-world-s w)]
    [(world-g? w) (render-world-g w)]
    [(world-pop? w) (render-world-pop w)]))

; render-world-p : PairWorld -> Image
; displays the descending pair and the puyos on the ground
(define (render-world-p w)
  (beside (render-lop (world-p-grounded w) (render-pair (world-p-floating w) BACKGROUND))
          (overlay (render-score (world-p-score w)) SCOREBOARD)))

; render-world-s : SplitWorld -> Image
; displays the descending puyo and the puyos on the ground
(define (render-world-s w)
  (beside (render-lop (world-s-grounded w) (render-puyo (world-s-floating w) BACKGROUND))
          (overlay (render-score (world-s-score w)) SCOREBOARD)))

; render-world-g : GroundWorld -> Image
; displays all the puyos on the ground
(define (render-world-g w)
  (beside (render-lop (world-g-grounded w) BACKGROUND)
          (overlay (render-score (world-g-score w)) SCOREBOARD)))

; render-world-pop : PoppingWorld -> Image
; displays the puyos on the ground and the blocks in a distinct way
(define (render-world-pop w)
  (beside (render-blocks (world-pop-blocks w) (render-lop (world-pop-grounded w) BACKGROUND))
          (overlay (render-score (world-pop-score w)) SCOREBOARD)))

; render-score : Score -> Image
; displays the score to the right of the game view
(define (render-score scr)
  (above (text (string-append "Score: " (number->string (score-curr scr))) FONT-SIZE "black")
         (text (string-append "Highest chain: "
                              (number->string (score-long-chain scr))) FONT-SIZE "black")
         (text (string-append "High score: " (number->string (score-curr scr))) FONT-SIZE "black")))

; render-blocks : [List-of [List-of Puyo]] Image -> Image
; draws all the blocks in a distinct way
(define (render-blocks blocks img)
  (render-lop2 (foldr append '() blocks) img))

; render-pair : PuyoPair Image -> Image
; draw the given PuyoPair on top of the given image 
(define (render-pair p background)
  (render-puyo (pair-p1 p) (render-puyo (pair-p2 p) background)))

;(check-expect (render-pair pair-a BACKGROUND) (render-puyo red-puyo (render-puyo puyo-a BACKGROUND)))
;(check-expect (render-pair pair-b empty-image)
;              (render-puyo blue-puyo (render-puyo puyo-b empty-image)))


; render-lop : [List-of Puyo] Image -> Image
; draw all the puyos in the list on top of the given background image
(define (render-lop lop background)
  (foldr render-puyo background lop))


;(check-expect (render-lop '() BACKGROUND) BACKGROUND)
;(check-expect (render-lop (list puyo-a puyo-b puyo-h) BACKGROUND)
;              (render-puyo puyo-h (render-puyo puyo-b (render-puyo puyo-a BACKGROUND))))

; render-lo2 L [List-of Puyo] Image -> Image
; draw all the puyos in a distinct way
(define (render-lop2 lop background)
  (foldr render-puyo2 background lop))


; render-puyo : Puyo Image -> Image
; draw the given Puyo on top of the background image
(define (render-puyo p background)
  (place-image (puyo->image (puyo-color p))
               (* CELL-SIZE (+ 0.5 (puyo-x p)))
               (* CELL-SIZE (+ 0.5 (puyo-y p)))
               background))

;(check-expect (render-puyo puyo-a BACKGROUND) (place-image (puyo->image "green")
;                                                           (* CELL-SIZE 1.5)
;                                                           (* CELL-SIZE .5)
;                                                           BACKGROUND))
;(check-expect (render-puyo puyo-b empty-image) (place-image (puyo->image "green")
;                                                            (* CELL-SIZE 5.5)
;                                                            (* CELL-SIZE 10.5)
;                                                            empty-image))

; render-puyo2 : Puyo Image -> Image
; draw the given puyo in outline mode
(define (render-puyo2 p background)
  (place-image (puyo->image2 (puyo-color p))
               (* CELL-SIZE (+ 0.5 (puyo-x p)))
               (* CELL-SIZE (+ 0.5 (puyo-y p)))
               background))



; puyo->image : Color -> Image
; returns the image that represents a puyo of the given color
(define (puyo->image c)
  (circle RADIUS "solid" c))

;(check-expect (puyo->image "green") (circle RADIUS "solid" "green"))
;(check-expect (puyo->image (make-color 10 30 200)) (circle RADIUS "solid" (make-color 10 30 200)))

; puyo->image2 : Color -> Image
; returns the image that represents a puyo of the given color
(define (puyo->image2 c)
  (circle RADIUS "outline" c))


;------------------- On-Tick Functions --------------------------

; update-world : World -> World
; update the state of the world:
; - check if game over
; - move floating pairs down
; - check for blocks (and chains)
; - update score
; - release the new pair after a certain interval
; - update state of on-deck pairs (todo)
(define (update-world w)
  (cond
    [(world-g? w) (pop-blocks (drop-all w))]
    [(world-pop? w) (wait w)]
    [else (move-down w)]))

; wait : PoppingWorld -> World
; displays the blocks being popped for one tick, updates the score, and switches
; back to a world-g
(define (wait w)
  (cond
    [(empty? (world-pop-blocks w))
     (release-pair (make-world-g (world-pop-grounded w)
                                 (world-pop-score w)))]
    [else
     (make-world-g (world-pop-grounded w)
                   (update-score (world-pop-blocks w) (world-pop-score w)))]))
    

; release-pair : GroundWorld -> World
; add a new PuyoPair to the view at the initial grid position
(define (release-pair w)
  (make-world-p (world-g-grounded w) (new-pair (random-color 0) (random-color 0))
                (reset-curr-chain (world-g-score w))))

; new-pair : Color Color -> PuyoPair
; generate a new PuyoPair from the two (random) colors
(define (new-pair c1 c2)
  (make-pair (new-puyo c1 -1) (new-puyo c2 0) 0))
; To do: display the next two puyo-pairs that are on-deck to the user

;(check-expect (new-pair "red" "red") (make-pair (new-puyo "red" -1) (new-puyo "red" 0) 0))
;(check-expect (new-pair "blue" "red") (make-pair (new-puyo "blue" -1) (new-puyo "red" 0) 0))

; new-puyo : Color Number -> Puyo
; create a new Puyo structure of the given color at the given y-coordinate
(define (new-puyo c y)
  (make-puyo c INIT-X y))

;(check-expect (new-puyo "red" 1) (make-puyo "red" INIT-X 1))
;(check-expect (new-puyo "blue" 0) (make-puyo "blue" INIT-X 0))

; random-color : Any -> Color
; get a random color from the list of valid colors
; red, blue, green and purple
(define (random-color x)
  (local [(define rand-x (random 4))]
    (cond
      [(= 0 rand-x) "red"]
      [(= 1 rand-x) "blue"]
      [(= 2 rand-x) "green"]
      [(= 3 rand-x) "purple"])))

;(check-member-of (random-color 0) "red" "blue" "green" "purple")

; move-down : World -> World
; move down all floating Puyo or PuyoPair by 1 if possible, or add them to the grounded list
(define (move-down w)
  (cond
    [(world-p? w) (move-down-p w)]
    [(world-s? w) (move-down-s w)]
    [else w]))

; move-down-p : PairWorld -> World
; move the puyo-pair down, possibly splitting the pair
(define (move-down-p w)
  (local [(define lop (world-p-grounded w))
          (define p1 (pair-p1 (world-p-floating w)))
          (define p2 (pair-p2 (world-p-floating w)))
          (define move-p1? (move-down?/puyo p1 lop))
          (define move-p2? (move-down?/puyo p2 lop))]
    (cond
      [(and (not move-p1?) (not move-p2?)) (make-world-g (cons p1 (cons p2 lop)) (world-p-score w))]
      [(and (not move-p1?) move-p2?) (make-world-s (cons p1 lop) p2 (world-p-score w))]
      [(and move-p1? (not move-p2?)) (make-world-s (cons p2 lop) p1 (world-p-score w))]
      [(and move-p1? move-p2?)       (make-world-p lop (move-pair-down (world-p-floating w))
                                                   (world-p-score w))])))

                         
; move-down-s : SplitWorld -> World
(define (move-down-s w)
  (make-world-g (cons (drop-puyo (world-s-floating w) (world-s-grounded w))
                      (world-s-grounded w))
                (world-s-score w)))

; move-pair-down : PuyoPair -> PuyoPair
(define (move-pair-down p)
  (make-pair (move-puyo-down (pair-p1 p)) (move-puyo-down (pair-p2 p)) (pair-rot-idx p)))

; move-puyo-down : Puyo -> Puyo
(define (move-puyo-down p)
  (make-puyo (puyo-color p) (puyo-x p) (add1 (puyo-y p))))

; move-down?/puyo : Puyo [List-of Puyo] -> Boolean
; is there a puyo one below the given puyo?
(define (move-down?/puyo p lop)
  (is-empty-cell? (puyo-x p) (add1 (puyo-y p)) lop))

;(check-expect (move-down?/puyo puyo-a lop-1) #true)
;(check-expect (move-down?/puyo (make-puyo "blue" 0 10) lop-1) #false)
;(check-expect (move-down?/puyo blue-puyo lop-2) #false)


;----- Finding Blocks ----

; get-blocks : [List-of Puyo] -> [List-of [List-of Puyo]]
; get a list of all the blocks in a given list of Puyos
; A Block is a group of 4 or more horizontally or vertically
; adjacent puyos
(define (get-blocks lop)
  (filter (λ (l) (> (length l) 3)) (get-blocks-help lop)))
  
; get-blocks-help : [List-of Puyo] -> [List-of [List-of Puyo]]
; get a list of all the blocks in a given list of Puyos
; helps get Block by returning all groups of 2 or more
; A Block is a group of 4 or more horizontally or vertically
; adjacent puyos
(define (get-blocks-help lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (local
                   [(define maybe-block (get-same-color-nbrs (first lop) lop))]
                   (if (empty? maybe-block)
                       (get-blocks-help (rest lop))
                       (cons (finish-block (first lop) maybe-block (rest lop))
                               (get-blocks-help (except (rest lop)
                                                   (finish-block (first lop) maybe-block (rest lop))
                                                   )))))]))

; except : [List-of X] [List-of X] -> [List-of X]
; removes all instances of l2 from l1
(define (except l1 l2)
  (filter (λ (x) (not (member? x l2))) l1))

; finish-block : Puyo [List-of Puyo] [List-of Puyo] -> [List-of Puyo]
(define (finish-block first-puyo start-of-block grounded)
  (cond
    [(empty? start-of-block) (list first-puyo)]
    [(cons? start-of-block)
     (local [(define next (get-same-color-nbrs (first start-of-block)
                                               (except grounded (list first-puyo))))]
       (if (empty? next)
           (append (list (first start-of-block))
                   (finish-block first-puyo (rest start-of-block)
                                 (except grounded (list (first start-of-block)))))
           (local [(define almost-finished-block
                     (finish-block (first start-of-block) next (except grounded (list first-puyo))))]
             (append almost-finished-block
                     (finish-block first-puyo (rest start-of-block)
                                   (except grounded almost-finished-block))))))]))

; get-neighbors : Puyo [List-of Puyo]-> [List-of Puyo]
; get all the Puyo's that the given Puyo is horizontally or
; vertically adjacent to
(define (get-neighbors p grounded)
  (local [(define x (puyo-x p))
          (define y (puyo-y p))
          (define (adjacent? a-puyo)
            (or (and (= x (puyo-x a-puyo)) (= (+ 1 y) (puyo-y a-puyo)))
                (and (= x (puyo-x a-puyo)) (= (- y 1) (puyo-y a-puyo)))
                (and (= (+ 1 x) (puyo-x a-puyo)) (= y (puyo-y a-puyo)))
                (and (= (- x 1) (puyo-x a-puyo)) (= y (puyo-y a-puyo)))))
          ]
    (filter adjacent? grounded)))

; get-same-color-nbrs : Puyo [List-of Puyo] -> [List-of Puyo]
(define (get-same-color-nbrs p grounded)
  (filter (λ (a-puyo) (string=? (puyo-color p) (puyo-color a-puyo)))
          (get-neighbors p grounded)))
            

; remove-blocks : GroundWorld -> GroundWorld
(define (remove-blocks w)
  (local [(define blocks (get-blocks (world-g-grounded w)))]
    (cond
      [(empty? blocks) w]
      [(cons? blocks) (remove-blocks (drop-all (make-world-g (except (world-g-grounded w)
                                                                     (foldr append '() blocks))
                                                             (world-g-score w))))])))

; drop-all : GroundWorld -> GroundWorld
; drop all the Puyo's in the gievn world as low as they can go
(define (drop-all w)
  (local [(define sorted-lop (sort (world-g-grounded w) (λ (p1 p2) (> (puyo-y p1) (puyo-y p2)))))
          (define (drop-all/r lop grounded)
            (cond
              [(empty? lop) '()]
              [(cons? lop) (if (move-down?/puyo (first lop) grounded)
                               (local [(define new-puyo (drop-puyo (first lop) grounded))]
                                 (cons new-puyo (drop-all/r (rest lop)
                                                            (except (cons new-puyo grounded)
                                                                    (list (first lop))))))
                               (cons (first lop) (drop-all/r (rest lop) grounded)))]))]
    (make-world-g (drop-all/r sorted-lop sorted-lop) (world-g-score w))))

; pop-blocks : GroundWorld -> PoppingWorld
(define (pop-blocks w)
  (local [(define blocks (get-blocks (world-g-grounded w)))]
  (make-world-pop blocks
                  (except (world-g-grounded w) (foldr append '() blocks))
                  (world-g-score w))))

; update-score : [List-of [List-of Puyo]] Score -> Score
; update the score using the given list of blocks to pop
; 10 points per block times 2^(current chain)
(define (update-score blocks scr)
  (local [(define new-score (+ (score-curr scr) (* (expt 2 (score-curr-chain scr))
                                                   (calc-score blocks))))]
    (make-score new-score
                (+ 1 (score-curr-chain scr))
                (max (+ 1 (score-curr-chain scr)) (score-long-chain scr))
                (max (score-high scr) new-score))))

; calc-score : [List-of [List-of Puyo]] -> Number
(define (calc-score blocks)
  (foldr (λ (block sum) (+ sum (* 10 (length block)))) 0 blocks))

; reset-curr-chain : Score -> Score
(define (reset-curr-chain scr)
  (make-score (score-curr scr)
              0
              (score-long-chain scr)
              (score-high scr)))
          

;----------------------------- Stop When Functions -------------------------------------------

; game-over? : World -> Boolean
; is the game over? i.e. is there a grounded puyo in the (INIT-X, 0) cell that cannot be moved down?
(define (game-over? w)
  (cond
    [(world-g? w) (game-over-g? (drop-all (remove-blocks w)))] 
    [else #false])) 

; game-over-g? : GroundWorld -> Boolean
; is the game over?
(define (game-over-g? w)
  (not (is-empty-cell? INIT-X 0 (world-g-grounded w))))


;---------------------------- ProccessKey Functions ------------------------------------------------

; process-key : World Key -> World
; handle keyboard input from user
(define (process-key w key)
  (cond
    [(world-p? w) (process-key-p w key)]
    [(world-s? w) w] ; later: add pause key, restart and exit keys when game-over
    [(world-g? w) w]
    [(world-pop? w) w]
    [else w]))

; process-key-p : PairWorld Key -> World
(define (process-key-p w key)
  (cond
    [(key=? key "left") (move-left/world w)]
    [(key=? key "right") (move-right/world w)]
    [(key=? key "down") (drop-pair/world w)]
    [(key=? key " ") (rotate/world w)]
    [else w]))

; move-left/world : PairWorld -> PairWorld
(define (move-left/world w)
  (if (can-move-left? (world-p-floating w) (world-p-grounded w))
      (make-world-p (world-p-grounded w) (move-left/pair (world-p-floating w)) (world-p-score w))
      w))

; move-left/pair : PuyoPair -> PuyoPair
(define (move-left/pair p)
  (make-pair (move-left/puyo (pair-p1 p)) (move-left/puyo (pair-p2 p)) (pair-rot-idx p)))

; move-left/puyo : Puyo -> Puyo
(define (move-left/puyo p)
  (make-puyo (puyo-color p) (sub1 (puyo-x p)) (puyo-y p)))

; move-right/world : PairWorld -> PairWorld
(define (move-right/world w)
  (if (can-move-right? (world-p-floating w) (world-p-grounded w))
      (make-world-p (world-p-grounded w) (move-right/pair (world-p-floating w)) (world-p-score w))
      w))

; move-right/pair : PuyoPair -> PuyoPair
(define (move-right/pair p)
  (make-pair (move-right/puyo (pair-p1 p)) (move-right/puyo (pair-p2 p)) (pair-rot-idx p)))

; move-right/puyo : Puyo -> Puyo
(define (move-right/puyo p)
  (make-puyo (puyo-color p) (add1 (puyo-x p)) (puyo-y p)))

; can-move-left? : PuyoPair [List-of Puyo] -> Boolean
; is the cell to the left of each Puyo in the pair free?
(define (can-move-left? p lop)
  (and (can-move-left?/puyo (pair-p1 p) lop) (can-move-left?/puyo (pair-p2 p) lop)))

; can-move-left?/puyo : Puyo [List-of Puyo] -> Boolean
; is the cell to the left of the given Puyo free?
(define (can-move-left?/puyo p lop)
  (is-empty-cell? (- (puyo-x p) 1) (puyo-y p) lop))

; can-move-right? : PuyoPair [List-of Puyo] -> Boolean
; is the cell to the right of each Puyo in the pair free?
(define (can-move-right? p lop)
  (and (can-move-right?/puyo (pair-p1 p) lop) (can-move-right?/puyo (pair-p2 p) lop)))

; can-move-right?/puyo : Puyo [List-of Puyo] -> Boolean
; is the cell to the right of the given Puyo free?
(define (can-move-right?/puyo p lop)
  (is-empty-cell? (+ (puyo-x p) 1) (puyo-y p) lop))


; drop-pair/world : PairWorld -> GroundWorld
; immediately drop the floating PuyoPair as far as it can go
(define (drop-pair/world w)
  (make-world-g (append (drop-pair/pair (world-p-floating w) (world-p-grounded w))
                        (world-p-grounded w)) (world-p-score w)))

; drop-pair/pair : PuyoPair [List-of Puyo] -> [List-of Puyo]
; immediately drop the floating PuyoPair as far as it can go
(define (drop-pair/pair p lop)
  (local [(define lower (lower-puyo p))
          (define higher (higher-puyo p))
          (define dropped-lower (drop-puyo lower lop))]
  (cons dropped-lower (cons (drop-puyo higher (cons dropped-lower lop)) '()))))

; drop-puyo : Puyo [List-of Puyo] -> Puyo
; immediately drop the given Puyo as far as it can go
(define (drop-puyo p lop)
  (if (move-down?/puyo p lop)
      (drop-puyo (move-puyo-down p) lop) 
      p))

(define (lower-puyo p)
  (local [(define p1 (pair-p1 p))
          (define p2 (pair-p2 p))]
    (if (>= (puyo-y p1) (puyo-y p2))
        p1
        p2)))

(define (higher-puyo p)
  (local [(define p1 (pair-p1 p))
          (define p2 (pair-p2 p))]
    (if (< (puyo-y p1) (puyo-y p2))
        p1
        p2)))

; is-empty-cell? : Number Number [List-of Puyo] -> Boolean
; is the cell at the given (x, y) occupied by a Puyo in the list?
(define (is-empty-cell? x y lop)
  (and (< x GRID-WIDTH) (>= x 0) (< y GRID-HEIGHT) (>= y 0)
       (empty? (filter (λ (a-puyo) (and (= x (puyo-x a-puyo))
                                (= y (puyo-y a-puyo)))) lop))))

;-------------------- Rotate -----------------------------------

; rotate/world : PairWorld -> PairWorld
; rotates the floating PuyoPair if possible
(define (rotate/world w)
  (make-world-p (world-p-grounded w) (rotate/pair (world-p-floating w) (world-p-grounded w))
                (world-p-score w)))

; rotate/pair : PuyoPair [List-of Puyo] -> PuyoPair
; rotate the floating PuyoPair if possible
; details: increases the rot-idx of the pair and adjusts the puyos accordingly
; rot-idx is a number between 0 and 3 representing the 4 possible orientations of a Puyo Pair
; as follows:
; 0 -> p1 is above p2
; 1 -> p1 is to the left of p2
; 2 -> p1 is below p2
; 3 -> p1 is to the right of p2
(define (rotate/pair p lop)
  (cond
    [(= 0 (pair-rot-idx p)) (rot0-1 p lop 0)]
    [(= 1 (pair-rot-idx p)) (rot1-2 p lop 0)]
    [(= 2 (pair-rot-idx p)) (rot2-3 p lop 0)]
    [else (rot3-0 p lop 0)]))

; rot0-1 : PuyoPair [List-of Puyo] Number -> PuyoPair
; rotate the puyopair so that p1 is to the left of p2
(define (rot0-1 p lop count)
  (cond
    [(< count 4)
     (if (is-empty-cell? (- (puyo-x (pair-p2 p)) 1) (puyo-y (pair-p2 p)) lop)
         (make-pair
          (make-puyo (puyo-color (pair-p1 p)) (- (puyo-x (pair-p2 p)) 1) (puyo-y (pair-p2 p)))
          (pair-p2 p) 1)
         (rot1-2 p lop (+ 1 count)))]
    [else p]))

; rot1-2 : PuyoPair [List-of Puyo] Number -> PuyoPair
; rotate the puyopair so that p1 is below of p2
(define (rot1-2 p lop count)
  (cond
    [(< count 4)
     (if (is-empty-cell? (puyo-x (pair-p2 p)) (add1 (puyo-y (pair-p2 p))) lop)
         (make-pair
          (make-puyo (puyo-color (pair-p1 p)) (puyo-x (pair-p2 p)) (add1 (puyo-y (pair-p2 p))))
          (pair-p2 p) 2)
         (rot2-3 p lop (+ 1 count)))]
    [else p]))

; rot2-3 : PuyoPair [List-of Puyo] Number -> PuyoPair
; rotate the puyopair so that p1 is to the right of p2
(define (rot2-3 p lop count)
  (cond
    [(< count 4)
     (if (is-empty-cell? (+ (puyo-x (pair-p2 p)) 1) (puyo-y (pair-p2 p)) lop)
         (make-pair
          (make-puyo (puyo-color (pair-p1 p))(+ (puyo-x (pair-p2 p)) 1) (puyo-y (pair-p2 p)))
          (pair-p2 p) 3)
         (rot3-0 p lop (+ 1 count)))]
    [else p]))

; rot3-0 : PuyoPair [List-of Puyo] Number -> PuyoPair
; rotate the puyopair so that p1 is to the left of p2
(define (rot3-0 p lop count)
  (cond
    [(< count 4)
     (if (is-empty-cell? (puyo-x (pair-p2 p)) (sub1 (puyo-y (pair-p2 p))) lop)
         (make-pair
          (make-puyo (puyo-color (pair-p1 p))(puyo-x (pair-p2 p)) (sub1 (puyo-y (pair-p2 p))))
          (pair-p2 p) 0)
         (rot0-1 p lop (+ 1 count)))]
    [else p]))


; starts the game when program is run:
(define play (play-puyo blank-world))