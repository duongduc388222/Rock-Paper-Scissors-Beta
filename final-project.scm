;, RPS-prototype.scm
;; CSC 151 (Fa24)
;; Project: Rock Paper Scissors with Machine Learning 
;; Authors: Duc, Soma, Michael and Jake
;; Date: 2024-12-12
;; Acknowledgements:
;;   We acknoledge the help of mentors in the mentor sessions: 

; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------
; ----                             PART 0: IMPORT LIBRARIES                                ----
; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------

(import canvas)
(import image)
(import reactive)
(import html)

; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------
; ----                                PART 1: CONSTANT                                     ----
; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------


;;;(struct state (x1 y1 x2 y2 image1 image2 dx dy outcome running?))->()
;;; x1 : number? (x position of image 1)
;;; y2 : number? (y position of image 1)
;;; x2 : number? (x position of image 2)
;;; y2 : number? (y position of image 2)
;;; image1 : drawing? (Image for player move: Rock, Paper, Scissors)
;;; image2 : drawing? (Image for computer move: Rock, Paper, Scissors)
;;; dx : number? (function)
;;; dy : number? (function)
;;; outcome : number? (1, -1, 0)
;;; running? : boolean? 
;;; A structure used to set and modify canvas.

(struct state (x1 y1 x2 y2 image1 image2 dx dy outcome running?))

; The width of the canvas
(define width 500)

; The height of the canvas
(define height 500)

; The length of the shape
(define len 50)

; The velocity of the shape
(define velocity 5)

; The timer interval for updating our simulation (in milliseconds)
(define interval 25)

; A place holder value for miscellaneous usage
(define holder 10)

; A place holder image for miscellaneous usafe
(define non
  (solid-circle 0 "white"))

; The scissors graphic
(define scissors
  (let* ([handle (overlay 
                    (circle 15 "solid" "white") 
                    (circle 20 "solid" "blue"))]
        [handles (beside handle handle)]
        [blade (solid-rectangle 10 75 "gray")]
        [blades (beside (rotate -10 blade)
                        (rotate 10 blade))]
        [sciss (above blades handles)])
      sciss))

; The rock graphic
(define rock
  (solid-circle 45 "brown"))

; The paper graphic
(define paper
  (overlay
  (rectangle 90 120 "outline" "black")
  (rectangle 90 120 "solid" "white")))

; The starting screen graphic with directions
(define directions
                          (above/align "left" (text "Click on keys 'r' 'p' or 's' to choose Rock" 25 "black" "24px sans-serif")
                          (text "Paper or Scissors." 25 "black" "24px sans-serif")
                          (text "Watch the animation and click on" 25 "black" "24px sans-serif")
                          (text "Canvas to FIGHT AGAIN!!"  25 "black" "24px sans-serif")))

; The winning graphic
(define win (text "YOU WIN!" 50 "green" "24px sans-serif"))

; The losing graphic
(define lose (text "YOU LOSE!" 50 "red" "24px sans-serif"))

; The drawing graphic
(define draw (text "YOU DRAW!" 50 "black" "24px sans-serif"))



; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------
; ----                               PART 2: ALGORITHM                                     ----
; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------

;;; (moves-log)-> vector?
;;; Records the history of all moves made by the players. 

(define moves-log 
  (vector 0 0 0))

;;; (mean vec)  ->  number?
;;;    vec   :   vector? of moves log
;;; Returns the mean of the moves log.

(define mean
  (lambda (vec)
    (/ (+ (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)) (vector-length vec))))

;;; (sum-sq-resids vec)  ->  number?
;;;    vec   :   vector? of moves log
;;; Returns the squared sum residues of the moves log.

(define sum-sq-resids
  (lambda (vec)
    (+ (expt (- (vector-ref vec 0) (mean vec)) 2) (expt (- (vector-ref vec 1) (mean vec)) 2) (expt (- (vector-ref vec 2) (mean vec)) 2))))

;;; (varianve vec)  ->  number?
;;;    vec   :   vector? of moves log
;;; Returns the variance of the moves log.
(define variance
  (lambda (vec)
    (/ (sum-sq-resids vec) 2)))

;;; (sd vec)  ->  number?
;;;    vec   :   vector? of moves log
;;; Returns the standard deviation of the moves log.

(define sd
  (lambda (vec)
    (sqrt (variance vec))))


;;; (random-equal moves-list) -> moves-list
;;;       moves-list   :  list? of moves log
;;; Return a random list element given a list.
(define random-equal
  (lambda(moves-list)
    (list-ref moves-list 
      (random 
        (length moves-list)))))

;;; (max-val vec) -> string? of "Rock", "Paper" or "Scissors"
;;;      vec     :     vector? aka the moves log
;;; Return the move that is played the most.

(define max-val
  (lambda (vec)
    (let* ([rock     (vector-ref vec 0)]
           [paper    (vector-ref vec 1)]
           [scissors (vector-ref vec 2)])
    (cond
      [(and (= rock paper) (= paper scissors))
          (random-equal (list "Rock" "Paper" "Scissors"))]
      [(and (> rock scissors)
            (> rock paper))
          "Rock"]
      [(and (> scissors rock)
            (> scissors paper))
          "Scissors"]
      [(and (> paper rock)
            (> paper scissors))
          "Paper"]
      [(= rock paper)
          (random-equal (list "Rock" "Paper"))]
      [(= paper scissors)
          (random-equal (list "Paper" "Scissors"))]
      [else
          (random-equal (list "Scissors" "Rock"))]))))

;;; (min-val vec) -> string? of "Rock", "Paper" or "Scissors"
;;;      vec     :     vector? aka the moves log
;;; Return the move that is played the least.

(define min-val
  (lambda (vec)
    (let* ([rock     (vector-ref vec 0)]
           [paper    (vector-ref vec 1)]
           [scissors (vector-ref vec 2)])
    (cond
      [(and (= rock paper) (= paper scissors))
          (random-equal (list "Rock" "Paper" "Scissors"))]
      [(and (< rock scissors)
            (< rock paper))
          "Rock"]
      [(and (< scissors rock)
            (< scissors paper))
          "Scissors"]
      [(and (< paper rock)
            (< paper scissors))
          "Paper"]
      [(= rock paper)
          (random-equal (list "Rock" "Paper"))]
      [(= paper scissors)
          (random-equal (list "Paper" "Scissors"))]
      [else
          (random-equal (list "Scissors" "Rock"))]))))


;;; (strat vec) -> string? of "Rock", "Paper" or "Scissors"
;;;     vec    :     vector? aka the moves log
;;; Return a string that the computer thinks that the player is most likely to play based on
;;; the standard deviation (SD) of the count of Rock Paper and Scissors.
;;; If SD > 1, this means that the player has some bias in their moves, so we would predict
;;; that the player is more likely to play the moves that's been played the MOST.
;;;
;;; IF SD <= 1, this measn that the player has no strategy and is playing randomly, so they 
;;; would try to even the counts of moves out if they feel like they're playing a certain
;;; move too much. In this case, we will target the move that is LEAST played so far.

(define strat
  (lambda (vec)
    (let* ([std-dev (sd vec)])
      (if (<= std-dev 1)
          (min-val vec)
          (max-val vec)))))


;;; (outcome player-move comp-move)-> number?
;;; player-move : string? ("Rock" "Scissors" "Paper")
;;; comp-move : string? ("Rock" "Scissors" "Paper")
;;; Returns either 1, -1, or 0, depednding if the Player:Comp rps
;;; match-up is a win, lost, or draw, respectively
(define outcome
  (lambda(player-move comp-move)
    (match (pair player-move comp-move)
      [(pair "Rock" "Scissors")
          1]
      [(pair "Scissors" "Paper")
          1]
      [(pair "Paper" "Rock")
          1]
      [(pair "Scissors" "Rock")
          -1]
      [(pair "Paper" "Scissors")
          -1]
      [(pair "Rock" "Paper")
          -1]
      [else 0])))

;;; (comp-move vec)-> string?
;;; vec : vector? (move-log)
;;; Returns the AI's move decision
(define comp-move
  (lambda (vec)
    (let* ([move (strat vec)])
      (cond [(equal? move "Rock") "Paper"]
            [(equal? move "Paper") "Scissors"]
            [(equal? move "Scissors") "Rock"]))))

;;; (update-log player-move vec)-> void?
;;; player-move : string? ("Paper" "Rock" "Scissors")
;;; vec : vector? (moves-log)
;;; returns an updated log that either increments, decrements, or does nothing to a specified
;;; element of the vector
(define update-log
  (lambda (player-move vec)
      (cond [(equal? player-move "Rock")
               (vector-set! vec 0
                  (+ (vector-ref vec 0) 1))]
            [(equal? player-move "Paper")
               (vector-set! vec 1
                  (+ (vector-ref vec 1) 1))]
            [(equal? player-move "Scissors")
               (vector-set! vec 2
                  (+ (vector-ref vec 2) 1))])))

;;; (text->image str)-> drawing?
;;; str : string? ("Rock" "Paper" "Scissors")
;;; Returns an image based on the string input.
(define text->image
  (lambda (str)
    (cond
       [(equal? str "Rock")
        rock]
       [(equal? str "Paper")
        paper]
       [else scissors])))




; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------
; ----                                PART 3: GRAPHIC                                      ----
; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------

;;; (view st canv)-> void?
;;; st : struct?
;;; canv : canvas?
;;; Display the canvas after every update.

(define view 
  (lambda(st canv)
    (match st
      [(state x1 y1 x2 y2 image1 image2 dx dy outcome running?)
        (begin
           (canvas-drawing! canv 0 0 directions)
           (if running?
             (begin
              (canvas-drawing! canv x1 y1 image1)
              (canvas-drawing! canv x2 y2 image2))
              
              (canvas-drawing! canv x1 y1 image1)))])))

;;; (change-move st key)-> state?     ;helper function for update
;;; st : struct
;;; key : string? (from event-down-key)
;;; Changes properties of the vector 'moves-log' and the image properties of structure 'state' 
;;; Resets screen and update player-input into images in the canvas

(define change-move
  (lambda(st key)
    (let* ([cm  (comp-move moves-log)]
           [cm-image (text->image cm)]) 
             (cond 
                [(equal? "r" key)
                    (begin 
                      (update-log "Rock" moves-log)
                      (state 0 200 400 200 rock cm-image 10 0 (outcome "Rock" cm) #t))]
                [(equal? "p" key)
                    (begin 
                      (update-log "Paper" moves-log)
                      (state 0 200 400 200 paper cm-image 10 0 (outcome "Paper" cm) #t))]
                [(equal? "s" key)
                    (begin 
                      (update-log "Scissors" moves-log)
                      (state 0 200 400 200 scissors cm-image 10 0 (outcome "Scissors" cm) #t))]
                [else st]))))

;;; (result-canvas st oc)-> state?
;;; st : struct? 
;;; oc : number? (from outcome)
;;; Changes properties of the struct 'st' making the screen display the result after 
;;; a mouse click
(define result-canvas
  (lambda (st oc)
    (cond 
        [(= oc 1)
            (state 120 200 0 0 win non 0 0 holder #f)]
        [(= oc -1)
            (state 125 200 0 0 lose non 0 0 holder #f)]
        [(= oc 0)
            (state 100 200 0 0 draw non 0 0 holder #f)]
        [else st])))

;;; (update msg st canv)-> state?
;;; msg : subscription?
;;; st : struct?
;;; canv : canvas?
;;; Changes properties of the struct 'state depending on what actions happens: 
;;;  mouse-click or key-down
(define update
  (lambda (msg st canv)
    (match st
      [(state x1 y1 x2 y2 image1 image2 dx dy oc running?)
        (match msg
          [(event-timer time elapsed)
              (move-shape st canv)]
          [(event-key-down key)
              (change-move st key)]
          [(event-mouse-click button x y)
              (result-canvas st oc)]
          [else st])])))
                    
;;; (move-shape st canv)-> state?
;;; st : struct?
;;; canv : canvas?
;;; Changes the property coordinates properties (x1 y1 x2 y2) of struct 'state' 
;;; Making the winning move hits the losing move away and the drawing moves would bounce off 
;;; each other.

(define move-shape
  (lambda (st canv)
    (match st
      [(state x1 y1 x2 y2 image1 image2 dx dy outcome running?)
            (if (or  (> x1 (floor (/ width 3)))
                     (< x2 (floor (/ width 3))))
              (cond 
                [(= outcome 1)
                    (state (+ 30 (floor (/ width 3))) y1 (+ dx x2) (+ dy y2) image1 image2 50 50 outcome running?)]
                [(= outcome -1)
                    (state (+ dx x1) (+ dy y1) (- (floor (/ width 3)) 30) y2 image1 image2 -50 -50 outcome running?)]
                [(= outcome 0)
                    (state (+ -50 x1) y1 (+ 50 x2) y2 image1 image2 0 0 outcome running?)]
                [else st])
              (if running?
                (state (+ x1 dx) (+ y1 dy) (- x2 dx) (- y2 dy) image1 image2 dx dy outcome running?)
                st))])))

; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------
; ----                                 FINALE: RUNNING                                     ----
; ---------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------

(display
    (reactive-canvas
    width height
    (state 0 200 400 200 non non 0 0 holder #f)
    view
    update
   (on-timer interval)
   (on-key-down)
   (on-mouse-click)))







