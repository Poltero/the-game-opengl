;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; OpenGL 2.1 2d skeleton

(define level-number 1)

(define game-contents
  (call-with-input-file "LevelData.dat" (lambda (port) (read-all port))))

(define level-contents 'nothing)

(define set-level-contents! 
  (lambda (level)
    (set! level-contents (cdr (assq level game-contents)))))


(define logic-states 'none)

;Vars of control time
(define delta-time 0)
(define last-time 0)

;Level dimension
(define level-width 100000.0)
(define level-height 400.0)

(define level-final 0)

;Global position origin y
(define position-y-origin 0)


(define screen-width 1280.0)
(define screen-height 752.0)


(define add-background-menu-screen (lambda (px)
                        (set! vertex-data-vector 
                        (f32vector-append vertex-data-vector 
                                          (list->f32vector (list 0.5 0.0 (* px 0.15) 0.31
                                                                 0.5 (+ 0.0 700) (* px 0.15) 0.45
                                                                 (+ 0.5 1310) (+ 0.0 700) (* 0.15 (+ px 1)) 0.45
                                                                 (+ 0.5 1310) 0.0 (* 0.15 (+ px 1)) 0.31))))))




(define-structure tile posx posy width height)
(define-structure camera position state speed)
(define-structure enemy posx posy width height points type direction)
(define-structure player posx posy width height vstate hstate score)
(define-structure coin posx posy width height points color)
(define-structure world gamestates tiles camera player coins enemies)
(define vertex-data-vector '#f32())

;; Vars of number elements
(define number-of-tiles 0)
(define number-of-enemies 0)
(define number-of-coins 0)
(define max-count-x 0)


(define create-f32vector!
  (lambda (x y width height px py)
    (let ((vector 
           (f32vector x y (* px 0.014) 0.0
                      x (+ y height) (* px 0.014) 0.014
                      (+ x width) (+ y height) (* (+ px 1.0) 0.014) 0.014
                      (+ x width) y (* (+ px 1.0) 0.014) 0.0)))
      vector)))


(define set-element-in-vector!
  (lambda (index vector)
    (let recur ((count 0))
      (when (< count 16)
          (f32vector-set! vertex-data-vector (+ (* index 16) count) (f32vector-ref vector count))
          (recur (+ count 1))))))


(define set-player! 
  (lambda (player camera start)
    (set-element-in-vector!
     start
     (create-f32vector! 
      (exact->inexact (- (player-posx player) (camera-position camera)))
      (exact->inexact (player-posy player))
      (player-width player)
      (player-height player)
      2.9
      0.5))))

(define set-tiles! 
  (lambda (tiles camera start)
    (let set-tiles-in-vector! ((rest tiles) (count start))
      (when (not (null? rest))
            (set-element-in-vector!
             count
             (create-f32vector! 
              (exact->inexact (- (tile-posx (car rest)) (camera-position camera)))
              (exact->inexact (tile-posy (car rest)))
              (tile-width (car rest))
              (tile-height (car rest))
              1.0
              1.0))
            (set-tiles-in-vector! (cdr rest) (+ count 1))))))

(define set-enemies! 
  (lambda (enemies camera start)
    (let set-enemies-in-vector! ((rest enemies) (count start))
      (when (not (null? rest))
            (set-element-in-vector!
             count
             (create-f32vector! 
              (exact->inexact (- (enemy-posx (car rest)) (camera-position camera)))
              (exact->inexact (enemy-posy (car rest)))
              (enemy-width (car rest))
              (enemy-height (car rest))
              7.1
              0.5))
            (set-enemies-in-vector! (cdr rest) (+ count 1))))))

(define set-coins! 
  (lambda (coins camera start)
    (let set-coins-in-vector! ((rest coins) (count start))
      (when (not (null? rest))
            (set-element-in-vector!
             count
             (create-f32vector! 
              (exact->inexact (- (coin-posx (car rest)) (camera-position camera)))
              (exact->inexact (coin-posy (car rest)))
              (coin-width (car rest))
              (coin-height (car rest))
              0.5
              1.0))
            (set-coins-in-vector! (cdr rest) (+ count 1))))))


;Functions logic game

;;Check collsion player with something 

(define check-collision-player-with-generic 
  (lambda (player element element-posx element-posy element-width element-height)
    (let check-collision ((leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (element-posx element))
                          (rightB (+ (element-posx element) (element-width element)))
                          (topB (element-posy element))
                          (bottomB (+ (element-posy element) (element-height element))))
      (not (or (<= bottomA topB)
               (>= topA bottomB)
               (<= rightA leftB)
               (>= leftA rightB))))))

;; End [Check collsion player with something] 


;;Call different collision of player with something generic

(define check-collision-player-with-enemy
  (lambda (player enemy)
    (check-collision-player-with-generic player enemy enemy-posx enemy-posy enemy-width enemy-height)))

(define check-collision-player-with-coin
  (lambda (player coin)
    (check-collision-player-with-generic player coin coin-posx coin-posy coin-width coin-height)))

(define check-collision-player-with-finish
  (lambda (player finish)
    (check-collision-player-with-generic player finish finish-posx finish-posy finish-width finish-height)))

;; End [Call different collision of player with something]


;;Use functions collision of player with something

(define update-player-points-for-take-coin
  (lambda (player coins start)
    (let loop ((rest coins) (count start))
      (unless (null? rest)
              (if (check-collision-player-with-coin player (car rest))
                  (begin
                    (destroy-coin! (car rest))
                    (set-element-in-vector!
                     count (make-f32vector 16 0.0))
                    (player-score-set! player (+ (player-score player) (coin-points (car rest)))))
                  (loop (cdr rest) (+ count 1)))))))

(define destroy-coin!
  (lambda (coin)
    (coin-posx-set! coin 0.0)
    (coin-posy-set! coin 0.0)
    (coin-width-set! coin 0.0)
    (coin-height-set! coin 0.0)))

(define check-player-crash-enemy
  (lambda (player enemies)
    (let loop ((rest enemies))
      (unless (null? rest)
              (if (check-collision-player-with-enemy player (car rest))
                  #t
                  (loop (cdr rest)))))))

;; End [Use functions collision of player with something]


;;Collision bottom player

(define check-collision-bottom
  (lambda (player tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let check-collision (
                                    (leftA (player-posx player))
                                    (rightA (+ (player-posx player) (player-width player)))
                                    (bottomA (+ (player-posy player) (player-height player)))
                                    (leftB (tile-posx (car rest)))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest))))
                                    (rightB (+ (tile-posx (car rest)) (tile-width (car rest)))))
                (if (and (>= bottomA (- topB 6)) (< bottomA bottomB) (>= rightA leftB) (<= leftA rightB))
                    (player-posy-set! player (- topB 31))
                    (loop (cdr rest))))))))

(define check-collision-top
  (lambda (player tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let check-collision (
                                    (leftA (player-posx player))
                                    (rightA (+ (player-posx player) (player-width player)))
                                    (bottomA (+ (player-posy player) (player-height player)))
                                    (topA (player-posy player))
                                    (leftB (tile-posx (car rest)))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest))))
                                    (rightB (+ (tile-posx (car rest)) (tile-width (car rest)))))
                (if (and (<= topA (+ bottomB 12)) (> topA topB) (>= rightA leftB) (<= leftA rightB))
                    #t
                    (loop (cdr rest))))))))

(define check-collision-bottom-player-with-enemy
  (lambda (player enemy)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (enemy-posx enemy))
                          (topB (enemy-posy enemy))
                          (bottomB (+ (enemy-posy enemy) (enemy-height enemy)))
                          (rightB (+ (enemy-posx enemy) (enemy-width enemy))))
      (and (> bottomA (- topB 20)) (< bottomA bottomB) (>= rightA (+ leftB 10)) (<= leftA (- rightB 10))))))


;;Collision bottom enemy

(define check-collision-bottom-enemy
  (lambda (enemy tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (when (<  (- (tile-posx (car rest)) (enemy-posx enemy) 200))
                    (let check-collision (
                                          (leftA (enemy-posx enemy))
                                          (rightA (+ (enemy-posx enemy) (enemy-width enemy)))
                                          (bottomA (+ (enemy-posy enemy) (enemy-height enemy)))
                                          (leftB (tile-posx (car rest)))
                                          (topB (tile-posy (car rest)))
                                          (bottomB (+ (tile-posy (car rest)) (tile-height (car rest))))
                                          (rightB (+ (tile-posx (car rest)) (tile-width (car rest)))))
                      (if (and (> bottomA (- topB 1)) (< bottomA bottomB) (>= rightA leftB) (<= leftA rightB))
                          #t
                          (loop (cdr rest)))))))))


(define collision-down-tiles-enemy
  (lambda (enemy tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (enemy-posx enemy) (tile-posx (car rest)))
                   (> (+ (enemy-posx enemy) 40) (tile-posx (car rest))))
               (< (enemy-posx enemy) (+ (tile-posx (car rest)) 40))
               (> (enemy-posy enemy) (- (tile-posy (car rest)) 39))
               (< (enemy-posy enemy) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))

;;Collsion tiles left of player

(define check-collision-left-tiles
  (lambda (player tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let ckeck-collision (
                                    (rightA (+ (player-posx player) (player-width player)))
                                    (topA (player-posy player))
                                    (bottomA (+ (player-posy player) (player-height player)))
                                    (leftB (tile-posx (car rest)))
                                    (leftA (player-posx player))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest)))))
                (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (>= rightA (- leftB 13)) (<= leftA leftB))
                    (player-posx-set! player (- leftB 31))
                    (loop (cdr rest))))))))

(define check-collision-right-tiles
  (lambda (player tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let ckeck-collision (
                                    (rightA (+ (player-posx player) (player-width player)))
                                    (topA (player-posy player))
                                    (bottomA (+ (player-posy player) (player-height player)))
                                    (leftB (tile-posx (car rest)))
                                    (rightB (+ (tile-posx (car rest)) (tile-width (car rest))))
                                    (leftA (player-posx player))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest)))))
                (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (<= leftA (+ rightB 13)) (>= leftA leftB))
                    (player-posx-set! player (+ rightB 1))
                    (loop (cdr rest))))))))

;;Collsion tiles left of enemy

(define check-collision-left-enemies
  (lambda (player enemies)
    (let loop ((rest enemies))
      (unless (null? rest)
              (if (< (abs (- (enemy-posx (car rest)) (player-posx player))) 200)
                  (let ckeck-collision (
                                        (rightA (+ (player-posx player) (player-width player)))
                                        (topA (player-posy player))
                                        (bottomA (+ (player-posy player) (player-height player)))
                                        (leftB (enemy-posx (car rest)))
                                        (leftA (player-posx player))
                                        (topB (enemy-posy (car rest)))
                                        (bottomB (+ (enemy-posy (car rest)) (enemy-height (car rest)))))
                    (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (>= rightA (- leftB 1)) (<= leftA leftB))
                        #t
                        (loop (cdr rest))))
                  (loop (cdr rest)))))))


;;Collision tiles left of enemy
(define check-collision-left-tiles-enemy
  (lambda (enemy tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let ckeck-collision (
                                    (rightA (+ (enemy-posx enemy) (enemy-width enemy)))
                                    (topA (enemy-posy enemy))
                                    (bottomA (+ (enemy-posy enemy) (enemy-height enemy)))
                                    (leftB (tile-posx (car rest)))
                                    (leftA (enemy-posx enemy))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest)))))
                (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (>= rightA (- leftB 13)) (<= leftA leftB))
                    #t
                    (loop (cdr rest))))))))




;;Collsion tiles right of enemy
(define check-collision-right-enemies
  (lambda (player enemies)
    (let loop ((rest enemies))
      (unless (null? rest)
              (if (< (abs (- (enemy-posx (car rest)) (player-posx player))) 200)
                  (let ckeck-collision (
                                        (rightA (+ (player-posx player) (player-width player)))
                                        (topA (player-posy player))
                                        (bottomA (+ (player-posy player) (player-height player)))
                                        (leftB (enemy-posx (car rest)))
                                        (rightB (+ (enemy-posx (car rest)) (enemy-width (car rest))))
                                        (leftA (player-posx player))
                                        (topB (enemy-posy (car rest)))
                                        (bottomB (+ (enemy-posy (car rest)) (enemy-height (car rest)))))
                    (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (<= leftA (+ rightB 1)) (>= leftA leftB))
                        #t
                        (loop (cdr rest))))
                  (loop (cdr rest)))))))

;;Collision tiles right of enemy
(define check-collision-right-tiles-enemy
  (lambda (enemy tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let ckeck-collision (
                                    (rightA (+ (enemy-posx enemy) (enemy-width enemy)))
                                    (topA (enemy-posy enemy))
                                    (bottomA (+ (enemy-posy enemy) (enemy-height enemy)))
                                    (leftB (tile-posx (car rest)))
                                    (rightB (+ (tile-posx (car rest)) (tile-width (car rest))))
                                    (leftA (enemy-posx enemy))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest)))))
                (if (and  (>= bottomA (+ topB 5)) (<= topA bottomB) (<= leftA (+ rightB 20)) (>= leftA leftB))
                    #t
                    (loop (cdr rest))))))))



;; Util function for sort conditions
(define condition-sort
  (lambda (condition values)
    (if condition
        (car values)
        (car (cdr values)))))

;; End




;; Logic for generations maps

(define (create-tiles-map l)
  (let loop ((rest-map (cdr (assq 'map level-contents))) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (let create-plataforms ((element (vector-ref (vector-ref rest-map count-y) count-x)))
            (if (or (eq? element 1) (eq? element '+) (eq? element '-))
                (let create-plataform-normal ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 4)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-normal (+ number 1) (+ posx 39))))))
            (if (or (eq? element 2) (eq? element '+++))
                (let create-plataform-double ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (if (< number 8)
                      (begin
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-double (+ number 1) (+ posx 39))))))
            (if (or (eq? element 'i) (eq? element '++))
                (let create-unique-plataform ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))))

            (if (eq? element '|--|)
                (let create-plataform-for-enemy ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (when (= number 0)
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 99)) 40.0 40.0) rest))
                        (create-plataform-for-enemy (+ number 1) (+ posx 39)))
                  (when (and (> number 0) (< number 7))
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-for-enemy (+ number 1) (+ posx 39)))
                  (when (= number 7)
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 100)) 40.0 40.0) rest)))))
            (if (eq? element 'final)
                (let create-final-level ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
                  (set! level-final posx))))
          (if (< count-x max-count-x)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-coins-map l)
  (let loop ((rest-map (cdr (assq 'map level-contents))) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((+)
             (let create-plataform-with-coins ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 4)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-sort (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins (+ number 1) (+ posx 40))))))
            ((++)
             (let create-plataform-with-coins-special ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 1)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-sort (< count-y 2) '(88 102)) )) 15.0 15.0 50 'green) rest))
                     (create-plataform-with-coins-special (+ number 1) (+ posx 40))))))
            ((|--|)
             (let create-plataform-with-coins-doubles ((number 0) (posx (+ (+ 0 (* 50 4)) (* count-x 100))))
               (if (< number 6)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-sort (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins-doubles (+ number 1) (+ posx 40))))))

            ((+++)
             (let create-plataform-with-coins-doubles ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 8)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-sort (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins-doubles (+ number 1) (+ posx 40)))))))
          (if (< count-x max-count-x)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-enemies-map l)
  (let loop ((rest-map (cdr (assq 'map level-contents))) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((*)
             (let create-enemy-kamikaze ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (set! rest (cons (make-enemy (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 99)) 30.0 30.0 10 'kamikaze 'none) rest))))
            ((*+)
             (let create-enemy-defender ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (set! rest (cons (make-enemy (exact->inexact (+ posx 50)) (exact->inexact (* (+ 0.7 count-y) 99)) 40.0 40.0 10 'defender 'left) rest)))))
          (if (< count-x max-count-x)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

;; End [Logic for generations maps]

;, End [Logic-functions]


(define vertex-shader #<<end-of-shader

#version 120
attribute vec2 position;
attribute vec2 texCoord;

varying vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
  colorCoord = texCoord;
  gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 120

varying vec2 colorCoord;
uniform sampler2D colorTexture;

void main()
{
  gl_FragColor = texture2D(colorTexture, colorCoord);
}

end-of-shader
)


(define (main)
  (let ((init-screen-width 1280)
        (init-screen-height 752)
        (screen-width* (alloc-int* 1))
        (screen-height* (alloc-int* 1)))
    (when (< (SDL_Init SDL_INIT_VIDEO) 0) report: (fusion:error "Couldn't initialize SDL!"))
    ;; SDL
    (let ((win (SDL_CreateWindow
                ""
                SDL_WINDOWPOS_CENTERED
                SDL_WINDOWPOS_CENTERED
                (cond-expand (mobile 0) (else init-screen-width))
                (cond-expand (mobile 0) (else init-screen-height))
                SDL_WINDOW_OPENGL)))
      (unless win (fusion:error "Unable to create render window" (SDL_GetError)))
      (SDL_GetWindowSize win screen-width* screen-height*)
      (let ((screen-width (*->int screen-width*))
            (screen-height (*->int screen-height*))
            (ctx (SDL_GL_CreateContext win)))
        (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
        ;; OpenGL
        (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)


        ;;Start sound mixer
        (unless (= 0 (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT 2 1024))
                (fusion:error (string-append "Unable to initialize sound system -- " (Mix_GetError)))) 

        ;; Generate programs, buffers, textures
        (let* ((perspective-matrix (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                                             (matrix:* (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)
                                                       (make-identity-matrix))))
               (position-buffer-object-id* (alloc-GLuint* 1))
               (main-vao-id* (alloc-GLuint* 1))
               (surface-id* (alloc-GLuint* 1))
               (texture-id* (alloc-GLuint* 1))
               (texture-unit 0)
               (sampler-id* (alloc-GLuint* 1))
               ;; (vertex-data-vector '#f32(
               ;;                           50.0 50.0 0.0 0.0
               ;;                           150.0 50.0 0.0 1.0
               ;;                           150.0 145.0 1.0 1.0
               ;;                           50.0 145.0 1.0 0.0


               ;;                           280.0 50.0 0.0 0.0
               ;;                           380.0 50.0 0.0 1.0
               ;;                           380.0 145.0 1.0 1.0
               ;;                           280.0 145.0 1.0 0.0))
               (vertex-data (f32vector->GLfloat* vertex-data-vector))
               (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                              (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
               (shader-program (fusion:create-program shaders))
               (texture-image* (IMG_Load "assets/template.png"))
               
               ;;Background Music
               (background-music* 'nothing))
          
          
          
          

          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)

          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_RGBA GL_UNSIGNED_BYTE
                        (SDL_Surface-pixels texture-image*))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
          (glBindTexture GL_TEXTURE_2D 0)
          (SDL_FreeSurface texture-image*)

          ;; Uniforms
          (glUseProgram shader-program)
          (glUniformMatrix4fv (glGetUniformLocation shader-program "perspectiveMatrix")
                              1 GL_FALSE
                              (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           perspective-matrix)))
          (glUniform1i (glGetUniformLocation shader-program "colorTexture") texture-unit)
          (glUseProgram 0)

          ;; Sampler
          (glGenSamplers 1 sampler-id*)
          (let ((sampler-id (*->GLuint sampler-id*)))
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST))
          


          ;; Vertex Array Object
          (glGenBuffers 1 position-buffer-object-id*)
          (let ((position-buffer-object-id (*->GLuint position-buffer-object-id*)))
            ;; Upload buffer
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            (glBufferData GL_ARRAY_BUFFER
                          (* (f32vector-length vertex-data-vector) GLfloat-size)
                          vertex-data
                          GL_DYNAMIC_DRAW)
            ;; Create VAO
            (glGenVertexArrays 1 main-vao-id*)
            (glBindVertexArray (*->GLuint main-vao-id*))
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            
            (let ((position-attr (glGetAttribLocation shader-program "position"))
                  (texture-coordinates-attr (glGetAttribLocation shader-program "texCoord")))
              (glEnableVertexAttribArray position-attr)
              (glVertexAttribPointer position-attr 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray texture-coordinates-attr)
              (glVertexAttribPointer texture-coordinates-attr 2
                                     GL_FLOAT GL_FALSE
                                     (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size))))
            
            (glBindBuffer GL_ARRAY_BUFFER 0)
            (glBindVertexArray 0)
            
            
            

            

            
            
            
            ;;(pp (vector-length (vector-ref world-map 0)))
            
            ;; Game loop
            (let ((event* (alloc-SDL_Event)))
              (call/cc
               (lambda (quit)
                 (let main-loop ((world (make-world 'splashscreen '() 'none (make-player 0.0 0.0 0.0 0.0 'none 'none 0) '() '())) (time (SDL_GetTicks)))
                   (set! delta-time (- time last-time))
                   (set! last-time time)
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event*)))
                             (cond
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))

                                       ((= key SDLK_RIGHT)
                                        (if (eq? (world-gamestates world) 'gamescreen)
                                            (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                    (make-player
                                                                     (player-posx (world-player world))
                                                                     (player-posy (world-player world))
                                                                     (player-width (world-player world))
                                                                     (player-height (world-player world))
                                                                     'right
                                                                     (player-hstate (world-player world))
                                                                     (player-score (world-player world)))
                                                                    (world-coins world)
                                                                    (world-enemies world)))))
                                       
                                       ((= key SDLK_LEFT)
                                        (if (eq? (world-gamestates world) 'gamescreen)
                                            (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                    (make-player
                                                                     (player-posx (world-player world))
                                                                     (player-posy (world-player world))
                                                                     (player-width (world-player world))
                                                                     (player-height (world-player world))
                                                                     'left
                                                                     (player-hstate (world-player world))
                                                                     (player-score (world-player world)))
                                                                    (world-coins world)
                                                                    (world-enemies world)))))


                                       ((= key SDLK_UP)
                                        (if (eq? (world-gamestates world) 'gamescreen)
                                            (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                    (make-player
                                                                     (player-posx (world-player world))
                                                                     (player-posy (world-player world))
                                                                     (player-width (world-player world))
                                                                     (player-height (world-player world))
                                                                     (player-vstate (world-player world))
                                                                     'up
                                                                     (player-score (world-player world)))
                                                                    (world-coins world)
                                                                    (world-enemies world)))))
                                       
                                       ((= key SDLK_RETURN)
                                        (when (or (eq? (world-gamestates world) 'splashscreen) (eq? (world-gamestates world) 'lose))
                                              (set! logic-states 'start)))
                                       
                                       
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              

                              ((= event-type SDL_KEYUP)
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond 
                                  ((= key SDLK_RIGHT)
                                   (if (eq? (world-gamestates world) 'gamescreen)
                                       (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                               (make-player
                                                                (player-posx (world-player world))
                                                                (player-posy (world-player world))
                                                                (player-width (world-player world))
                                                                (player-height (world-player world))
                                                                'none
                                                                (player-hstate (world-player world))
                                                                (player-score (world-player world)))
                                                               (world-coins world)
                                                               (world-enemies world)))))

                                  ((= key SDLK_LEFT)
                                   (if (eq? (world-gamestates world) 'gamescreen)
                                       (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                               (make-player
                                                                (player-posx (world-player world))
                                                                (player-posy (world-player world))
                                                                (player-width (world-player world))
                                                                (player-height (world-player world))
                                                                'none
                                                                (player-hstate (world-player world))
                                                                (player-score (world-player world)))
                                                               (world-coins world)
                                                               (world-enemies world)))))

                                  ((= key SDLK_UP)
                                   (if (eq? (world-gamestates world) 'gamescreen)
                                       (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                               (make-player
                                                                (player-posx (world-player world))
                                                                (player-posy (world-player world))
                                                                (player-width (world-player world))
                                                                (player-height (world-player world))
                                                                (player-vstate (world-player world))
                                                                (player-hstate (world-player world))
                                                                (player-score (world-player world)))
                                                               (world-coins world)
                                                               (world-enemies world)))))
                                  
                                  (else
                                   (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))
                   
                   
                   
                   (when (eq? logic-states 'start)
                         (set-level-contents! level-number)
                         (set! max-count-x (- (vector-length (vector-ref (cdr (assq 'map level-contents)) 0)) 1))

                         (pp (cdr (assq 'camera level-contents)))
                         
                         (set! world (make-world 
                                      'gamescreen
                                      (create-tiles-map (world-tiles world))
                                      (make-camera 0.0 (cdr (assq 'camera level-contents)) 0.1)
                                      (make-player 400.0 430.0 30.0 30.0 'none 'down 0)
                                      (create-coins-map (world-coins world))
                                      (create-enemies-map (world-enemies world))))
                         
                         (set! vertex-data-vector (make-f32vector (* (+ (length (world-tiles world))
                                                                        (length (world-coins world))
                                                                        (length (world-enemies world))
                                                                        1)
                                                                     16)
                                                                  0.0))
                         
                         
                         ;;Inicializar todos los datos del vector
                         (let* ((count 0) 
                                (player (world-player world)) 
                                (tiles (world-tiles world)) 
                                (enemies (world-enemies world)) 
                                (coins (world-coins world)))
                           
                           (set-element-in-vector! 
                            count 
                            (create-f32vector!
                             (- (player-posx player) (camera-position (world-camera world)))
                             (player-posy player) 
                             (player-width player) 
                             (player-height player)
                             6.0
                             0.0))
                           
                           (set-tiles! 
                            tiles (world-camera world) 1)
                           (set-enemies! 
                            enemies (world-camera world) (+ (length tiles) 1))
                           (set-coins! 
                            coins (world-camera world) (+ (length tiles) (length enemies) 1)))
                         
                         (set! logic-states 'none)

                         ;;Empieza la musica de fondo


                         (set! background-music* (or (Mix_LoadMUS (string-append "assets/background" (number->string level-number) ".ogg"))
                                                     (fusion:error (string-append "Unable to load OGG music -- " (Mix_GetError)))))

                         (if (= 0 (Mix_PlayingMusic))
                             (unless (Mix_FadeInMusic background-music* -1 1000)
                                     (fusion:error (string-append "Unable to play OGG music -- " (Mix_GetError))))
                             (Mix_FadeOutMusic 1000)))


                   
                   
                   (case (world-gamestates world)
                     ((splashscreen)
                      
                      
                      (add-background-menu-screen 0.0))

                     

                     ((win)
                      ;;Reset list of world
                      (world-tiles-set! world '())
                      (world-enemies-set! world '())
                      (world-coins-set! world '())
                      
                      (set! level-number (+ level-number 1))
                      (set! vertex-data-vector '#f32())
                      (set! logic-states 'start)

                      (Mix_FreeMusic background-music*))

                     
                     ((lose)
                      (add-background-menu-screen 2.93)
                      
                      ;;Reset list of the world
                      (world-tiles-set! world '())
                      (world-enemies-set! world '())
                      (world-coins-set! world '())


                      ;; Stop music
                      (Mix_FadeOutMusic 1000)
                      
                      )

                     
                     ((gamescreen)
                      

                      ;;Logic Events
                      


                      ;;Move player to left
                      (let* ((player (world-player world)) (tiles (world-tiles world)) (camera (world-camera world)))
                        (if (eq? (player-vstate (world-player world)) 'left)
                            (if (not (check-collision-right-tiles player tiles))
                                (begin
                                  (player-posx-set! player (- (player-posx player) (* 0.3 delta-time)))
                                  (if (eq? (camera-state camera) 'on)
                                      (camera-position-set! camera (- (camera-position camera) (* 0.3 delta-time))))
                                  
                                  
                                  (set-player! 
                                   player camera 0)
                                  (set-tiles!
                                   tiles camera 1)
                                  (set-coins!
                                   (world-coins world) camera (+ (length (world-tiles world)) 1 (length (world-enemies world)))))
                                (set-player! 
                                 player (world-camera world) 0))))
                      

                      ;;Move player to right
                      (let* ((player (world-player world)) (tiles (world-tiles world)) (camera (world-camera world)))
                        (if (eq? (player-vstate player) 'right)
                            (if (not (check-collision-left-tiles player tiles))
                                (begin
                                  (player-posx-set! player (+ (player-posx player) (* 0.3 delta-time)))
                                  (if (eq? (camera-state camera) 'on)
                                      (camera-position-set! camera (+ (camera-position camera) (* 0.3 delta-time))))
                                  
                                  
                                  (set-player! 
                                   player camera 0)
                                  (set-tiles!
                                   tiles camera 1)
                                  (set-coins!
                                   (world-coins world) camera (+ (length (world-tiles world)) 1 (length (world-enemies world)))))
                                (set-player! 
                                 player (world-camera world) 0))))
                      

                      ;;Manage states up's
                      (if (eq? (player-hstate (world-player world)) 'up) 
                          (if (check-collision-bottom (world-player world) (world-tiles world))
                              (player-hstate-set! (world-player world) 'jump)
                              (player-hstate-set! (world-player world) 'down)))

                      ;;Move player to up
                      (let* ((player (world-player world)))
                        (if (eq? (player-hstate player) 'jump) 
                            (if (not (check-collision-top (world-player world) (world-tiles world)))      
                                (begin
                                  (player-posy-set! player (- (player-posy player) (* 0.3 delta-time)))
                                  (set-player! (world-player world) (world-camera world) 0))
                                (player-hstate-set! (world-player world) 'down))))


                      ;;Move player to down
                      (let* ((player (world-player world)))
                        (if (eq? (player-hstate player) 'down)
                            (if (not (check-collision-bottom (world-player world) (world-tiles world)))
                                (begin
                                  (player-posy-set! player (+ (player-posy player) (* 0.3 delta-time)))
                                  (set-player! (world-player world) (world-camera world) 0)) 
                                (set-player! player (world-camera world) 0))))
                      
                      ;;(pp (check-collision-bottom (world-player world) (world-tiles world)))

                      
                                        ;Control limits jump
                      (if (> position-y-origin (player-posy (world-player world)))
                          (player-hstate-set! (world-player world) 'down))
                      
                      
                      ;;Set camera
                      (when (eq? (camera-state (world-camera world)) 'auto)
                            (camera-position-set! 
                             (world-camera world) (+ (camera-position (world-camera world)) (* (camera-speed (world-camera world)) delta-time)))
                            (set-player! (world-player world) (world-camera world) 0)
                            (set-tiles! (world-tiles world) (world-camera world) 1)
                            (set-coins! (world-coins world) (world-camera world) (+ (length (world-tiles world)) 1 (length (world-enemies world)))))

                      

                      ;;keep the camera in bounds
                      (if (< (camera-position (world-camera world)) 0)
                          (camera-position-set! (world-camera world) 0))

                      
                      (if (> (camera-position (world-camera world)) (- level-final 640))
                          (world-gamestates-set! world 'win))


                                        ;Calculate collision with coins
                      (update-player-points-for-take-coin 
                       (world-player world) (world-coins world) (+ (length (world-tiles world)) 1 (length (world-enemies world))))



                      (let process-enemies ((rest (world-enemies world)))
                        (when (not (null? rest))
                              (when (and 
                                     (< (abs 
                                         (- (enemy-posx (car rest)) (player-posx (world-player world)))) 1280) 
                                     (< (player-posx (world-player world)) (+ (enemy-posx (car rest)) 500)))
                                    (case (enemy-type (car rest))
                                      ((kamikaze)
                                       (if (not (check-collision-bottom-enemy (car rest) (world-tiles world)))
                                           (begin (if (not (check-collision-right-tiles-enemy (car rest) (world-tiles world)))
                                                      (enemy-posx-set! (car rest) (- (enemy-posx (car rest)) (* 0.1 delta-time)))
                                                      (enemy-posy-set! (car rest) (+ (enemy-posx (car rest)) (* 0.1 delta-time))))
                                                  (enemy-posy-set! (car rest) (+ (enemy-posy (car rest)) (* 0.1 delta-time))))
                                           (enemy-posx-set! (car rest) (- (enemy-posx (car rest)) (* 0.1 delta-time))))
                                       
                                       (set-enemies! (world-enemies world) (world-camera world) (+ (length (world-tiles world)) 1)))
                                      
                                      ((defender)
                                       (if (not (check-collision-bottom-enemy (car rest) (world-tiles world)))
                                           (enemy-posy-set! (car rest) (+ (enemy-posy (car rest)) (* 0.1 delta-time))))
                                       (if (check-collision-right-tiles-enemy (car rest) (world-tiles world))
                                           (enemy-direction-set! (car rest) 'right)
                                           (if (check-collision-left-tiles-enemy (car rest) (world-tiles world))
                                               (enemy-direction-set! (car rest) 'left)))
                                       (if (eq? (enemy-direction (car rest)) 'right)
                                           (enemy-posx-set! (car rest) (+ (enemy-posx (car rest)) (* 0.1 delta-time)))
                                           (if (eq? (enemy-direction (car rest)) 'left)
                                               (enemy-posx-set! (car rest) (- (enemy-posx (car rest)) (* 0.1 delta-time)))))
                                       
                                       (set-enemies! (world-enemies world) (world-camera world) (+ (length (world-tiles world)) 1)))))
                              
                              
                              (process-enemies (cdr rest))))
                      
                      

                      
                      ;; ;;Kill enemies
                      (let kill-enemies ((rest (world-enemies world)) (count (+ (length (world-tiles world)) 1)))
                        (unless (null? rest)
                                (if  (< (abs (- (enemy-posx (car rest)) (player-posx (world-player world)))) 100)
                                     (begin 
                                       (if (check-collision-bottom-player-with-enemy (world-player world) (car rest))
                                           (begin
                                             (enemy-posx-set! (car rest) -20.0)
                                             (set-element-in-vector!
                                              count
                                              (create-f32vector! 
                                               0.0
                                               0.0
                                               0.0
                                               0.0
                                               0.0
                                               0.0))
                                             (kill-enemies (cdr rest) (+ count 1)))
                                           (kill-enemies (cdr rest) (+ count 1)))))
                                (kill-enemies (cdr rest) (+ count 1))))

                      

                      ;;Player lost
                      (when (or (check-collision-left-enemies (world-player world) (world-enemies world)) (check-collision-right-enemies (world-player world) (world-enemies world)))
                            (world-gamestates-set! world 'lose)
                            (set! vertex-data-vector '#f32()))

                      (when (< (- (player-posx (world-player world)) (camera-position (world-camera world))) (* -1 (player-width (world-player world))))
                            (world-gamestates-set! world 'lose)
                            (set! vertex-data-vector '#f32()))

                      (when (> (player-posy (world-player world)) 750)
                            (world-gamestates-set! world 'lose)
                            (set! vertex-data-vector '#f32()))))





                   ;;-- Draw
                   (glClearColor 0.0 0.0 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)
                   
                   (glActiveTexture (+ GL_TEXTURE0 texture-unit))
                   (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
                   (glBindSampler texture-unit (*->GLuint sampler-id*))
                      
                      ;; Begin VAO
                     
                   
                   (glBindVertexArray (*->GLuint main-vao-id*))
                      ;; Update vertex data buffer
                     
                   
                   (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
                      #;
                      (glBufferSubData GL_ARRAY_BUFFER
                      0
                      (* (f32vector-length vertex-data-vector) GLfloat-size)
                     (f32vector->GLfloat* vertex-data-vector))
                   (glBufferData GL_ARRAY_BUFFER
                                 (* (f32vector-length vertex-data-vector) GLfloat-size)
                                 (f32vector->GLfloat* vertex-data-vector)
                                 GL_DYNAMIC_DRAW)
                   
                   (glUseProgram shader-program)
                   (glDrawArrays GL_QUADS 0 (/ (f32vector-length vertex-data-vector) 4))
                   (glUseProgram 0)
                   (glBindVertexArray 0)
                      ;; End VAO
                     
                   
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop world (SDL_GetTicks)))
                 ))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

