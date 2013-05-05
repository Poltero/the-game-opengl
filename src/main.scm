;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; OpenGL 2.1 2d skeleton

;Vars of control time
(define delta-time 0)
(define last-time 0)

;Level dimension
(define level-width 10000.0)
(define level-height 400.0)

;Global position origin y
(define position-y-origin 0)

;Map level
(define world-map '#(#(0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 0 0 0 1 0 0 + 0 0 0 0 0 0 0 0 ++ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(0 0 0 0 0 0 0 0 +++ 0 0 + 0 0 0 0 0 0 + i i i 0 0 * 0 0 * 0 0 0 0 0 0 0 0 0 0 + 0 0 0 0 0 s 0 0 0 + 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+ 0 1 0 0 0 0 0 0 0 0 * i i i 0 * * 0 i 0 0 0 0 0 0 +++ 1 1 + 1 i 0 0 0 0 0 0 0 0 0 0 0 0 |--| 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 * * * * * * * * 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         #(+++ + 1 1 * 1 1 ++ ++ + 0 0 + 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 ++ 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 + 1 1 1 1 1 1 1 1 1 1 1 1 1)
                         ))


(define-structure tile posx posy width height)
(define-structure camera position state speed)
(define-structure player posx posy width height vstate hstate score)
(define-structure coin posx posy width height points color)
(define-structure world gamestates tiles camera player coins)
(define vertex-data-vector '#f32())


(define add-element-to-vector!
  (lambda (x y width height)
    (set! vertex-data-vector
          (f32vector-append vertex-data-vector (list->f32vector (list x y 0.0 0.0
                                                                     (+ x width) y 1.0 0.0
                                                                     (+ x width) (+ y height) 1.0 1.0
                                                                     x (+ y height) 0.0 1.0))))))

;Functions logic game

(define check-collision-player-with-coin
  (lambda (player coin)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (coin-posx coin))
                          (rightB (+ (coin-posx coin) (coin-width coin)))
                          (topB (coin-posy coin))
                          (bottomB (+ (coin-posy coin) (coin-height coin))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))

(define check-collision-player-with-enemy
  (lambda (player enemy)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (enemy-posx enemy))
                          (rightB (+ (enemy-posx enemy) (enemy-width enemy)))
                          (topB (enemy-posy enemy))
                          (bottomB (+ (enemy-posy enemy) (enemy-height enemy))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))

(define check-collision-player-with-finish
  (lambda (player finish)
    (let check-collision (
                          (leftA (player-posx player))
                          (rightA (+ (player-posx player) (player-width player)))
                          (topA (player-posy player))
                          (bottomA (+ (player-posy player) (player-height player)))
                          (leftB (finish-posx finish))
                          (rightB (+ (finish-posx finish) (finish-width finish)))
                          (topB (finish-posy finish))
                          (bottomB (+ (finish-posy finish) (finish-height finish))))
      (if (<= bottomA topB)
          #f
          (if (>= topA bottomB)
              #f
              (if (<= rightA leftB)
                  #f
                  (if (>= leftA rightB)
                      #f
                      #t)))))))



(define update-player-points-for-take-coin
  (lambda (player coins)
    (let loop ((rest coins))
      (unless (null? rest)
              (if (check-collision-player-with-coin player (car rest))
                  (begin
                    (coin-posx-set! (car rest) -20)
                    (player-score-set! player (+ (player-score player) (coin-points (car rest)))))
                  (loop (cdr rest)))))))

(define check-player-crash-enemy
  (lambda (player enemies)
    (let loop ((rest enemies))
      (unless (null? rest)
              (if (check-collision-player-with-enemy player (car rest))
                  #t
                  (loop (cdr rest)))))))


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
                (if (and (> bottomA (- topB 16)) (< bottomA bottomB) (>= rightA leftB) (<= leftA rightB))
                    #t
                    (loop (cdr rest))))))))


;;Collision bottom enemy

(define check-collision-bottom-enemy
  (lambda (enemy tiles)
    (let loop ((rest tiles))
      (unless (null? rest)
              (let check-collision (
                                    (leftA (enemy-posx enemy))
                                    (rightA (+ (enemy-posx enemy) (enemy-width enemy)))
                                    (bottomA (+ (enemy-posy enemy) (enemy-height enemy)))
                                    (leftB (tile-posx (car rest)))
                                    (topB (tile-posy (car rest)))
                                    (bottomB (+ (tile-posy (car rest)) (tile-height (car rest))))
                                    (rightB (+ (tile-posx (car rest)) (tile-width (car rest)))))
                (if (and (> bottomA (- topB 12)) (< bottomA bottomB) (>= rightA leftB) (<= leftA rightB))
                    #t
                    (loop (cdr rest))))))))



;; (define collision-down-tiles
;;   (lambda (player tileslist)
;;     (let loop ((rest tileslist))
;;       (unless (null? rest)
;;           (if (and
;;                (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 40) (tile-posx (car rest))))
;;                    (< (player-posx player) (+ (tile-posx (car rest)) 40))
;;                    (> (player-posy player) (- (tile-posy (car rest)) 39))
;;                    (< (player-posy player) (tile-posy (car rest))))
;;               #t
;;               (loop (cdr rest)))))))

(define collision-down-tiles-enemy
  (lambda (enemy tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (enemy-posx enemy) (tile-posx (car rest))) (> (+ (enemy-posx enemy) 40) (tile-posx (car rest))))
                   (< (enemy-posx enemy) (+ (tile-posx (car rest)) 40))
                   (> (enemy-posy enemy) (- (tile-posy (car rest)) 39))
                   (< (enemy-posy enemy) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))

;;Collsion tiles left

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
                    #t
                    (loop (cdr rest))))))))


;;Collsion tiles right

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
                    #t
                    (loop (cdr rest))))))))



(define collision-right-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 15) (tile-posx (car rest))))
               (> (+ (player-posx player) 40) (tile-posx (car rest)))
               (> (player-posy player) (tile-posy (car rest)))
               (< (- (player-posy player) 30) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))


(define collision-top-coins
  (lambda (player coinslist)
    (let loop ((rest coinslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (coin-posx (car rest))) (> (+ (player-posx player) 15) (coin-posx (car rest))))
                   (< (player-posx player) (+ (coin-posx (car rest)) 15))
                   (> (player-posy player) (coin-posy (car rest)))
                   (< (- (player-posy player) 40) (coin-posy (car rest))))
              (car rest)
              (loop (cdr rest)))))))

(define collision-down-coins
  (lambda (player coinslist)
    (let loop ((rest coinslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (coin-posx (car rest))) (> (+ (player-posx player) 15) (coin-posx (car rest))))
                   (< (player-posx player) (+ (coin-posx (car rest)) 15))
                   (> (player-posy player) (- (coin-posy (car rest)) 15))
                   (< (player-posy player) (coin-posy (car rest))))
              (car rest)
              (loop (cdr rest)))))))

(define collision-top-tiles
  (lambda (player tileslist)
    (let loop ((rest tileslist))
      (unless (null? rest)
          (if (and 
               (or (> (player-posx player) (tile-posx (car rest))) (> (+ (player-posx player) 15) (tile-posx (car rest))))
               (< (player-posx player)  (+ ( tile-posx (car rest)) 40))
               (> (player-posy player) (tile-posy (car rest)))
               (< (- (player-posy player) 40) (tile-posy (car rest))))
              #t
              (loop (cdr rest)))))))


(define condition-short
  (lambda (condition values)
    (if condition
        (car values)
        (car (cdr values)))))

(define (create-tiles-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (let create-plataforms ((element (vector-ref (vector-ref rest-map count-y) count-x)))
            (if (or (eq? element 1) (eq? element '+) (eq? element '*))
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
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 99)) 40.0 40.0) rest))
                        (create-plataform-for-enemy (+ number 1) posx))
                  (when (and (> number 0) (< number 8))
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (create-plataform-for-enemy (+ number 1) (+ posx 39)))
                  (when (= number 8)
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 110)) 40.0 40.0) rest))
                        (set! rest (cons (make-tile (exact->inexact posx) (exact->inexact (* (+ 0.7 count-y) 100)) 40.0 40.0) rest))))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-coins-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((+)
             (let create-plataform-with-coins ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 4)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins (+ number 1) (+ posx 40))))))
            ((++)
             (let create-plataform-with-coins-special ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 1)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(88 102)) )) 15.0 15.0 50 'green) rest))
                     (create-plataform-with-coins-special (+ number 1) (+ posx 40))))))
            ((+++)
             (let create-plataform-with-coins-doubles ((number 0) (posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (if (< number 8)
                   (begin
                     (set! rest (cons (make-coin (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) (condition-short (< count-y 2) '(98 102)))) 15.0 15.0 10 'yellow) rest))
                     (create-plataform-with-coins-doubles (+ number 1) (+ posx 40)))))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))

(define (create-enemies-map l)
  (let loop ((rest-map world-map) (rest l) (count-x 0) (count-y 0))
    (if (< count-y 5)
        (begin
          (case (vector-ref (vector-ref rest-map count-y) count-x)
            ((*)
             (let create-enemy-normal ((posx (+ (+ 0 (* 40 4)) (* count-x 100))))
               (set! rest (cons (make-enemy (exact->inexact (+ posx 10)) (exact->inexact (* (+ 0.7 count-y) 99)) 40.0 40.0 10 'blue) rest)))))
          (if (< count-x 101)
              (loop rest-map rest (+ count-x 1) count-y)
              (loop rest-map rest 0 (+ count-y 1))))
        rest)))


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
        (SDL_Log (string-append "OpenGL Version: " (unsigned-char*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)

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
               (texture-image* (SDL_LoadBMP "assets/128x128.bmp")))
          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)

          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 3
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_BGR GL_UNSIGNED_BYTE
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



            ;;Init
            ;(set! time (internal-time/ticks->seconds (process-time-clock)))
            
            ;; Game loop
            (let ((event* (alloc-SDL_Event 1)))
              (call/cc
               (lambda (quit)
                 (let main-loop ((world (make-world 'splashscreen '() 'none (make-player 0.0 0.0 0.0 0.0 'none 'none 0) '())) (time (SDL_GetTicks)))
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
                                        (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                (make-player
                                                                 (player-posx (world-player world))
                                                                 (player-posy (world-player world))
                                                                 (player-width (world-player world))
                                                                 (player-height (world-player world))
                                                                 'right
                                                                 (player-hstate (world-player world))
                                                                 (player-score (world-player world)))
                                                                (world-coins world))))
                                       
                                       ((= key SDLK_LEFT)
                                        (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                (make-player
                                                                 (player-posx (world-player world))
                                                                 (player-posy (world-player world))
                                                                 (player-width (world-player world))
                                                                 (player-height (world-player world))
                                                                 'left
                                                                 (player-hstate (world-player world))
                                                                 (player-score (world-player world)))
                                                                (world-coins world))))


                                       ((= key SDLK_UP)
                                        (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                (make-player
                                                                 (player-posx (world-player world))
                                                                 (player-posy (world-player world))
                                                                 (player-width (world-player world))
                                                                 (player-height (world-player world))
                                                                 (player-vstate (world-player world))
                                                                 'up
                                                                 (player-score (world-player world)))
                                                                (world-coins world))))
                                       
                                       ((= key SDLK_RETURN)
                                        (set! world (make-world 
                                                     'gamescreen
                                                     (create-tiles-map (world-tiles world))
                                                     (make-camera 0.0 'on 0.1)
                                                     (make-player 400.0 460.0 30.0 30.0 'none 'down 0)
                                                     (create-coins-map (world-coins world)))))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              

                              ((= event-type SDL_KEYUP)
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond 
                                  ((= key SDLK_RIGHT)
                                   (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                           (make-player
                                                            (player-posx (world-player world))
                                                            (player-posy (world-player world))
                                                            (player-width (world-player world))
                                                            (player-height (world-player world))
                                                            'none
                                                            (player-hstate (world-player world))
                                                            (player-score (world-player world)))
                                                           (world-coins world))))

                                  ((= key SDLK_LEFT)
                                   (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                           (make-player
                                                            (player-posx (world-player world))
                                                            (player-posy (world-player world))
                                                            (player-width (world-player world))
                                                            (player-height (world-player world))
                                                            'none
                                                            (player-hstate (world-player world))
                                                            (player-score (world-player world)))
                                                           (world-coins world))))

                                  ((= key SDLK_UP)
                                        (set! world (make-world (world-gamestates world) (world-tiles world) (world-camera world) 
                                                                (make-player
                                                                 (player-posx (world-player world))
                                                                 (player-posy (world-player world))
                                                                 (player-width (world-player world))
                                                                 (player-height (world-player world))
                                                                 (player-vstate (world-player world))
                                                                 (player-hstate (world-player world))
                                                                 (player-score (world-player world)))
                                                                (world-coins world))))
                                  
                                  (else
                                   (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))
                   
                   (set! vertex-data-vector '#f32())
                   
                   (case (world-gamestates world)
                     ((gamescreen)
                      
                      

                      ;;Logic Events

                      (if (and (eq? (player-vstate (world-player world)) 'left) (not (check-collision-right-tiles (world-player world) (world-tiles world))))
                          (let player-left ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
                            (begin
                              (player-posx-set! player (- (player-posx player) (* 0.3 delta-time)))
                              (if (eq? (camera-state camera) 'on)
                                  (camera-position-set! camera (- (camera-position camera) (* 0.3 delta-time)))))))
                      
                      (if (and (eq? (player-vstate (world-player world)) 'right) (not (check-collision-left-tiles (world-player world) (world-tiles world))))
                          (let player-right ((camera (world-camera world)) (tiles (world-tiles world)) (player (world-player world)))
                            (begin
                              (player-posx-set! player (+ (player-posx player) (* 0.3 delta-time)))
                              (if (eq? (camera-state camera) 'on)
                                  (camera-position-set! camera (+ (camera-position camera) (* 0.3 delta-time)))))))

                      (if (eq? (player-hstate (world-player world)) 'up) 
                          (if (check-collision-bottom (world-player world) (world-tiles world))
                              (player-hstate-set! (world-player world) 'jump)
                              (player-hstate-set! (world-player world) 'down)))


                      (if (eq? (player-hstate (world-player world)) 'jump) 
                          (if (not (collision-top-tiles (world-player world) (world-tiles world)))
                              (begin 
                                (if (= position-y-origin (player-posy (world-player world)))
                                    (set! position-y-origin (- (player-posy (world-player world)) -50)))
                                (let player-up ((player (world-player world)))
                                  (player-posy-set! player (- (player-posy player) (* 0.3 delta-time)))))
                              (player-hstate-set! (world-player world) 'down)))


                      (if (and (eq? (player-hstate (world-player world)) 'down) (not (check-collision-bottom (world-player world) (world-tiles world))))
                          (let player-down ((player (world-player world)))
                            (player-posy-set! player (+ (player-posy player) (* 0.3 delta-time)))))

                      
                      ;Control limits jump
                      (if (> position-y-origin (player-posy (world-player world)))
                          (player-hstate-set! (world-player world) 'down))
                      
                      
                      ;;Set camera
                      (if (eq? (camera-state (world-camera world)) 'auto)
                          (camera-position-set! (world-camera world) (+ (camera-position (world-camera world)) (* (camera-speed (world-camera world)) delta-time))))

                      

                      ;;keep the camera in bounds
                      (if (< (camera-position (world-camera world)) 0)
                          (camera-position-set! (world-camera world) 0))
                      (if (> (camera-position (world-camera world)) (- level-width (camera-position (world-camera world))))
                          (world-gamestates-set! world 'win))


                      ;Calculate collision with coins
                      (update-player-points-for-take-coin (world-player world) (world-coins world))
                      
                      
                      ;;Add all tiles of the world to buffer
                      (let add-all-tiles-of-world ((rest (world-tiles world)))
                        (when (not (null? rest))
                              (add-element-to-vector! 
                               (exact->inexact (- (tile-posx (car rest)) (camera-position (world-camera world))))
                               (exact->inexact (tile-posy (car rest)))
                               (tile-width (car rest))
                               (tile-height (car rest)))
                              (add-all-tiles-of-world (cdr rest))))

                      
                      ;;Add all coins of the world to buffer
                      (let add-all-coins-of-world ((rest (world-coins world)))
                        (when (not (null? rest))
                              (add-element-to-vector!
                               (exact->inexact (- (coin-posx (car rest)) (camera-position (world-camera world))))
                               (exact->inexact (coin-posy (car rest)))
                               (coin-width (car rest))
                               (coin-height (car rest)))
                              (add-all-coins-of-world (cdr rest))))
                      
                      
                      ;;Add player to buffer
                      (if (eq? (world-gamestates world) 'gamescreen)
                          (let add-player-to-buffer ((player (world-player world)))
                            (add-element-to-vector! (- (player-posx player) (camera-position (world-camera world)))
                                                    (player-posy player) 
                                                    (player-width player) 
                                                    (player-height player))))
                      
                      ;;Debug
                                        ;(println (string-append "Estados game: " (object->string (world-gamestates world))))
                      
                                        ;(println (string-append "Time: " (number->string time)))
                      
                      (if (eq? (world-gamestates world) 'gamescreen)
                          (println (object->string (camera-position (world-camera world)))))
                      
                      
                      ;; -- Draw --
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
                      
                      (SDL_GL_SwapWindow win)))
          (main-loop world (SDL_GetTicks)))
                 (free event*)))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

