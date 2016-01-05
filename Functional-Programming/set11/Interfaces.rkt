#lang racket

;; new version, based on WidgetWorks

(provide SWidget<%> Model<%>)

(define SWidget<%>
  (interface ()
    add-to-scene           ; Scene -> Scene
    after-tick             ; -> Void
    after-button-up        ; Nat Nat -> Void
    after-button-down      ; Nat Nat -> Void
    after-drag             ; Nat Nat -> Void
    after-key-event        ; KeyEvent -> Void
    ))

(define Model<%>
  (interface ()

    ;; -> Void
    after-tick        
    ;; Command -> Void
    ;; Executes the given command
    execute-command   
    ;; get-p -> Particle
    ;; returns our particle
    get-p
))
