;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;GOAL:To move a probe landed on pluto
(check-location "02" "probe.rkt")
(require rackunit)
(require "extras.rkt")
(provide
  probe-at
  probe-turn-left
  probe-turn-right
  probe-forward
  probe-north?
  probe-south?
  probe-east?
  probe-west?)
;;DATA DEFINATION:
(define NORTH "north")
;;Represents north direction
(define SOUTH "south")
;;Represents south direction
(define WEST  "west")
;;Represents west direction
(define EAST "east")
;;Represents east direction
(define-struct probe(xcoordinate ycoordinate probeface))
;;A probe is (make-probe integer integer string)
;;INTERPRETATION
;;xcoordinate is the X-coordinate of probe
;;ycoordinate is the Y-coordinate of probe
;;probeface is the direction in which the probe is pointing

;; TEMPLATE:
;; (define (probe-fn probe)
;;   (...
;;     (probe-xcoordinate probe)
;;     (probe-ycoordinate  probe)
;;     (probe-probeface    probe))     

;;probeface is one of
;; -- "north"
;; -- "south" 
;; -- "east"
;; -- "west"
;; INTERPRETATION: self-evident
;;TEMPLATE
;;probeface-fn:probeface->??
;;(define (probeface-fn face)
;; (cond
;;   [(string=? face "north") ...]
;;   [(string=? face "south") ...]
;;   [(string=? face "east")  ...]
;;   [(string=? face "west")  ...]
;;   ))  
;;probe-at:integer integer->probe
;;GIVEN: a x-coordinate and a y-coordinate
;;WHERE: these coordinates leave the probe entirely inside the trap
;;RETURNS: a probe with its center at those coordinates, facing north.
;;EXAMPLE:
;;(probe-at 100 200)=(make-probe 100 200 "north")
;;DESIGN STRATERGY:Combine Simpler functions
(define (probe-at xcoordinate ycoordinate)
  (make-probe xcoordinate ycoordinate NORTH))
;;TEST
(begin-for-test
  (check-equal? (probe-at 100 200) (make-probe 100 200 "north")
                "The coordinate of structure probe are set using this function"
                ))

;;probe-left-decide:string->string
;;GIVEN:The current face of probe
;;RETURN:The face of probe after turning left
;;EXAMPLE:
;;(probe-left-decide (north)=west
;;DESIGN STRATERGY:Use cases on probe face
(define (probe-left-decide dir)
  (cond
    [(string=? NORTH dir) WEST] 
    [(string=? WEST  dir) SOUTH]
    [(string=? SOUTH dir) EAST]
    [(string=? EAST dir) NORTH]))
;;TEST
(begin-for-test
  (check-equal? (probe-left-decide "north")
                "west"
                "The probe should face west when it turns to right")
  (check-equal? (probe-left-decide "west")
                "south"
                "The probe should face south when it turns to right")
  (check-equal? (probe-left-decide "south")
                "east"
                "The probe should face south when it turns to right")
  (check-equal? (probe-left-decide "east")
                "north"
                "The probe should face south when it turns to right"))


;;probe-turn-left:probe->probe
;;GIVEN:The probe before turning left(direction of probe)
;;RETURN:The probe after the probe has turned left(change in direction of probe)
;;EXAMPLE:
;;(probe-turn-left (make-probe 100 200 "north"))=(make-probe 100 200 "west")
;;DESIGN STRATERGY:Use template on probe probe.
(define (probe-turn-left probe)
  (make-probe (probe-xcoordinate probe)
              (probe-ycoordinate probe)
              (probe-left-decide (probe-probeface probe))))
;Test
(begin-for-test
  (check-equal? (probe-turn-left (make-probe 100  230 "north"))
                (make-probe 100 230 "west")
      "The probe should have face west when it turns left")
 (check-equal? (probe-turn-left (make-probe 100  230 "west"))
               (make-probe 100 230 "south")
      "The probe should have face south when it turns left")
(check-equal? (probe-turn-left (make-probe 100  230 "south"))
              (make-probe 100 230 "east")
      "The probe should have face east when it turns left")
 (check-equal? (probe-turn-left (make-probe 100  230 "east"))
               (make-probe 100 230 "north")
      "The probe should have face south when it turns left"))



;;probe-right-decide:string->string
;;GIVEN:The current face of probe
;;RETURN:The face of probe after turning right
;;EXAMPLE:
;;(probe-right-decide (north)=east
;;DESIGN STRATERGY:Use cases on probe face
(define (probe-right-decide dir)
  (cond
    [(string=? NORTH dir) EAST] 
    [(string=? WEST  dir) NORTH]
    [(string=? SOUTH dir) WEST]
    [(string=? EAST dir) SOUTH]))
;;Test
(begin-for-test
  (check-equal? (probe-right-decide "north")
                "east"
                "The probe should face east when it turns to right")
  (check-equal? (probe-right-decide "west")
                "north"
                "The probe should face north when it turns to right")
  (check-equal? (probe-right-decide "south")
                "west"
                "The probe should face south when it turns to right")
  (check-equal? (probe-right-decide "east")
                "south"
                "The probe should face east when it turns to right")
  )

;;probe-turn-right:probe->probe
;;GIVEN:The probe before turning right(direction of probe)
;;RETURN:The probe after the probe has turned right(change in direction of probe)
;;EXAMPLE:
;;(probe-turn-right (make-probe 100 200 "north"))=(make-probe 100 200 "east")
;;DESIGN STRATERGY:Use template on probe probe.
(define (probe-turn-right probe)
    (make-probe (probe-xcoordinate probe)
              (probe-ycoordinate probe)
              (probe-right-decide (probe-probeface probe))))

;Test
(begin-for-test
  (check-equal? (probe-turn-right (make-probe 100 230 "north"))
                (make-probe 100 230 "east")
                "The probe should face east when it turns to right")
  (check-equal? (probe-turn-right (make-probe 100 230 "east"))
                (make-probe 100 230 "south")
                "The probe should face south when it turns to right")
(check-equal? (probe-turn-right (make-probe 100 230 "south"))
              (make-probe 100 230 "west")
                "The probe should face west when it turns to right")
(check-equal? (probe-turn-right (make-probe 100 230 "west"))
              (make-probe 100 230 "north")
                "The probe should face north when it turns to right"))



;;probe-north:probe->boolean
;;GIVEN:The probe with its xcoordinate ycoordinate  and direction
;;RETURN:If the probe is facing north it returns true else it returns false.
;;EXAMPLE:
;;(probe-north (make-probe 200 150 "north")=#True
;;DESIGN-STRATERGY:Use template on probe probe            
(define (probe-north? probe)
  (string=? NORTH (probe-probeface probe)))
;Test
(begin-for-test
  (check-equal? (probe-north? (make-probe 0 0 "north")) #true
      "The boolean value should be true as it is facing north"))

;;probe-south:probe->boolean
;;GIVEN:The probe with its xcoordinate ycoordinate  and direction
;;RETURN:If the probe is facing south it returns true else it returns false.
;;EXAMPLE:
;;(probe-north (make-probe 200 150 "south")=#True
;;DESIGN-STRATERGY:Use template on probe probe             
(define (probe-south? probe)
  (string=? (probe-probeface probe) SOUTH))
;Test
(begin-for-test
  (check-equal? (probe-south? (make-probe 20 30 "north")) #false
      "The boolean value should be false as it is facing south"))

;;probe-east:probe->boolean
;;GIVEN:The probe with its xcoordinate ycoordinate  and direction
;;RETURN:If the probe is facing east it returns true else it returns false.
;;EXAMPLE:
;;(probe-east (make-probe 200 150 "east")=#True
;;DESIGN-STRATERGY:Use template on probe probe            
(define (probe-east? probe)
  (string=? (probe-probeface probe) EAST))
;Test
(begin-for-test
  (check-equal? (probe-east? (make-probe 10 20 "east")) #true
      "The boolean value should be true as it is facing east"))

;;probe-east:probe->boolean
;;GIVEN:The probe with its xcoordinate ycoordinate  and direction
;;RETURN:If the probe is facing west it returns true else it returns false.
;;EXAMPLE:
;;(probe-east (make-probe 200 150 "west")=#True
;;DESIGN-STRATERGY:Use template on probe probe             
(define (probe-west? probe)
  (string=? (probe-probeface probe) WEST))

;Test
(begin-for-test
  (check-equal? (probe-west? (make-probe 10 20 "west")) #true
      "The boolean value should be true as it is facing west")
   (check-equal? (probe-west? (make-probe 10 20 "east")) #false
      "The boolean value should be false as it is facing west")
  )

;;calculate-distance-north-west:integer PosNum ->Integer 
;;GIVEN:The xcoordinate or ycoordinate of probe and the distance it has to travel in that direction when it is facing north or west
;;RETURN:The xcoordinate or ycoordinate after the probe has travelled  the given distance
;;EXAMPLE:
;;(calculate-distance-north-west -100 10)=-110
;;DESIGN-STRATERGY:Combining Simpler Functions
(define (calculate-distance-north-west coordinate distance)
(- coordinate distance)
  )

;;move-probe-north:probe distance->probe
;;GIVEN:The probe and distance the probe should move north
;;RETURN:The probe after the probe has moved north
;;EXAMPLE:
;;(move-probe-north (make-probe 23 -123 "north" ) 10)=(make-probe 23 -133 "north")
;;DESIGN STRATERGY:Use template on probe probe
(define (move-probe-north probe distance)
 (if
  (<= (- 153) (- (probe-ycoordinate probe) distance))
     (make-probe (probe-xcoordinate probe)
                 (calculate-distance-north-west (probe-ycoordinate probe) distance) (probe-probeface probe))
     probe))
;Test
(begin-for-test
   (check-equal? (move-probe-north (make-probe 23 -123 "north" ) 10) (make-probe 23 -133 "north")
                 "The probe should move north by given distance")
   (check-equal? (move-probe-north (make-probe 23 -123 "north" ) 31) (make-probe 23 -123 "north")
                 "The probe should not  move north by given distance"))
;;calculate-distance-south-east:integer PosNum ->Integer 
;;GIVEN:The xcoordinate or ycoordinate of probe and the distance it has to travel in that direction when it is facing south or east
;;RETURN:The xcoordinate or ycoordinate after the probe has travelled  the given distance
;;EXAMPLE:
;;(calculate-distance-south-east -100 10)=90
(define (calculate-distance-south-east coordinate distance)
    (+ coordinate distance))

;;move-probe-south:probe distance->probe
;;GIVEN:The probe and distance the probe should move south
;;RETURN:The probe after the probe has moved south
;;EXAMPLE:
;;(move-probe-north (make-probe 23 -123 "south" ) 10)=(make-probe 23 -113 "south")
;;DESIGN STRATERGY:Use template on probe probe
(define (move-probe-south probe distance)
  (if
   (<= (+(probe-ycoordinate probe ) distance) 153)
   (make-probe (probe-xcoordinate probe) (calculate-distance-south-east (probe-ycoordinate probe) distance) (probe-probeface probe))
   probe
   ))  
;Test
 (begin-for-test
   (check-equal? (move-probe-south (make-probe 23 -123 "south" ) 10) (make-probe 23 -113 "south")
                 "The probe should move south by given distance")
  (check-equal? (move-probe-south (make-probe 23 123 "south" ) 31) (make-probe 23 123 "south")
                 "The probe should not move south by given distance"))
;;move-probe-west:probe distance->probe
;;GIVEN:The probe and distance the probe should move to west
;;RETURN:The probe after the probe has moved west
;;EXAMPLE:
;;(move-probe-west (make-probe 23 -123 "west" ) 10)=(make-probe 13 -133 "west")
;;DESIGN STRATERGY:Use template on probe probe
(define (move-probe-west probe distance)
  (if
   (<= (- 153) (- (probe-xcoordinate probe) distance))
      (make-probe (calculate-distance-north-west (probe-xcoordinate probe) distance) (probe-ycoordinate probe) (probe-probeface probe))
      probe))
   ;Test
   (begin-for-test
   (check-equal? (move-probe-west (make-probe 23 -123 "west" ) 10) (make-probe 13 -123 "west")
                 "The probe should move west by given distance")
   (check-equal? (move-probe-west (make-probe -144 -123 "west" ) 10) (make-probe -144 -123 "west")
                 "The probe should not  move west by given distance"))


;;move-probe-east:probe distance->probe
;;GIVEN:The probe and distance the probe should move to east
;;RETURN:The probe after the probe has moved east
;;EXAMPLE:
;;(move-probe-east (make-probe 23 -123 "east" ) 10)=(make-probe 33 -133 "east")
;;DESIGN STRATERGY:Use template on probe probe
(define (move-probe-east probe distance)
  (if
   (>= 153 (+ (probe-xcoordinate probe) distance))
      (make-probe (calculate-distance-south-east (probe-xcoordinate probe) distance) (probe-ycoordinate probe) (probe-probeface probe))
      probe))
;Test
(begin-for-test
   (check-equal? (move-probe-east (make-probe 13 -123 "east" ) 10) (make-probe 23 -123 "east")
                 "The probe should move east by given distance")
    (check-equal? (move-probe-east (make-probe 124 -123 "east" ) 30) (make-probe 124 -123 "east")
                 "The probe should not move east by given distance"))
;;probe-forward:probe distance->probe
;;GIVEN:A probe and the distance the probe should travel
;;RETURN:A probe after travelling that distance
;;EXAMPLE:
;;(probe-forward (make-probe 100 100 "north") 20 )=(make-probe 100 80 "north")
;;DESIGN STRATERGY:Use cases on probe
(define (probe-forward probe distance)
  (cond
    [(probe-north? probe) (move-probe-north probe distance)]
    [(probe-south? probe) (move-probe-south probe distance)]
    [(probe-west?  probe) (move-probe-west  probe distance)]
    [(probe-east? probe) (move-probe-east probe distance)]
    ))
;TEST:
(begin-for-test
   (check-equal? (probe-forward (make-probe 13 -123 "east" ) 10) (make-probe 23 -123 "east")
                 "The probe should move east by given distance")
   (check-equal? (probe-forward (make-probe 13 -123 "west" ) 10) (make-probe 3 -123 "west")
                 "The probe should move west by given distance")
   (check-equal? (probe-forward (make-probe 13 -133 "north" ) 20) (make-probe 13 -153 "north")
                 "The probe should move north by given distance")
   (check-equal? (probe-forward (make-probe 13 -133 "south" ) 20) (make-probe 13 -113 "south")
                 "The probe should move south by given distance"))