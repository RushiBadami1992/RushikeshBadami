;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;PROBLEM DESCRIPTION
;you will design and implement a system for a graphical interface for trees. Your system
;will allow you to create and manipulate trees on a canvas. Create a file called
;"trees.rkt" with the following properties:The canvas starts empty. Its size is 500
;pixels wide by 400 pixels high.Nodes of the tree are rendered as green outline circles
;of a fixed radius. The default value for the radius is 10, but your system should allow
;you to change the radius for the next run by changing a single line of your code.When
;the tree is displayed, there should be a straight blue line from the center of a node to
;the center of each of its sons.You can select a node by clicking on it, as in previous
;problems. Selected nodes are displayed as green solid circles. Clicking on a node selects
;only the node, not any of its subtrees. If the mouse is clicked.in the overlap of two or
;more nodes, all the nodes are selected, even if one node is a son or descendant
;of the other.

;*******************************************************************************
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)
(check-location "06" "trees.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)


;;Goal:To implement tree.rkt
;******************************************************************************
(define-struct world(trees))
;; world is a World (make-world ListOfTrees)

;INTERPRETATION
;; trees is a ListOfTrees where each Tree in the list can either have multiple
;; descendants or no descendants  with an only node.


; A Tree is a Node
; A ListOfTrees is a ListOfNodes .

;; ListOfTrees(LOT) is either
;; -- empty
;; -- (cons Tree LOT)


;; Template:
;; lot-fn:ListofTrees->??
;; (define (lot-fn trees)
;;  (cond
;;  [(empty? trees )...]
;;  [(else(....
;;          (node-fn (first trees)
;;          (lot-fn(rest trees)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct node(x y mx my selected? nodes))
;; A Node is (make-node  Integer Integer Integer Integer Boolean ListOfNodes)

;; INTERPRETATION
;; x and y:Are the coordinates of the  center of node
;; mx and my are the coordinates of mouse.
;; selected? is a boolean which specifies if a node is selected or not following
;; the mouse Event
;; nodes is a ListOfNodes which can either be the part of same tree or different trees

;; Template for Node:
;; node-fn: Node->??
;; (define (node-fn n)
;; (....(node-x n)
;;      (node-y n)
;;      (node-mx n)
;;      (node-my n)
;;      (node-selected? n)
;;      (nodes n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ListOfNodes(LON) is either
;; --empty
;; --(cons Node LON)


;;Template for ListOfNodes
;; lon-fn: ListofNodes->??
;; (define (lon-fn nodes)
;;  (cond
;;  [(empty? nodes )...]
;;  [(else(....
;;          (node-fn (first nodes)
;;           (lon-fn(rest nodes)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dimensions of Canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define HALF-WIDTH 250)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constants defined for Program Implementation
(define CIRCLE (circle 10 "outline" "green"))
(define SELECTED-CIRCLE(circle 10 "solid" "green"))
(define ROOT-NODE-X 250)
(define ROOT-NODE-Y 10)
(define ROOT-NODE-X1 260)
(define BLUE "blue")
(define THREE-RADII 30)
(define RADIUS 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for Mouse Events
(define BUTTONUP "button-up")
(define BUTTONDOWN "button-down")
(define DRAG "drag")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for Key Events
(define CREATE-ROOT "t")
(define ADD-NODE "n")
(define DELETE-NODE "d")
(define DELETE-LEFT-NODE "l")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constants for TESTING
(define MX1 290 )
(define MY1 40)
(define MX2 300)
(define MY2 50)
(define ROOT-NODE (make-node ROOT-NODE-X ROOT-NODE-Y 0 0 false empty))
(define ROOT-NODE1 (make-node  260  ROOT-NODE-Y 0 0 false empty))
(define ROOT-NODE2 (make-node  260  ROOT-NODE-Y 0 0 true empty))
(define ROOT-NODE3 (make-node  260  40 0 0 false empty))
(define ROOT-NODE6 (make-node  290  40 0 0 false empty))
(define ROOT-NODE7 (make-node  290  40 0 0 true empty))
(define ROOT-NODE10(make-node  290 70 0 0 #f empty))
(define ROOT-NODE8 (make-node  260  ROOT-NODE-Y 0 0 false (list ROOT-NODE7)))
(define ROOT-NODE4 (make-node  260  ROOT-NODE-Y 0 0 true (list ROOT-NODE3)))
(define ROOT-NODE5 (make-node  260  ROOT-NODE-Y 0 0 true (list ROOT-NODE6 ROOT-NODE3)))
(define ROOT-NODE9 (make-node  260  ROOT-NODE-Y 0 0 false (list ROOT-NODE7 ROOT-NODE10)))
(define INITIAL-WORLD6
  (make-world (cons(make-node 260 10 0 0 #f (cons(make-node 290 40 0 0 #t 
                                                   (cons(make-node 290 70 0 0 #f empty)
                                                     empty))empty))empty)))
(define ROOT-NODE13 (make-node  290  40 0 0 true empty))
(define ROOT-NODE12 (make-node  260  ROOT-NODE-Y 0 0 false (list ROOT-NODE13)))
(define ROOT-NODE14 (make-node 260 10 290 40 #f (cons(make-node 290 40 290 40 #t empty)
                                                     empty)))
(define MOUSE-NODE(make-node 260  ROOT-NODE-Y 0 0 true (list ROOT-NODE10)))
(define MOUSE-NODE1 (make-node  290  40 MX1 MY1 true empty))
(define MOUSE-NODE2 (make-node  290  40 MX1 MY1 false empty))
(define MOUSE-NODE3(make-node 260  ROOT-NODE-Y 0 0 false (list MOUSE-NODE1)))
(define MOUSE-NODE4(make-node 260  ROOT-NODE-Y 290 40 false (list MOUSE-NODE2)))
(define ADD-ROOT(make-world(cons ROOT-NODE empty)))
(define MOUSE-NODE5 (make-node  290  40 MX1 MY1 true empty))
(define MOUSE-NODE6 (make-node  300  50 MX2 MY2 true empty))
(define MOUSE-NODE7 (make-node 290 40 MX1 MY1 false (list MOUSE-NODE6)))
(define MOUSE-NODE8 (make-node 290 40 290 40 #f
                               (cons(make-node 300 50 300 50 #t empty)empty)))
(define NODEC (make-node 260 80 0 0  false empty))
(define NODES (make-node 260 50 0 0 false (list NODEC)))
(define ROOT-NODE11 (make-node  290  40 MX1 MY1 true empty))
(define INITIAL-WORLD (make-world empty))
(define INITIAL-WORLD1 (make-world (cons ROOT-NODE1 empty)))
(define INITIAL-WORLD2 (make-world (cons ROOT-NODE2 empty)))
(define INITIAL-WORLD3 (make-world (cons ROOT-NODE4 empty)))
(define INITIAL-WORLD4 (make-world (cons ROOT-NODE5 empty)))
(define INITIAL-WORLD5 (make-world (cons ROOT-NODE8 empty)))
(define INITIAL-WORLDM (make-world (cons ROOT-NODE7 empty)))
(define INITIAL-WORLD7 (make-world (cons ROOT-NODE11 empty)))
(define INITIAL-WORLD8 (make-world (cons ADD-ROOT empty)))
(define INITIAL-WORLDS (make-world (cons NODES empty)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;                           End of Data Definitions
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; EXAMPLE: Check test for example
;; DESIGN-STRATERGY: Template of world on w

(define (initial-world a)
  (make-world empty))

;;Test
(begin-for-test
  (check-equal? (initial-world 0.2)
                INITIAL-WORLD
                "Create Initial World"))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; STRATEGY: Combine Simpler Function
(define (run a)
  (big-bang (initial-world a)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)
            (to-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be following the given key event
;; EXAMPLE:  (world-after-key-event INITIAL-WORLD CREATE-ROOT) ADD-ROOT)
;; STRATEGY: Use cases on Keyevent kev.

(define (world-after-key-event w kev)
  (cond
    [(string=? kev CREATE-ROOT)(add-root-node w)] 
    [(string=? kev DELETE-NODE)(delete-nodes w kev)]
    [(string=? kev ADD-NODE)(add-children-to-node w)]
    [(string=? kev DELETE-LEFT-NODE)(delete-nodes w kev)]
    [else w]))

;;TEST
(begin-for-test
  (check-equal? (world-after-key-event INITIAL-WORLD CREATE-ROOT) ADD-ROOT
                "Add root node on canvas")
  (check-equal? (world-after-key-event INITIAL-WORLD "X") INITIAL-WORLD
                "Should do nothing")
 (check-equal? (world-after-key-event ADD-ROOT DELETE-LEFT-NODE) ADD-ROOT
                "Delete nodes on left side")
  (check-equal? (world-after-key-event INITIAL-WORLD1 DELETE-LEFT-NODE) INITIAL-WORLD1
                "Delete nodes on left side")
  (check-equal? (world-after-key-event INITIAL-WORLD1 DELETE-NODE) INITIAL-WORLD1
                "Delete selected nodes")
  (check-equal? (world-after-key-event INITIAL-WORLD2 DELETE-NODE) INITIAL-WORLD
                "Delete selected nodes")
  (check-equal? (world-after-key-event INITIAL-WORLD2 ADD-NODE) INITIAL-WORLD3
                "Delete selected nodes")
  (check-equal? (world-after-key-event INITIAL-WORLD3 ADD-NODE) INITIAL-WORLD4
                "Delete selected nodes")
  (check-equal? (world-after-key-event INITIAL-WORLD5 ADD-NODE) INITIAL-WORLD6
                "Delete selected nodes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add-root-node: World-> World
;;GIVEN A world
;;RETURN: A world after adding  the root node on it.
;;EXAMPLE: Check Test for world-after-key-event.
;;STRATEGY: Use Template for world on w.

(define (add-root-node w)
  (make-world (cons (make-node ROOT-NODE-X ROOT-NODE-Y 0 0 false
                               empty) (world-trees w))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete-nodes: World KeyEvent-> World
;; GIVEN: A world
;; RETURN: A world after deleting the nodes whose centre point of the nodes are on the
;; left half of the canvas width or  if the node is selected using keyEvent.
;; EXAMPLE:Check test of world-after-key-event
;; STRATEGY: Use template for world on w.
(define (delete-nodes w key)
  (make-world (delete-nodes-on-left-and-selected (world-trees w) key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-nodes-on-left-and-selected: ListOfNodes Keyevent-> ListOfNodes
;; GIVEN:  A List of nodes
;; RETURN: list of nodes which are left after deleting the nodes on the left half of the
;; canvas
;; EXAMPLE:Check test of world-after-key-event
;; STRATEGY: use HOF foldr on lon
(define (delete-nodes-on-left-and-selected lon key)
  (foldr
   ;; Node ListOfNodes->  ListOfNodes
   ;; GIVEN: A node and list of nodes
   ;; RETURN: A list of nodes which are neither selected nor on the left side of the half
   ;; width of the canvas
   (lambda (n resultList)(if(not(check-node-for-deletion? n key))
                   (remaining-nodes-after-deletion n resultList key)
                   resultList))  
   empty 
   lon))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remaining-nodes-after-deletion: Node ListOfNodes Keyevent-> ListOfNodes
;; GIVEN:A node,List of nodes and a keyevent.
;; RETURN:A list of nodes which are neither selected nor on the left side of the half
;; width of the canvas
;; EXAMPLE:Check test of world-after-key-event
;; STRATEGY: Use template for node on n.
 
(define (remaining-nodes-after-deletion n lon key)
  (cons(make-node (node-x n)(node-y n)
                  (node-mx n)(node-my n)(node-selected? n)
                  (delete-nodes-on-left-and-selected (node-nodes n) key))lon)) 

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-node-for-deletion?: Node Keyevent-> Boolean
;; GIVEN:A node  and a keyevent.
;; RETURN: true iff node is selected or on the left half of the canvas.
;; EXAMPLE:Check test of world-after-key-event
;; STRATEGY: Use cases on keyEvent key.
(define (check-node-for-deletion? n key)
  (cond
    [(string=? DELETE-NODE key)(node-selected? n)]
     [(string=? DELETE-LEFT-NODE key)(check-node-left? n)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;check-node-left?: Node-> Boolean
;; GIVEN: A node
;; RETURN: true if the value of centre of the node is less than the half width of the
;; canvas.
;; EXAMPLE:Check test of world-after-key-event.
;; STRATEGY: use template of node on n.

(define (check-node-left? n)
  (< (node-x n) HALF-WIDTH))   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-children-to-node:  World-> World
;; GIVEN: A world
;; RETURN: a world after adding nodes .
;; EXAMPLE:Check tests of world-after-key-event
;; STRATEGY: Use hof map on w.
(define (add-children-to-node w)
  (make-world
   (map
    ;; Node-> Node
    ;;GIVEN: a node
    ;;RETURN: a node after adding a child node on it. 
    (lambda (node)(add-new-child-node node))
    (world-trees w)))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-new-child-node: Node-> Node
;; GIVEN: A node
;; RETURN: a node after adding new child node on it
;; EXAMPLE:Check test of world-after-key-event.
;: STRATEGY: Use template for node on node.
(define (add-new-child-node node)
  (if (node-selected? node)
      (add-child-node node)
      (add-node-to-child-node node)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-node-to-child-node: Node -> Node
;; GIVEN: A node
;; RETURN: a node after adding new child node on it
;; EXAMPLE: Check test of world-after-key-event.
;: STRATEGY: Use template for node on n.

(define (add-node-to-child-node n)
  (make-node (node-x n)(node-y n)(node-mx n)(node-my n)
             false(add-node-to-child (node-nodes n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; add-node-to-child: ListOfNodes-> ListOfNodes
;; GIVEN:A list of nodes 
;; RETURN: A list of nodes after adding the child nodes to them
;; EXAMPLE:Check test of world-after-key-event 
;: STRATEGY:Use HOF MAP on nodes.
(define (add-node-to-child nodes)
  (map
   ;Node-> node
   ;GIVEN: A node
   ;RETURN: A node after adding a child node to it.
   (lambda (n)(add-new-child-node n))
   nodes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-child-node: Node -> Node
;; GIVEN:A node
;; RETURN: a node after adding a child node to it.
;; EXAMPLE:Check test of world-after-key-event.
;: STRATEGY:Use of cases on node whether it is empty or not.
(define (add-child-node node)
  (cond
    [(empty? (node-nodes node))(add-below-parent node)]
    [else(add-node-next-to-child node)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-node-next-to-child: Node-> Node
;; GIVEN:A node
;; RETURN: a node after adding a  new child node next to its child node on right side.
;; EXAMPLE:Check test of world-after-key-event.
;: STRATEGY:use template for node on node.
(define (add-node-next-to-child node)
  (make-node (node-x node) (node-y node) 0 0 (node-selected? node)
             (cons(make-node (+ THREE-RADII (find-max-x (node-nodes node)))
                             (+ THREE-RADII (node-y node)) 0 0 false empty)
                  (add-node-to-child(node-nodes node)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;find-max-x ListOfNodes-> PosInt
;; GIVEN: List of nodes
;;RETURN: Maximum value of x co-ordinate among all  the nodes.
;;EXAMPLE: (list 20,30,40) =40
;;STRATEGY: Use HOF map on  lon.

(define (find-max-x lon)
  (first (sort(map
               ;; Node->PosInt
               ;; GIVEN:  A node 
               ;;RETURN: the value of the x co-ordinate of the node
               (lambda (node)(node-x node))
               lon)>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-below-parent: Node-> Node
;; GIVEN: A node
;; RETURN:Node after having a new child node added below it.
;; EXAMPLE:Check test of world-after-key-event.
;; STRATEGY: Use template for node on node
(define (add-below-parent node)
  (make-node (node-x node) (node-y node) 0 0 (node-selected? node)
             (cons (make-node (node-x node)(+ THREE-RADII (node-y node)) 0 0 false empty)
                   (node-nodes node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse event at
;; that location.
;; EXAMPLE: Check Testcases .
;; STRATEGY: Use template for world on w. 

(define (world-after-mouse-event w mx my mev)
  (make-world
   (nodes-after-mouse-event (world-trees w) mx my mev)))

;;TEST
(begin-for-test
  (check-equal? (world-after-mouse-event  INITIAL-WORLDM MX1 MY1 BUTTONDOWN)
                INITIAL-WORLD7
                "Should return same world as mouse event wrong input"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;nodes-after-mouse-event: ListOfNodes Integer Integer MouseEvent-> ListOfNodes
;; GIVEN: ListOfnodes x and y cordinates of mouse and MouseEvent
;; RETURN:listOfNodes after following mouseEvent mev
;; EXAMPLE: check test of world-after-mouse-event.
;; STRATERGY: Use of HOF map on nodes
(define (nodes-after-mouse-event nodes mx my mev)
  (map
   ;; Node -> Node
   ;; Given: A node
   ;; Return : A node after following the mouseEvent mev on it.
   (lambda (node) (node-after-mouse-event node mx my mev))
   nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-mouse-event: Node Integer Integer MouseEvent-> Node
;; GIVEN : node, x and y cordinates of mouse and MouseEvent
;; RETURN:ListOfNodes after following mouseEvent mev
;; EXAMPLES: (node-after-mouse-event ROOT-NODE7 MX1 MY1 BUTTONDOWN)
;;            ROOT-NODE11
;; STRATERGY:Cases on MouseEvent Mev


(define (node-after-mouse-event node mx my mev)
  (cond
    [(mouse=? mev BUTTONDOWN) (node-after-button-down node mx my)]
    [(mouse=? mev DRAG) (node-after-drag node mx my)]
    [(mouse=? mev BUTTONUP) (node-after-button-up node mx my)]
    [else node]))

;;TEST
(begin-for-test
  (check-equal? (node-after-mouse-event ROOT-NODE7 MX1 MY1 BUTTONDOWN)ROOT-NODE11
                "Should add mouse coordinates to node")
  (check-equal? (node-after-mouse-event ROOT-NODE12 MX1 MY1 BUTTONDOWN)ROOT-NODE14
                "Should add mouse coordinates to node")
  (check-equal? (node-after-mouse-event MOUSE-NODE1 MX1 MY1 BUTTONUP)
                        (make-node 290 40 290 40 #f '()))
   (check-equal?(node-after-mouse-event MOUSE-NODE1 MX1 MY1 "enter")
                    (make-node 290 40 290 40 #t '()))
  (check-equal?(node-after-mouse-event MOUSE-NODE5 MX2 MY2 DRAG)MOUSE-NODE6
               "Should drag root")
  (check-equal?(node-after-mouse-event MOUSE-NODE7 MX2 MY2 DRAG)MOUSE-NODE8
               "Should drag root"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-button-down: Node NonNegInt NonNegInt -> Node
;; GIVEN: node, x and y cordinates of mouse 
;; RETURN: ListOfNodes after following mouseEvent mev
;; EXAMPLE:(node-after-button-down ROOT-NODE12 MX1 MY1 BUTTONDOWN)
;;          ROOT-NODE14
;; STRATEGY: Use template for node on n.

(define (node-after-button-down n mx my)
  (if(in-node?  n mx my)
     (make-node (node-x n)(node-y n) mx my true
                (children-selected-on-button-down (node-nodes n) mx my))
     (make-node(node-x n)(node-y n) mx my  (node-selected? n)
               (children-selected-on-button-down (node-nodes n) mx my))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; children-selected-on-button-down: ListOfNodes NonNegInt NonNegInt -> ListOfNodes
;; GIVEN: ListOfnodes,x and y cordinates of mouse .
;; RETURN: list of child nodes after checking they are selected or not.
;; EXAMPLE: Check test of node-after-mouse-event
;; STRATEGY: Use HOF map on lon.

(define (children-selected-on-button-down lon  mx my)
  (map
   ;; node-> node
   ;; GIVEN: A node
   ;; RETURN: A node following the button down mouse event
   (lambda (n)(node-after-button-down n mx my))
   lon))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in-node?: Node NonNegInt NonNegInt -> Boolean
;; GIVEN: A node, x and y co-ordinates of mouse.
;; Return : True if the value of the distance is less than equal to radii of node
;; EXAMPLE: Check test of node-after-mouse-event
;; STRATEGY: Use template for node on n
(define (in-node? n mx my )
  (<=
   (sqrt
    (+(sqr(-(node-x n) mx))
      (sqr(-(node-y n) my))))
   RADIUS))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-button-up: Node NonNegInt NonNegInt -> Node
;; GIVEN: node x and y cordinates of mouse and MouseEvent
;; RETURN:node when the mouse  button is up
;; EXAMPLE:Check test of node-after-mouse-event
;; STRATEGY: Use template for node on n.

(define (node-after-button-up n mx my)
  (make-node(node-x n)(node-y n) mx my false
            (selected-children-after-button-up (node-nodes n) mx my))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selected-children-after-button-up: ListOfNodes NonNegInt NonNegInt -> ListOfNodes
;; GIVEN: ListOfnodes  x and y cordinates of mouse .
;; RETURN: list of children nodes after  mouse button-up.
;; EXAMPLE:Check test for example.
;; STRATEGY: Use HOF map on lon.

(define (selected-children-after-button-up lon  mx my) 
  (map
   ;node->node
   ;GIVEN: A node 
   ;RETURN: node when the mouse button is up .
   (lambda (n)(node-after-button-up n mx my))
   lon))


;;TEST
(begin-for-test
  (check-equal?
   (selected-children-after-button-up (list ROOT-NODE4) 260 60)
(list (make-node 260 10 260 60 #f (list (make-node 260 40 260 60 #f  '()))))))
 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-after-drag: Node NonNegInt NonNegInt -> Node
;; GIVEN: node, x and y cordinates of mouse .
;; RETURN: node after dragging  to some other position in the canvas.
;; EXAMPLE: Check test of node-after-mouse-event.
;; Strategy: Use template for node on n.
(define (node-after-drag node mx my)
  (if (node-selected? node)
      (smooth-drag node mx my)
      (make-node (node-x node)(node-y node)
                 (node-mx node) (node-my node)
                 (node-selected? node) (smooth-drag-in-childnodes
                                        (node-nodes node) mx my))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-drag-in-childnodes: ListOfNodes NonNegInt NonNegInt -> ListOfNodes
;; GIVEN: node, x and y cordinates of mouse.
;; RETURN: node when it is drag to some other position in the canvas.
;; EXAMPLE: Check test of node-after-mouse-event .
;; STRATEGY: Use HOF map on lon.
(define (smooth-drag-in-childnodes lon mx my)
  (map
   ;; node-> node
   ;; GIVEN: A node
   ;; Return: Node after dragging it to a new position.
   (lambda (n)(node-after-drag  n mx my))
   lon))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-drag: Node NonNegInt NonNegInt -> ListOfNodes
;; GIVEN: node, x and y cordinates of mouse 
;; RETURN:node after dragging it to some other position in the canvas.
;; EXAMPLE:Check test of node-after-mouse-event for example.
;; STRATEGY: Use template of node on n.

(define (smooth-drag n mx my)
  (make-node(new-coordinate-after-dragging (node-x n)(node-mx n) mx)
            (new-coordinate-after-dragging (node-y n)(node-my n) my)
            mx my (node-selected? n)
            (change-coordinate-of-child-node (node-nodes n) mx my)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change-coordinate-of-child-node: ListOfNodes NonNegInt NonNegInt -> ListOfNodes
;; GIVEN: ListofNodes ,x and y cordinates of mouse 
;; RETURN:nodeafter dragging it to some other position in the canvas.
;; EXAMPLE:Check test .
;; STRATEGY: Use template of node on n.

(define (change-coordinate-of-child-node lon mx my)
  (map
   ; Node NonNegInt NonNegInt  -> Node
   (lambda(n)(smooth-drag n mx my))
   lon))



;;TEST
(begin-for-test
  (check-equal? (change-coordinate-of-child-node (list MOUSE-NODE5)MX2 MY2)
                (list MOUSE-NODE6)
                "Change Drag Coordinates of child"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                

;; new-coordinate-after-dragging: Integer NonNegInt NonNegInt -> Integer
;; GIVEN:  co-ordinate of the current position ,co-ordinate of the mouse and 
;; co-ordinate after drag
;; RETURN: Value of updated  co-ordinate of the node .
;; EXAMPLE:Check test of mouse-after-node-event for example.
; STRATEGY: Combine Simpler function.

(define (new-coordinate-after-dragging cord mouse-buttondown-cord drag-cord)
  (+ cord (- drag-cord mouse-buttondown-cord)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene: A World -> Scene
;; GIVEN : A world
;; RETURN: Final worldstate as a scene.
;; EXAMPLE: Check tests.
;; STRATEGY Use Template for world on w.

(define (world-to-scene w)
  (extract-list-of-trees (world-trees w) EMPTY-CANVAS))

;;TEST
(begin-for-test
  (check-equal? (world-to-scene INITIAL-WORLD1)
                (place-image CIRCLE ROOT-NODE-X1 ROOT-NODE-Y EMPTY-CANVAS ) 
                "Place root node on scene")
  (check-equal? (world-to-scene INITIAL-WORLD2)
                (place-image SELECTED-CIRCLE ROOT-NODE-X1 ROOT-NODE-Y EMPTY-CANVAS ) 
                "Place root node on scene")
  (check-equal? (world-to-scene INITIAL-WORLDS)(place-image CIRCLE 260 50
  (scene+line (place-image CIRCLE 260 80 EMPTY-CANVAS)260 50 260 80 "blue"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract-list-of-trees: ListOfTrees Scene -> Scene
;; GIVEN: ListofTrees and a scene
;; Return: The final state of the world as scene.
;; EXAMPLE:Check test of world-to-scene for example
;; Strategy: Using HOF foldr on lot.
(define (extract-list-of-trees lot s)
  (foldr
   ; Node scene -> scene
   ;GIVEN: A node and the scene.
   ;RETURN: A scene with node placed in it.
   (lambda(node s) (place-node  node s))
   EMPTY-CANVAS
   lot))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-node: Node Scene -> Scene
;; GIVEN: a node and the scene
;; Return: An image after placing the node following the conditions whether
;; node is selected or not.
;; EXAMPLE:Check test of world-to-scene for example
;; Strategy: Use Template for node on node.
(define (place-node node scene)
  (if (node-selected? node)
      (place-node-on-canvas node scene SELECTED-CIRCLE)
      (place-node-on-canvas node scene CIRCLE)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; place-node-on-canvas:Node Scene Image -> Image
;; GIVEN: A node,scene and an Image of cirlce.
;; RETURN: An image of the node placed on the scene.
;; EXAMPLE:Check test of world-to-scene for example
;; Strategy: use template for node on n .

(define (place-node-on-canvas n scene circle)
  (place-image
   circle
   (node-x n)
   (node-y n)
   (place-line-to-scene (node-nodes n)(node-x n)(node-y n) scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-line-to-scene: ListOfNodes Integer Integer Scene -> Scene
;; GIVEN: List of nodes which are children of some node x and Y coordinate of the node
;; and scene
;; RETURN: a scene after placing the  blue line which will be from the centre of parent 
;; node to the centre of child node.
;; EXAMPLE:Check test of world-to-scene for example.
;; Strategy: Use HOF foldr on Children.

(define (place-line-to-scene children x y scene)
  (foldr
   ;node scene -> scene
   ;GIVEN: A node and scene 
   ;Returns: A scene after placing the  blue line between the child node
   ;and parent node from the x and y co-ordinate of the of both nodes.
   ;STRATERGY:Use template for node on child.
   (lambda (child s)(scene+line (place-node child s)
                                x y (node-x child)
                                (node-y child) BLUE))
   scene
   children)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given
;; tree.
;; EXAMPLE: Check test
;; STRATEGY:Use template for node on node
(define (tree-to-sons node)
  (node-nodes node))

;;TEST
(begin-for-test
  check-equal?
  (tree-to-sons MOUSE-NODE2)
  '()
  "returns the nodes associated with the immediate subtrees")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-to-center : Node -> Posn
;; GIVEN: A node
;; RETURNS: the center of the given node as it is to be displayed on the
;; scene.
;; EXAMPLE:Check Test for example.
;; STRATEGY:Use template for node on node
(define (node-to-center node)
  (make-posn (node-x node)(node-y node)))

;;TEST
(begin-for-test
  check-equal?
  (node-to-center (make-node 290 70 0 0 #f empty))
  (make-posn 290 70)
  " the position will be made")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected
;; EXAMPLE:Check test.
;; DESIGN-STRATEGY:Use template for node on node
(define (node-to-selected? node)
  (node-selected? node))

;;TEST
(begin-for-test
  (check-equal?
   (node-to-selected?(make-node 290 70 0 0 #f empty))
   #false)
  "if the node is selected or not")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLE:Check test.
;; STRATEGY:Use template for world on w
(define (world-to-trees w)
  (world-trees w))

;;TEST
(begin-for-test
  (check-equal?
   (world-to-trees INITIAL-WORLD6)
   (list(make-node 260 10 0 0 #f (list(make-node 290 40 0 0 #t
                                                 (list(make-node 290 70 0 0 #f '()))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE:Check test.
;; STRATEGY: Combine simpler function.
(define (tree-to-root node)
  node)

;;TEST
(begin-for-test
  (check-equal? (tree-to-root ROOT-NODE)ROOT-NODE
                "Should return  the same node"))

;**************************************************************************************
;                       END OF THE PROGRAM
;**************************************************************************************

