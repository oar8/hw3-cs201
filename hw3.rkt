#lang racket

(provide hours
         ins ins-c-state ins-c-symbol ins-n-state ins-n-symbol ins-dir
         tm-reverse
         i-match? i-lookup
         conf conf-state conf-ltape conf-symbol conf-rtape
         halted? change-state write-symbol
	 normalize
         shift-head-left shift-head-right
         next-config
         tm-convert
	 tm-sort)

; Please do not modify the lines above this comment.
; ********************************************************
; CS 201 HW #3  DUE Sunday, 10/8/2017 at 11:59 pm
;                via the submit system on the Zoo
; ****************************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
; ****************************************************************

; Unless the problem specifies otherwise:
; (a) You may solve the problem using any method and any Racket constructs 
; (*except* mutators, that is, set! and its relatives.)
; (b) You may write auxiliary procedure(s) in addition to the one(s) specified in the problem.  
; Please include a comment for each one specifying what it does and giving one or more examples of it.
; (c) Please make your code as clear and readable as possible.

; The topics of this assignment are:
; a simulator for Turing machines and writing Turing machine programs.

; ****************************************************************
; ** problem 0 ** (1 easy point)
; Modify the following definition to reflect the number of hours you spent on this assignment.

(define hours 8)

; ****************************************************************
; Turing machines were described in the lectures; see also the lecture notes on the course web page.
; Here is a top-level procedure to simulate a Turing machine starting from a given configuration until 
; either it halts or it has executed n steps.
; The procedure returns the list of the successive configurations of the computation,
; starting with the initial one.
; The length of the list of configurations is one more than 
; the number of steps taken by the machine.

(define (simulate mach config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? mach config) (list config))
    (else
     (cons config
           (simulate 
            mach (next-config mach config) (- n 1))))))

; mach is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; The procedures halted? and next-config will be
; written by you in the problems below; you will then
; have a complete Turing machine simulator.

; ****************************************************************
; Turing machine representation.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a struct
; defined as follows:

(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent) 

; The fields represent the following components of an instruction:
; current state, current symbol, new state, new symbol, and move direction

; The current state and new state are Racket symbols,
; the current symbol and new symbol are Racket symbols or non-negative integers
; and the move direction must be either the symbol 'L or the symbol 'R,
; representing a move to the left or right, respectively.

; Example
; > (define i1 (ins 'q1 0 'q3 1 'L))
; creates an instruction with with current state 'q1, current symbol 0,
; new state 'q3, new symbol 1, and move direction 'L,
; and names it i1.

; Because we've made ins "transparent", its field values
; will be printed out.
; > i1
; (ins 'q1 0 'q3 1 'L)

; We can access the components of i1 via the structure selectors:
; (ins-c-state i1) => 'q1
; (ins-c-symbol i1) => 0
; (ins-n-state i1) => 'q3
; (ins-n-symbol i1) => 1
; (ins-dir i1) => 'L

; Example (from lecture):
; a Turing machine that when started in state 'q1 on the leftmost of a string of 0's and 1's 
; changes all the 0's to 1's and all the 1's to 0's 
; and then returns the head to the leftmost symbol and halts.

(define tm1 
  (list
   (ins 'q1 0 'q1 1 'R)
   (ins 'q1 1 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'b 'q3 'b 'R)))

; ****************************************************************
; ** problem 1 (15 points)
; Define (in the format just given) a Turing machine named

; tm-reverse

; that takes an input string of 0's and 1's 
; and produces an output string equal to the reverse of the input string.
; When the machine halts, the head should be scanning the leftmost symbol 
; of the output.

; That is, when started in state q1 with the head on the leftmost of a
; string of 0's and 1's, it halts with the head on the leftmost of a
; string of 0's and 1's, and the output string is obtained from
; the input string by reversing it.

; Your machine *may* use additional tape symbols but the output should contain no
; symbols other than 0, 1 and blank.
; When the machine halts, symbols other than the output should be blank.

; Examples of the behavior of tm-reverse
; 1            =>  1
; 110          =>  011
; 0001         =>  1000
; 101011       =>  110101

; (It may help to review ideas from the machine to make a copy of its input,
; described in lectures and in the online lecture notes.)

; The initial state of your machine should be q1 -- other states may be named with
; Racket symbols of your choice.

; IMPORTANT: please describe how your Turing machine works.
; You'll be able to run it once you get the procedures for the simulator working.

; ****************************************************************
(define tm-reverse

  (list
   ;my turing machine creates a reversed list by identifying an end point of the given list/start point of the
   ;new list ('c) and then taking the first 0 or 1 before 'c (this would be a digit at the end of the given list) and replacing it with a 'd. It then places the 0 or
   ;1 at the end of the new list and cycles back to find the next 0 or 1 in the given list.
   ;The cycle repeats until there are no 0s or 1s before 'c. The head then moves to point at the first digit in the reversed list.

   ;moves right over the given list and places a 'c in the first blank spot
   (ins 'q1 0 'q1 0 'R); move right over 0's
   (ins 'q1 1 'q1 1 'R); move right over 1's
   (ins 'q1 'b 'q2 'c 'L); change first blank to c, move left & go to q2

   ;these instructions traverse the list from right to left
   ;takes the first 0 or 1 and replaces it with a 'd (to show that this number has been replicated) and transitions to a new state
   ;moves over 'c and 'd b/c 'c does not need to be replicated and 'd stands for a number that has already been replicated
   ;once the head has gone over all the numbers in the sequence (so they will all be d's) and hits a 'b, the reverse has been completed
   (ins 'q2 0 'q3 'd 'R);change 0 to 'd & move to state q3
   (ins 'q2 1 'q4 'd 'R);change 1 to 'd & move to state q4
   (ins 'q2 'c 'q2 'c 'L);move left over 'c
   (ins 'q2 'd 'q2 'd 'L);move left over 'd
   (ins 'q2 'b 'q5 'b 'R);move right over 'b & move to state q5

   ;these instructions are enacted when q2 finds a zero in the string of numbers
   ;the head moves right along the string until it finds a blank sqaure, and then it places a zero in the blank & moves to state q6
   (ins 'q3 'd 'q3 'd 'R);moves right over d
   (ins 'q3 'c 'q3 'c 'R);moves right over c
   (ins 'q3 'b 'q6 0 'L);replaces the blank with a 0 and calls q6
   (ins 'q3 1 'q3 1 'R);moves right over 1
   (ins 'q3 0 'q3 0 'R);moves right over 0

   ;these instructions are enacted when q2 finds a one in the string of numbers
   ;the head moves right along the string until it finds a blank sqaure, and then it places a one in the blank & moves to state q6
   (ins 'q4 'd 'q4 'd 'R);moves right over d
   (ins 'q4 'c 'q4 'c 'R);moves right over c
   (ins 'q4 'b 'q6 1 'L);replaces the blank with a 0 and calls q6
   (ins 'q4 1 'q4 1 'R);moves right over 1
   (ins 'q4 0 'q4 0 'R);moves right over 0
   
   ;these instructions move the head over anything until it reaches 'c
   ;once the head reaches 'c, it moves right once and halts
   ;the head is now pointing at the first item in the reversed string
   (ins 'q5 0 'q5 0 'R);move right over 0
   (ins 'q5 1 'q5 1 'R);move right over 1
   (ins 'q5 'd 'q5 'd 'R);move right over d
   (ins 'q5 'b 'q5 'b 'R);move right over b
   (ins 'q5 'c 'q99 'c 'R);move right over c and HALT

   ;these instructions are called when a 0 or 1 has been placed at the end of the new reversed string
   ;it moves the head left over any digits that have already been copied
   ;when the head reaches 'c, it moves left and calls q2 to start the copying process over again
   (ins 'q6 0 'q6 0 'L);moves left over 0
   (ins 'q6 1 'q6 1 'L);moves left over 1
   (ins 'q6 'c 'q2 'c 'L);moves left, and calls q2
   ))


; ****************************************************************
; ** problem 2 (10 points)
; Write the following two procedures.
; Remember to use the instruction selectors:
; ins-c-state, ins-c-symbol, ins-n-state, ins-n-symbol, ins-dir

; (i-match? state symbol inst)
; returns #t if state and symbol are equal to 
; the state and symbol of instruction inst
; otherwise returns #f

; (i-lookup state symbol mach)
; returns #f if no instruction of Turing machine mach 
; has state and symbol equal to state and symbol
; otherwise returns the instruction in mach that matches.
; You may assume that at most one instruction will match.

; Examples
; (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) => #t
; (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) => #f
; (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) => #t
; (equal? (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R)) => #t
; (equal? (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R)) => #t
; (i-lookup 'q3 1 tm1) => #f
; ****************************************************************

(define (i-match? state symbol inst)
  (if (and (equal? state (ins-c-state inst)) (equal? symbol (ins-c-symbol inst)))
      #t
      #f))

(define (i-lookup state symbol mach)
  (if (empty? mach)
      #f
      (if (equal? (i-match? state symbol (car mach)) #t)
          (car mach)
          (i-lookup state symbol (rest mach)))))


; ****************************************************************
; Representation of a Turing machine configuration.
; We represent a Turing machine configuration using the following structure:

(struct conf (state ltape symbol rtape) #:transparent)

; where state is the current state of the machine,
; ltape is a list of symbols to the left of the currently scanned symbol,
; symbol is the currently scanned symbol,
; rtape is a list of symbols to the right of the currently scanned symbol.

; We reserve the symbol 'b for the blank.

; For example, we define the following two configurations:

(define config1 (conf 'q3 '(0 0) 1 '(1)))
(define config2 (conf 'q6 '(1 b) 0 '(b b)))

; Note that the selectors are
; conf-state, conf-ltape, conf-symbol, conf-rtape

; config1 represents the Turing machine configuration

;   --------------------------
;   .. | 0 | 0 | 1 | 1 |  | ..
;   --------------------------
;                ^
;                q3

; in which the non-blank symbols on the tape are 0011,
; and the machine is in state q3 with the read/write head
; scanning the leftmost 1.

; config2 represents the Turing machine configuration

;   ------------------------------
;   .. |   | 1 |  | 0 |   |   | ..
;   ------------------------------
;                   ^
;                   q6

; in which the symbols 1, blank, 0, are on the tape, surrounded
; by blanks, and the machine is in state q6 with the read/write
; head scanning the 0.

; A configuration is *normalized* if neither the first symbol of
; ltape nor the last symbol of rtape is the symbol 'b.
; Of the two configurations above, config1 is normalized, 
; but config2 is not (the last element of its rtape list is 'b.)

; Note that tape squares not explicitly represented are
; assumed to contain blanks.  A normalized configuration
; to represent the machine in state q1 with all tape squares
; blank is thus (conf 'q1 '() 'b '())).

; ****************************************************************
; ** problem 3 (9 points)
; Write the following three procedures.

; (halted? mach config)
; returns #t if the Turing machine mach is halted in machine configuration config 
; (ie, no instruction of the machine matches the current state and symbol 
; in configuration config) and returns #f otherwise.

; (change-state new-state config)
; takes a configuration config and returns a configuration
; in which the state of the machine is changed to new-state.

; (write-symbol new-symbol config) takes a configuration config and
; returns a configuration in which the symbol scanned by 
; the read/write head has been replaced by new-symbol.

; Examples
; (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) => #f
; (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) => #t
; (change-state 'q2 (conf 'q1 '(0) 1 '())) => (conf 'q2 '(0) 1 '())
; (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) => (conf 'q13 '(0 1 1) 'b '())
; (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) => (conf 'q5 '(0) 1 '(1 1))
; (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) => (conf 'q2 '(0 0 1) 'c '(1 1))
; (write-symbol 'b (conf 'q3 '(1) 0 '())) => (conf 'q3 '(1) 'b '())
; ****************************************************************

(define (halted? mach config)
  (if (empty? mach)
      #t
      (if (i-match? (conf-state config) (conf-symbol config) (car mach))
          #f
          (halted? (rest mach) config))))

(define (change-state state config);change state to state
  (conf state (conf-ltape config)(conf-symbol config)(conf-rtape config)))

(define (write-symbol new-symbol config);change symbol to new-symbol
   (conf (conf-state config) (conf-ltape config)new-symbol(conf-rtape config)))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write one procedure

; (normalize config)
; takes a Turing machine configuration config and returns an equivalent 
; *normalized* configuration. That is, the same Turing machine configuration is
; represented by the input configuration and the output configuration, 
; and the output configuration does not have a 'b as the first element 
; of its ltape list or the last element of its rtape list.

; Examples
; (normalize config1) => (conf 'q3 '(0 0) 1 '(1))
; (normalize config2) => (conf 'q6 '(1 b) 0 '())
; (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) => (conf 'q3 '(0) 'b '(1 1 0))
; (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) => (conf 'q6 '(0 b 0) 1 '(0 b 0))
; (normalize (conf 'q4 '(b b) 'b '(b b b))) => (conf 'q4 '() 'b '())
; ****************************************************************


(define (normalize config)
  (conf (conf-state config) (helpLEFT(conf-ltape config)) (conf-symbol config) (helpRIGHT(reverse(conf-rtape config)))))

(define (helpLEFT lst)
  ;If lst is empty, return an empty list. Otherwise, if the first item in the list is a blank, remove the 'b by taking (rest lst),
  ;and pass it through the helpLEFT again to see if there are any more blanks to remove. If the first item is not a blank, return the lst.

  ;for example:
  ;(helpLEFT '(b b b 1)) would call
  ;(helpLEFT '(b b 1)) which would call
  ;(helpLEFT '(b 1)) which would call
  ;(helpLEFT '(1)) which would return
  ;'(1)
  (if (empty? lst);If lst is empty, return an empty list
      '()
      (if (equal? (car lst) 'b);Otherwise, if the first item in the list is a blank,
          (helpLEFT (rest lst));remove the 'b by taking (rest lst), and pass it through the helpLEFT again to see if there are any more blanks to remove
          lst)));If the first item is not a blank, return the lst.
          

(define (helpRIGHT lst)
  ;If lst is empty, return an empty list. Otherwise, if the first item in the list is a blank, remove the 'b by taking (rest lst),
  ;and pass it through the helpLEFT again to see if there are any more blanks to remove. If the first item is not a blank, return the lst reversed.
  ;the reversed list is returned, because, if you look at the call to helpRIGHT in normalize, a reversed list is passed into helpRIGHT.
  ;by reversing the list when it is returned, we are putting the numbers/characters back into the order they originally were in.

  ;for example:
  ;(helpLEFT '(b b b 1 0 b)) would call
  ;(helpLEFT '(b b 1 0 b)) which would call
  ;(helpLEFT '(b 1 0 b)) which would call
  ;(helpLEFT '(1 0 b)) which would return
  ;'(b 1 0)
  (if (empty? lst)
      '()
      (if (equal? (car lst) 'b)
          (helpRIGHT (rest lst))
          (reverse lst))))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (shift-head-left config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the left.

; (shift-head-right config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the right.

; Examples
; (shift-head-left (conf 'q5 '() 'b '())) => (conf 'q5 '() 'b '())
; (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) => (conf 'q6 '(0) 0 '(1 1 1))
; (shift-head-left (conf 'q7 '() 0 '(1 1 0))) => (conf 'q7 '() 'b '(0 1 1 0))
; (shift-head-right (conf 'q2 '() 'b '())) => (conf 'q2 '() 'b '())
; (shift-head-right (conf 'q9 '() 0 '(1 1 1))) => (conf 'q9 '(0) 1 '(1 1))
; (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) => (conf 'q8 '(1 0 1 1 b) 'b '())
; ****************************************************************

(define (shift-head-left config)
  (if (empty? (conf-ltape (normalize config)))
      (normalize (conf (conf-state config) '() 'b (append (list (conf-symbol config))(conf-rtape config))))
      (normalize (conf (conf-state config) (reverse (rest (reverse (conf-ltape config)))) (first (reverse(conf-ltape config))) (append (list (conf-symbol config))(conf-rtape config))))))

(define (shift-head-right config)
  (if (empty? (conf-rtape (normalize config)))
      (normalize (conf (conf-state config) (append (conf-ltape config)(list (conf-symbol config))) 'b '() ))
      (normalize (conf (conf-state config) (append (conf-ltape config)(list (conf-symbol config))) (first (conf-rtape config)) (rest (conf-rtape config))))))


; ****************************************************************
; ** problem 6 ** (15 points)
; Write a procedure 

; (next-config mach config)
; takes a Turing machine mach and a normalized configuration config
; and returns the normalized next configuration 
; for the Turing machine mach in the configuration config.
; If there is no applicable instruction, the configuration
; returned should be just the input configuration.

; Hint: get your procedures
; halted?, i-lookup, write-symbol, shift-head-left, shift-head-right
; working and combine them appropriately.

; Examples
; (next-config tm1 (conf 'q1 '() 0 '(0 1))) => (conf 'q1 '(1) 0 '(1))
; (next-config tm1 (conf 'q1 '(1) 0 '(1))) => (conf 'q1 '(1 1) 1 '())
; (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) => (conf 'q2 '(1 1) 0 '())
; (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) => (conf 'q3 '() 1 '(1 0))
; (next-config tm1 (conf 'q3 '() 1 '(1 0))) => (conf 'q3 '() 1 '(1 0))

;(define tm1 
;  (list
;   (ins 'q1 0 'q1 1 'R)
;   (ins 'q1 1 'q1 0 'R)
;   (ins 'q1 'b 'q2 'b 'L)
;   (ins 'q2 0 'q2 0 'L)
;   (ins 'q2 1 'q2 1 'L)
;   (ins 'q2 'b 'q3 'b 'R)))
; ****************************************************************
(define (next-config mach config);does the next thing on a turing machine
  (cond ((halted? mach config) config)
        (else (let* ([setting (i-lookup (conf-state config) (conf-symbol config) mach)]
                     [curConf (conf (ins-n-state setting) (conf-ltape config) (ins-n-symbol setting) (conf-rtape config))])
                (if (equal? (ins-dir setting) 'R)
                    (shift-head-right curConf)
                    (shift-head-left curConf))))))
                

; ****************************************************************
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive normalized configurations 
; of Turing machine tm1 when run from the given configuration.

;> (simulate tm1 (conf 'q1 '() 1 '(1 0 1 0)) 20)
;(list
; (conf 'q1 '() 1 '(1 0 1 0))
; (conf 'q1 '(0) 1 '(0 1 0))
; (conf 'q1 '(0 0) 0 '(1 0))
; (conf 'q1 '(0 0 1) 1 '(0))
; (conf 'q1 '(0 0 1 0) 0 '())
; (conf 'q1 '(0 0 1 0 1) 'b '())
; (conf 'q2 '(0 0 1 0) 1 '())
; (conf 'q2 '(0 0 1) 0 '(1))
; (conf 'q2 '(0 0) 1 '(0 1))
; (conf 'q2 '(0) 0 '(1 0 1))
; (conf 'q2 '() 0 '(0 1 0 1))
; (conf 'q2 '() 'b '(0 0 1 0 1))
; (conf 'q3 '() 0 '(0 1 0 1)))

; ****************************************************************
; ** problem 7 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-convert

; that takes as input a positive integer n represented in binary
; and produces as output a string of n x's.  When the machine
; halts, the read/write head should be positioned over the
; leftmost x in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; there should be just n x's, surrounded by blanks, on the tape.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-convert
; 1            => x
; 110          => xxxxxx
; 1111         => xxxxxxxxxxxxxxx

; Here are input configurations if you want to simulate your tm-convert on
; these inputs.

(define conv1 (conf 'q1 '() 1 '()))
(define conv6 (conf 'q1 '() 1 '(1 0)))
(define conv15 (conf 'q1 '() 1 '(1 1 1)))
(define sort-liv (conf 'q1 '() 1 '(0 1)))
; ****************************************************************

(define tm-convert
  (list
   ;my Turing machine convers from binary to unary by identifying the end of the given list/start of the new list ('c).
   ;It then calculates the unary number by moving left across the binary number until the head is pointing at a 1.
   ;It then changes this 1 to a 0. The head then moves right, changing every 0 to 1. Once the head reaches c, it continues
   ;right and adds an 'x into the first blank spot. The head then moves back to 'c and this process repeats until the given
   ;number has no 1's and is all 0's

   ;here is an example of how this would work with 101 (5):
   ;101 -> 100 -> 011 -> 010 -> 001 -> 000
   ;    x      x      x      x      x   -> each change in the given string of 1's and 0's adds an 'x to the new list

   ;go to the end of the list and add a 'c'
   (ins 'q1 0 'q1 0 'R);move right over 0's
   (ins 'q1 1 'q1 1 'R);move right over 1's
   (ins 'q1 'b 'q2 'c 'L);change first blank to c, move left, and change state to q2

   ;move left until a 1 is found. pass over 0s. when a 1 is found, change state to 'q3.
   ;if no one is found, and the pointer goes far enough to reach a blank, change state to 'q7 and go right.
   (ins 'q2 0 'q2 0 'L);move left over 0s
   (ins 'q2 1 'q3 0 'R);change the 1 to 0 and go to state q3
   (ins 'q2 'b 'q7 'b 'R);move right and change to state 'q7

   ;go right along the given string of numbers and change any 0 to 1 and any 1 to 0.
   ;Once you've transversed the given string and reached 'c, move to state q4
   (ins 'q3 0 'q3 1 'R);change 0 to 1 and move right
   (ins 'q3 1 'q3 0 'R);change 1 to 0 and move right
   (ins 'q3 'c 'q4 'c 'R);change to state q4

   ;move right until a blank is reached. replace the blank with an 'x and change to state q5
   (ins 'q4 0 'q4 0 'R);move right over 0
   (ins 'q4 1 'q4 1 'R);move right over 1
   (ins 'q4 'x 'q4 'x 'R);move right over x
   (ins 'q4 'b 'q5 'x 'L);change blank to an x and go to state q5

   ;move left until you reach 'c. then change the state to q2
   (ins 'q5 0 'q5 0 'L);move left over 0
   (ins 'q5 1 'q5 1 'L);move left over 1
   (ins 'q5 'x 'q5 'x 'L);move left over x
   (ins 'q5 'c 'q2 'c 'L);at 'c move left and change the state to q2

   ;pass over all 0's until 'c is reached. at 'c, move right (so that the pointer is on the first x) and halt.
   (ins 'q7 0 'q7 0 'R);move right over 0s
   (ins 'q7 'c 'q99 'c 'R);at 'c, move right and change state to q99
     
   ))

; ****************************************************************
; ** problem 8 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-sort

; that takes as input a non-empty string of 0's and 1's
; and produces as output a string of 0's and 1's equal to the input
; string rearranged to have all the 0's before all the 1's.
; When the machine halts, the read/write head should be positioned over the
; leftmost 0 or 1 in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; the only non-blank symbols on the tape should be the output string.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-sort
; 0          => 0  
; 1          => 1
; 00         => 00
; 110        => 011
; 1011011    => 0011111

; Here are some input configurations if you want to simulate your tm-sort on
; these inputs.

(define sort0 (conf 'q1 '() 0 '()))
(define sort1 (conf 'q1 '() 1 '()))/
(define sort00 (conf 'q1 '() 0 '(0)))
(define sort110 (conf 'q1 '() 1 '(1 0)))
(define sort111 (conf 'q1 '() 1 '(1 1)))
(define sort1010 (conf 'q1 '() 1 '(0 1 0)))
(define sort-longish (conf 'q1 '() 1 '(1 0 0 1 0)))
(define sort-long (conf 'q1 '() 1 '(0 1 1 0 1 1)))
; ****************************************************************

(define tm-sort
  (list
   ;my Turing machine takes a given string of 0s and 1s and sorts them by placing a 'c at the end of the given list/start of the new list.
   ;the machine then traverses the list from left to right, replaces zero with 'd, and then adds a 0 into the end of the new list.
   ;this continures until there are no more zeros in the list.
   ;Once all the 0's have been made into d's, the machine the then traverses the list from left to right, replaces 1 with 'e, and
   ;then adds a 1 into the end of the new list. this continues until there are no more ones in the list.
   ;the moved is then moved so it points at the first digit in the new, sorted list.

   ;go to the end of the list and add a 'c'
   (ins 'q1 0 'q1 0 'R);move right over 0's
   (ins 'q1 1 'q1 1 'R);move right over 1's
   (ins 'q1 'b 'q2 'c 'L);change first blank to c, move left, and change state to q2

   ;goes left over the given list until a blank is reached
   ;the head then goes right, so that it is pointing at the first item in the list
   (ins 'q2 0 'q2 0 'L);move left over 0’s
   (ins 'q2 1 'q2 1 'L);move left over 1’s
   (ins 'q2 'b 'q3 'b 'R);at first blank, the end of the given list has been reached. move right and change state to q3

   ;finds the next 0 in the list and replaces it with a 'd
   (ins 'q3 0 'q4 'd 'R);change 0 to d, move right to q4
   (ins 'q3 1 'q3 1 'R);move right over 1
   (ins 'q3 'c 'q8 'c 'L);c means copying of 0s is over, move left, and change state to q8

   ;goes right until a blank is reached. replaces the blank with a 0.
   (ins 'q4 0 'q4 0 'R);move right over 0’s
   (ins 'q4 1 'q4 1 'R);move right over 1’s
   (ins 'q4 'c 'q4 'c 'R);move right over c
   (ins 'q4 'b 'q6 0 'L);replace first blank with 0, move left to q6

   ;moves left across the list until a d is found (this will be the last 0 that was copied)
   (ins 'q6 0 'q6 0 'L);move left over 0’s
   (ins 'q6 1 'q6 1 'L);move left over 1’s
   (ins 'q6 'c 'q6 'c 'L);move left over c
   (ins 'q6 'd 'q3 'd 'R);at the first d, move right and change to state q3
      
   ;moves left until you reach the front of the chain
   (ins 'q8 'd 'q8 'd 'L);move right over 'd
   (ins 'q8 1 'q8 1 'L);move right over 1
   (ins 'q8 'b 'q9 'b 'R);at first blank, the end of the given list has been reached. move right and change state to q9

   ;move right until you hit a 1, replace it with an 'e
   (ins 'q9 'd 'q9 'd 'R);move right over d
   (ins 'q9 1 'q10 'e 'R);change 1 to e and change to state q10
   (ins 'q9 'c 'q99 'c 'R);when c has been reached, all 1's have been copied.
   ;move the head to the right so that it is pointing at the first item in the sorted list and HALT

   ;move left until you hit a blank and place the 1 there
   (ins 'q10 'd 'q10 'd 'R);move right over d
   (ins 'q10 'c 'q10 'c 'R);move right over c
   (ins 'q10 0 'q10 0 'R);move right over 0
   (ins 'q10 1 'q10 1 'R);move right over 1
   (ins 'q10 'b 'q11 1 'L);replace the first blank with a 1, move left, and change state to q11
   
   ;moves left across the string until an e is found (this will be the last 1 that was copied)
   (ins 'q11 'd 'q11 'd 'L);move left over d
   (ins 'q11 0 'q11 0 'L);move left over 0
   (ins 'q11 1 'q11 1 'L);move left over 1
   (ins 'q11 'c 'q11 'c 'L);move left over c
   (ins 'q11 'e 'q9 'e 'R);at the first e, move right and change to state q3
   
   ))


; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'i-match? (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) #t)
(test 'i-match? (i-match? 'q1 'o (ins 'q1 'b 'q3 'b 'L)) #f)
(test 'i-match? (i-match? 'q99 'b (ins 'q1 'b 'q3 'b 'L)) #f)
(test 'i-match? (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) #f)
(test 'i-match? (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) #t)

(test 'i-lookup (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R))
(test 'i-lookup (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R))
(test 'i-lookup (i-lookup 'q3 1 tm1) #f)
(test 'i-lookup (i-lookup 'q2 'c tm1) #f)

(test 'halted? (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) #f)
(test 'halted? (halted? tm1 (conf 'q2 '(1 1 0) 'c '())) #t)
(test 'halted? (halted? tm1 (conf 'q3 '(1 1 0) 'b '())) #t)
(test 'halted? (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) #t)

(test 'change-state (change-state 'q2 (conf 'q1 '(0) 1 '())) (conf 'q2 '(0) 1 '()))
(test 'change-state (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) (conf 'q13 '(0 1 1) 'b '()))
(test 'change-state (change-state 'q0 (conf 'q0 '(0) 1 '())) (conf 'q0 '(0) 1 '()))

(test 'write-symbol (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) (conf 'q5 '(0) 1 '(1 1)))
(test 'write-symbol (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) (conf 'q2 '(0 0 1) 'c '(1 1)))
(test 'write-symbol (write-symbol 'b (conf 'q3 '(1) 0 '())) (conf 'q3 '(1) 'b '()))

(test 'normalize (normalize config1) (conf 'q3 '(0 0) 1 '(1)))
(test 'normalize (normalize config2) (conf 'q6 '(1 b) 0 '()))
(test 'normalize (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) (conf 'q3 '(0) 'b '(1 1 0)))
(test 'normalize (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) (conf 'q6 '(0 b 0) 1 '(0 b 0)))
(test 'normalize (normalize (conf 'q4 '(b b) 'b '(b b b))) (conf 'q4 '() 'b '()))
(test 'normalize (normalize (conf 'q3 '(b b b 0 b b) 'b '(b b 1 1 0 b b))) (conf 'q3 '(0 b b) 'b '(b b 1 1 0)))
(test 'normalize (normalize (conf 'q3 '(b 0 b 0 b 0) 'b '(1 1 0 b b b b b b b b))) (conf 'q3 '(0 b 0 b 0) 'b '(1 1 0)))


(test 'shift-head-left (shift-head-left (conf 'q5 '() 'b '())) (conf 'q5 '() 'b '()))
(test 'shift-head-left (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) (conf 'q6 '(0) 0 '(1 1 1)))
(test 'shift-head-left (shift-head-left (conf 'q7 '() 0 '(1 1 0))) (conf 'q7 '() 'b '(0 1 1 0)))
(test 'shift-head-left (shift-head-left (conf 'q123 '(0 0 c) 1 '(1 1))) (conf 'q123 '(0 0) 'c '(1 1 1)))

(test 'shift-head-right (shift-head-right (conf 'q7 '(1 0 1) 'c '())) (conf 'q7 '(1 0 1 c) 'b '()))
(test 'shift-head-right (shift-head-right (conf 'q2 '() 'b '())) (conf 'q2 '() 'b '()))
(test 'shift-head-right (shift-head-right (conf 'q9 '() 0 '(1 1 1))) (conf 'q9 '(0) 1 '(1 1)))
(test 'shift-head-right (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) (conf 'q8 '(1 0 1 1 b) 'b '()))


(test 'next-config (next-config tm1 (conf 'q1 '() 0 '(0 1))) (conf 'q1 '(1) 0 '(1)))
(test 'next-config (next-config tm1 (conf 'q1 '(1) 0 '(1))) (conf 'q1 '(1 1) 1 '()))
(test 'next-config (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) (conf 'q2 '(1 1) 0 '()))
(test 'next-config (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) (conf 'q3 '() 1 '(1 0)))
(test 'next-config (next-config tm1 (conf 'q3 '() 1 '(1 0))) (conf 'q3 '() 1 '(1 0)))
(test 'next-config (next-config tm1 (conf 'q834927 '(1) 0 '(1))) (conf 'q834927 '(1) 0 '(1)))


; *************** end of hw3.rkt *********************************
