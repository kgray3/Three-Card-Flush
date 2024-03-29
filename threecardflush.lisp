

; Function: make-deck
; Adds all possible playing cards into a list and displays
( defun make-deck() 
    ( mapcan #'make-selected-deck '( CLUB DIAMOND HEART SPADE ))
)

; Function: make-selected-deck
; Pairs each rank with input suit
( defun make-selected-deck( suit &aux rank )
    ( setf rank '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE))
    ( setf suit-duplicates ( duplicate ( length rank ) suit ) )
    ( mapcar #'cons rank suit-duplicates )
)

; Function: demo--make-deck
; Function that tests make-deck()
( defun demo--make-deck ()
    ( format t ">>> Testing: make-deck~%" )
    ( setf deck ( make-deck ) )
    ( format t "--- deck = ~A~%" deck )
    ( format t "--- number of cards in deck = ~A~%" ( length deck ) )
    nil
)

; Function: duplicate
; Recursive function that prints input element lo n times
( defun duplicate (n lo)
    (cond
        ((= 1 n )
            (list lo )
        )
        (t
            (cons lo ( duplicate (- n 1) lo ) )
        )
    )
)

; Function: establish-shuffled-deck
; Sets property list *deck* to a shuffled deck
( defun establish-shuffled-deck ()
    ( setf *deck* ( shuffle ( make-deck ) ) )
    nil
)

; Function: shuffle
; Shuffles list of cards using random and recursion
( defun shuffle ( deck )
    (cond
        ((null deck )
            deck
        )
        (t
            ( setf card ( nth ( random ( length deck ) ) deck ) )
            ( setf deck ( remove card deck ) )
            ( cons card (shuffle deck ) )
        )
    )
)

; Function: demo--establish-shuffled-deck
; Checks if establish-shuffled-deck is working properly
( defun demo--establish-shuffled-deck ()
    ( format t ">>> Testing: shuffle-deck~%" )
    ( establish-shuffled-deck )
    ( format t "--- *deck* ... ~A~%" *deck* )
    ( format t "--- number of cards in *deck* = ~A~%" ( length *deck*))
    nil
)

; Function: deal-hands
; Deals cards from *deck* to *hand1* and *hand2*
( defun deal-hands ()
    ( establish-shuffled-deck )
    ( setf *hand1* () )
    ( setf *hand2* () )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    nil
)

; Function: deal-card-to-hand1
; Removes first card of *deck* and adds it to the end of *hand1*
( defun deal-card-to-hand1 ()
    ( setf card ( car *deck* ) )
    ( setf *deck* ( remove card *deck* ) )
    ( setf *hand1* ( append *hand1* ( list card ) ) )
)

; Function: deal-card-to-hand2
; Removes first card of *deck* and adds it to the end of *hand2*
( defun deal-card-to-hand2 ()
    ( setf card ( car *deck* ) )
    ( setf *deck* ( remove card *deck* ) )
    (setf *hand2* ( append *hand2* ( list card ) ) )
)

; Function: demo--deal-hands
; Tests the deal-hands method
( defun demo--deal-hands ()
    ( format t ">>> Testing: deal-hands~%" )
    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( format t "--- number of cards in *deck* = ~A~%" ( length *deck*))
    nil
)

; Function: randomly-discard-cards
; Function that discards one card from hand1 and one card from hand2
( defun randomly-discard-cards ()
    ( randomly-discard-card-from-hand1 )
    ( randomly-discard-card-from-hand2 )
    nil
)

; Function randomly-heuristically-discard-cards
; Function that randomly discards a card from hand1 and
; heuristically discards a card from hand2
( defun randomly-heuristically-discard-cards ()
    ( randomly-discard-card-from-hand1 )
    ( heuristic-discard-card-from-hand2 )
    nil
)

; Function: randomly-discard-card-from-hand1
; Function that randomly discards a card from hand1 and replaces it with nil
( defun randomly-discard-card-from-hand1 ()
    ( setf random-selection (nth (random (length '( LEFT CENTER RIGHT ) ) ) '( LEFT CENTER RIGHT ) ) )
    ( setf *hand1* ( replace-lcr random-selection nil *hand1* ) )
)

; Function: randomly-discard-card-from-hand2
; Function that randomly discards a card from hand2 and replaces it with nil
( defun randomly-discard-card-from-hand2 ()
    ( setf random-selection (nth (random (length '( LEFT CENTER RIGHT ) ) ) '( LEFT CENTER RIGHT ) ) )
    ( setf *hand2* ( replace-lcr random-selection nil *hand2* ) )
)

; Function: heuristic-discard-card-from-hand2
; Function that heuristically discards a card from hand2 and replaces it with nil
; The heuristics:
    ; 1. If 2/3 of the suits are the same, discard the different suit
    ; 2. If no suits match, discard the card of lowest rank
( defun heuristic-discard-card-from-hand2 ()
    ( setf suits ( mapcar #'cdr *hand2* ) )
    ( setf hand (copy-tree *hand2*) )
    (cond
        ; Check if two suits match and discard different suit
        ((equal ( first suits ) ( second suits ))
            ( setf *hand2* ( replace-lcr 'RIGHT nil *hand2* ) )
        )
        ((equal ( first suits ) ( third suits ))
            ( setf *hand2* ( replace-lcr 'CENTER nil *hand2* ) )
        )
        ((equal ( second suits ) ( third suits )) 
            ( setf *hand2* ( replace-lcr 'LEFT nil *hand2* ) )
        )
        ; If no suits match, discard the smallest card
        (t

            ( setf min-rank-list ( sort hand #'minimum-p ) )
            ( setf discard-card  ( car min-rank-list ) )
            (cond
                (( equal ( first *hand2* ) discard-card )
                    ( setf *hand2* ( replace-lcr 'LEFT nil *hand2* ) )
                )
                (( equal ( second *hand2* ) discard-card )
                    ( setf *hand2* ( replace-lcr 'CENTER nil *hand2* ) )
                )
                (( equal ( third *hand2* ) discard-card )
                    ( setf *hand2* ( replace-lcr 'RIGHT nil *hand2* ) )
                )
            )

        )
    )

)

; Function: minimum-p
; Returns true if a card's rank is less than the other card's rank
( defun minimum-p ( c1 c2 )
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( < ( position ( car c1 ) rank-order ) ( position ( car c2 )  rank-order ) )
)


; Function: demo--randomly-heuristically-discard-cards
; Tests randomly-heuristically-discard-cards function
( defun demo--randomly-heuristically-discard-cards ()
    ( format t ">>> Testing: randomly-discard-cards~%" )
    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( randomly-heuristically-discard-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)


; Function: demo--randomly-discard-cards
; Function that tests randomly-discard-cards ()
( defun demo--randomly-discard-cards ()
    ( format t ">>> Testing: randomly-discard-cards~%" )
    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( randomly-discard-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)

; Function Name: replace-lcr
; Takes three arguments:
; - LOCATION: Either the token LEFT or CENTER or RIGHT
; - ELEMENT: some Lisp object
; - LIST: a list of length 3
; returns one value: a newly created list which contains the same three
; elements as the LIST parameter except that the LEFT or CENTER
; or RIGHT element is replaced by the ELEMENT argument, as
; indicated by the LOCATION argument
( defun replace-lcr (location element li)
    (cond
        ((equal location 'LEFT)
            ( setf new-list (cons element (cdr li) ) )
            new-list
        )
        ((equal location 'CENTER)
            (list (car li) element (caddr li) )
        )
        ((equal location 'RIGHT)
            (list (first li) (second li) element)
        )
        
    )
    
)

; Function: replace-cards
; Used to replace nil in *hand1* and *hand2*
( defun replace-cards ()
    ( replace-card-in-hand1 )
    ( replace-card-in-hand2 )
    nil
)

; Function: replace-card-in-hand1
; Picks the top card from *deck*, checks the position of nil in *hand1*,
; and replaces nil with selected card
( defun replace-card-in-hand1 ()
    ( setf card ( car *deck* ) )
    ( setf *deck* ( remove card *deck* ) )
    (cond
        (( equal (first *hand1*) nil )
            ( setf *hand1* ( replace-lcr 'LEFT card *hand1*))
        )
        (( equal (second *hand1*) nil)
            ( setf *hand1* ( replace-lcr 'CENTER card *hand1* ))
        )
        (( equal (third *hand1*) nil)
            ( setf *hand1* ( replace-lcr 'RIGHT card *hand1*))
        )
    )
)

; Function: replace-card-in-hand2
; Picks the top card from *deck*, checks the position of nil in *hand2*,
; and replaces nil with selected card
( defun replace-card-in-hand2 ()
    ( setf card ( car *deck* ) )
    ( setf *deck* ( remove card *deck* ) )
    (cond
        (( equal (first *hand2*) nil )
            ( setf *hand2* ( replace-lcr 'LEFT card *hand2*))
        )
        (( equal (second *hand2*) nil)
            ( setf *hand2* ( replace-lcr 'CENTER card *hand2* ))
        )
        (( equal (third *hand2*) nil)
            ( setf *hand2* ( replace-lcr 'RIGHT card *hand2*))
        )
    )
)

; Function: demo--replace-cards
; Tests the replace-cards function
( defun demo--replace-cards ()
    ( format t ">>> Testing: replace-cards~%" )
    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( randomly-discard-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( replace-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)

; Function: players-each-take-a-turn
; Randomly discard a card from *hand1* and *hand2* and replace those cards
( defun players-each-take-a-turn ()
    ( randomly-heuristically-discard-cards )
    ( replace-cards )
    nil
)

; Function: demo--players-each-take-a-turn
; Tests players-each-take-a-turn function
( defun demo--players-each-take-a-turn ()
    ( format t ">>> Testing: players-each-take-a-turn~%" )
    ( deal-hands )
    ( format t "--- The hands ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)

; Function Name: uniform-p
; Takes one argument: a list
; Returns one value: T if no two elements in the list are
; different from one another; NIL if not
( defun uniform-p (li)
    (cond
        ((null li)
            t
        )
        (( = 1 ( length li))
            t
        )
        (( not (equal (car li) (second li) ) )
            nil
        )
        (t
            ( uniform-p ( cdr li ) )
        )
    )
)

; Function Name: flush-p
; Takes one argument: A list of dotted pairs which represent a
; of playing cards.
; Returns one value: T if the cards are all of the same suit; NIL if not.
( defun flush-p (hand)
    (setf card-list (mapcar #'cdr hand))
    (uniform-p card-list)
)

( defun demo--flush-p ( &aux hand )
    ( format t ">>> Testing: flush-p~%" )
    ( setf hand '( ( 2 . club ) ( ace . club ) ( 10 . club ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
    ( setf hand '( ( jack . diamond ) ( 9 . diamond ) ( 5 . diamond ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
    ( setf hand '( ( jack . heart ) ( 10 . heart ) ( 9 . heart ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
    ( setf hand '( ( 2 . spade) ( 3 . spade ) ( ace . spade ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
    ( setf hand '( ( 10 . spade) ( 5 . diamond ) ( ace . spade ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
    ( setf hand '( ( 8 . club) ( 9 . diamond ) ( 10 . heart ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
)

; Function: high-card
; Returns the card with the highest rank given a list of cards
( defun high-card ( hand )
    ( setf hand-ranks ( mapcar #'car hand ) )    
    ( setf highest-rank ( maximum-card hand-ranks ) )

    ( setf highest-card ( assoc highest-rank hand ) )
    highest-card

)

; Function: maximum-card
; Helper function of high-card that determines the highest
; rank given a list of ranks using recursion
( defun maximum-card ( li )
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf first-ele-position ( position ( car li ) rank-order ) )
    ( setf second-ele-position ( position ( second li ) rank-order ) )

    (cond
        ( ( = ( length li ) 1 ) 
            ( car li ) 
        )
        (( > first-ele-position second-ele-position  )
             ( maximum-card ( cons ( car li ) ( remove ( second li ) li ) ) )
        )
        (t
            ( maximum-card ( cdr li ) )
        )
    )
)

; Function: demo--high-card
; Tests the high-card function
( defun demo--high-card ()
    ( format t ">>> Testing: high-card~%" )
    ( setf hand '( ( 10 . heart ) ( 5 . club ) ( queen . spade ) ( 7 . heart ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )
    ( setf hand '( ( 2 . diamond ) ( 2 . club ) ( 10 . heart ) ( 4 . diamond )
        ( ace . club ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )
    ( setf hand '( ( ace . diamond ) ( ace . club ) ( 5 . spade ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )
    nil
)

; Function: straight-p
; Checks if a list of cards is a straight
( defun straight-p ( hand ) 
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf sorted-hand ( sort (mapcar #'car hand) #'sort-rank-p ) )

    ( straight-p-recurse sorted-hand )
)

; Function: straight-p-recurse
; Helper function of straight-p that takes a sorted rank list as input
; and checks if the absolute value of the difference of each consecutive
; element is equal to 1
( defun straight-p-recurse ( li )
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf p1 ( position ( car li ) rank-order ) )
    ( setf p2 ( position ( second li ) rank-order ) )
    (cond
        ( ( = ( length li ) 1 )
            t
        )
        ( ( not (  = ( abs ( - p1 p2 ) ) 1 ) ) 
            nil
        )
        (t
            ( straight-p-recurse ( cdr li ) )
        )
    )
)

; Function: sort-rank-p
; Helper function of straight-p that sorts ranks from lowest to highest
( defun sort-rank-p ( r1 r2 )
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf p1 ( position r1 rank-order ) )
    ( setf p2 ( position r2 rank-order ) )
    ( < p1 p2 )
)

; Function: demo--straight-p
; Tests straight-p function
( defun demo--straight-p ()
    ( format t ">>> Testing: straight-p~%" )
    ( setf hand '( ( 5 . spade) ( 3 . diamond ) ( 4 . spade ) ( 6 . club )) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )
    ( setf hand '( ( 5 . spade) ( 7 . diamond ) ( 4 . spade ) ( 8 . club ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )
    ( setf hand '( ( king . heart ) ( queen . diamond ) ( ace . spade ) ( 10 . club )
        ( jack . diamond ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )
    ( setf hand '( ( ace . club ) ( 2 . diamond ) ( 3 . spade ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )
    nil
)

; Function: analyze-hand
; Function that checks is a hand is a flush, the hand's high-card,
; and if the hand is a straight flush
( defun analyze-hand ( hand )
    (cond
        (( flush-p hand )
            (setf flush (list (cdr ( car hand ) ) 'FLUSH ) )
            (setf high ( list ( car (high-card hand) ) 'HIGH ) )
            
            (cond
                (( straight-p hand )
                    ( setf result ( append high '(STRAIGHT) flush ) )
                    result
                )
                (t
                    ( setf result ( append high flush ) )
                    result
                )
            )
        )
        (t
            'BUST
        )
    )
)

; Function: demo--analyze-hand
; Tests the analyze-hand function
( defun demo--analyze-hand ()
    ( format t ">>> Testing: analyze-hand~%" )
    ( setf hand '( ( 5 . spade) ( 3 . diamond ) ( 4 . spade ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )
    ( setf hand '( ( 5 . club) ( 9 . club ) ( 4 . club ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )
    ( setf hand '( ( queen . heart ) ( ace . heart ) ( king . heart ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )
    nil
)

; Function: analyze-game
; Function that sets the *game-state*
( defun analyze-game ()
    (setf *game-state* (list (analyze-hand *hand1* ) (analyze-hand *hand2*) ) )
)

; Function: demo--analyze-game
; Function that tests the analyze-game function
( defun demo--analyze-game ()
    ( format t ">>> Testing: analyze-game~%" )
    ; a couple of busts
    ( format t "Game 1 ... ~%" )
    ( setf *hand1* '( ( 2 . diamond ) ( 4 . diamond ) ( jack . heart ) ) )
    ( setf *hand2* '( ( 10 . spade ) ( king . heart ) ( queen . heart ) ) )
    ( analyze-game )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    ( format t "*game-state* = ~A~%" *game-state* )
    ; an ordinary flush and a straight flush
    ( format t "Game 2 ... ~%" )
    ( setf *hand1* '( ( 10 . diamond ) ( jack . diamond ) ( 2 . diamond ) ) )
    ( setf *hand2* '( ( 3 . spade ) ( 5 . spade ) ( 4 . spade ) ) )
    ( analyze-game )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    ( format t "*game-state* = ~A~%" *game-state* )
    nil
)

; Function: report-the-result
; Function that checks *game-state* for a one of 7 states
    ; 1. bust / bust
    ; 2. flush / bust
    ; 3. bust / flush
    ; 4. straight flush / flush
    ; 5. flush / straight flush
    ; 6. high flush / flush
    ; 7. flush / high flush
; this is used to determine the winner (or lack thereof) of three card flush
( defun report-the-result ()
    ( cond
        ( ( equal *game-state* '( bust bust ) )
            ( increment '*draw-count* )
            ( format t "--> The game is a draw. The deck is dead.~%")
        )
        ( ( and ( not ( equal ( first *game-state* ) 'bust ) )
            ( equal ( second *game-state* ) 'bust )
            )
            ( increment '*win1-count* )
            ( format t "--> Player 1 wins with ~A~%" ( first *game-state* ) )
        )
        ( ( and ( equal ( first *game-state* ) 'bust )
        ( not ( equal ( second *game-state* ) 'bust ) )
        )
            ( increment '*win2-count* )
            ( format t "--> Player 2 wins with ~A~%" ( second *game-state* ) )
        )
        ( ( and ( straight-p *hand1* ) ( not ( straight-p *hand2* ) ) )
            ( format t "!!! Both players found their way to a flush~%" )
            ( increment '*win1-count* )
            ( increment '*f1f2-count* )
            ( format t "--> Player 1 wins with ~A~%" ( first *game-state* ) )
        )
        ( ( and ( not ( straight-p *hand1* ) ) ( straight-p *hand2* ) )
            ( format t "!!! Both players found their way to a flush~%" )
            ( increment '*win2-count* )
            ( increment '*f1f2-count* )
            ( format t "--> Player 2 wins with ~A~%" ( second *game-state* ) )
        )
        ( ( card-greater ( high-card *hand1* ) ( high-card *hand2* ) )
            ( format t "!!! Both players found their way to a flush~%" )
            ( increment '*win1-count* )
            ( increment '*f1f2-count* )
            ( format t "--> Player 1 wins with ~A~%" ( first *game-state* ) )
        )
        ( ( card-greater ( high-card *hand2* ) ( high-card *hand1* ) )
            ( format t "!!! Both players found their way to a flush~%" )
            ( increment '*win2-count* )
            ( increment '*f1f2-count* )
            ( format t "--> Player 2 wins with ~A~%" ( second *game-state* ) )
        )
    )
    nil
)

; Function: card-greater
; Function that checks if a card's rank is greater than another
; If the first card is greater, return true; else false
( defun card-greater ( c1 c2 )
    ( setf rank-order '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf p1 ( position ( car c1 ) rank-order ) )
    ( setf p2 ( position ( car c2 ) rank-order ) )
    
    ( > p1 p2 )
)

; Function: demo--report-the-result
; Tests report-the-result function
( defun demo--report-the-result ()
    ; Test Case: bust - bust
    ( format t ">>> Testing: report-the-result - bust / bust~%" )
    ( setf *hand1* '( ( 6 . spade) ( JACK . diamond ) ( 2 . heart ) ) )
    ( setf *hand2* '( ( 3 . spade ) ( QUEEN . diamond) ( 2 . spade ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case: flush - bust
    ( format t ">>> Testing: report-the-result - flush / bust~%" )
    ( setf *hand1* '( ( 7 . spade) ( QUEEN . spade ) ( 3 . spade ) ) )
    ( setf *hand2* '( ( 9 . spade ) ( JACK . diamond) ( 8 . spade ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case: bust - flush
    ( format t ">>> Testing: report-the-result - bust / flush~%" )
    ( setf *hand1* '( ( 7 . diamond) ( QUEEN . spade ) ( 3 . heart ) ) )
    ( setf *hand2* '( ( 9 . spade ) ( JACK . spade) ( 8 . spade ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case: straight flush - flush
    ( format t ">>> Testing: report-the-result - straight flush / flush~%" )
    ( setf *hand1* '( ( 3 . diamond) ( 4 . diamond ) ( 5 . diamond ) ) )
    ( setf *hand2* '( ( 8 . heart ) ( QUEEN . heart) ( JACK . heart ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case: flush - straight flush
    ( format t ">>> Testing: report-the-result - flush / straight flush~%" )
    ( setf *hand1* '( ( 9 . diamond) ( 10 . diamond ) ( ACE . diamond ) ) )
    ( setf *hand2* '( ( JACK . spade ) ( QUEEN . spade) ( 10 . spade ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case: flush high - flush
    ( format t ">>> Testing: report-the-result - flush high / flush~%" )
    ( setf *hand1* '( ( 2 . club) ( 5 . club ) ( KING . club ) ) )
    ( setf *hand2* '( ( 10 . spade ) ( 6 . spade) ( QUEEN . spade ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    ( format t "-----------------------------------")
    ; Test Case flush - flush high
    ( format t ">>> Testing: report-the-result - flush / flush high~%" )
    ( setf *hand1* '( ( 3 . diamond) ( JACK . diamond ) ( 9 . diamond ) ) )
    ( setf *hand2* '( ( JACK . club ) ( 2 . club) ( ACE . club ) ) )
    ( format t "*hand1* = ~A~%" ( write-to-string *hand1* ) )
    ( format t "*hand2* = ~A~%" *hand2* )
    (analyze-game)
    ( format t "*game-state* = ~A~%" *game-state* )
    (report-the-result)
    nil
)

; Function: hand-rep 
; Function that converts a hand of cards to an abbreviated version
( defun hand-rep ( hand )
    ( setf result ( mapcar #'abbreviate-r hand ) )
    result
)

; Function: abbreviate-r
; Helper function of hand-rep that converts a card to its abbreviated version
( defun abbreviate-r ( card )
    ( setf external-ranks '( 2 3 4 5 6 7 8 9 X J Q K A) )
    ( setf internal-ranks '( 2 3 4 5 6 7 8 9 10 JACK QUEEN KING ACE ) )
    ( setf rank-assoc ( pairlis internal-ranks external-ranks ) )
    
    ( setf external-suits '( H D C S ) )
    ( setf internal-suits '( HEART DIAMOND CLUB SPADE ) )
    ( setf suit-assoc ( pairlis internal-suits external-suits) )
    
    ( setf new-rank ( cdr ( assoc ( car card ) rank-assoc ) ) )
    ( setf suit ( cdr ( assoc ( cdr card ) suit-assoc ) ) )
    ( cons new-rank suit )
        
    
)

; Function: demo--hand-rep
; Tests hand-rep function
( defun demo--hand-rep ( &aux hand )
    ( establish-shuffled-deck )
    ( setf internal ( list ( pop *deck* ) ( pop *deck* ) ( pop *deck* ) ) )
    ( setf external ( hand-rep internal ) )
    ( format t "~A --> ~A~%" internal external )
    nil
)

; Function: play-game
; Function that plays a game of three card flush
( defun play-game ()
    ( increment '*game-count* )
    ( deal-hands )
    ( make-moves )
    ( report-the-result )
)

; Function: make-moves
; Players take turns, hands get analyzed until a win or draw occurs
( defun make-moves ()
    ( increment '*turn-count* )
    ( format t "~A      ~A~%" ( hand-rep *hand1* ) ( hand-rep *hand2* ) )
    ( if ( not ( game-over-p ) )
        ( let ()
            ( players-each-take-a-turn )
            ( make-moves )
        )
    )
    nil
)

; Function: game-over-p
; Function that checks if a win or null deck has occurred
( defun game-over-p ()
    ( analyze-game )
    ( or 
        ( not ( equal *game-state* '( bust bust ) ) )
        ( null *deck* )
    )
)

; Function: demo--play-game
; Tests play-game function
( defun demo--play-game ()
    ( format t ">>> Testing: play-game~%" )
    ( play-game )
)

; Counter initialization -- initialize once so the game can be played regardless of
; whether or not statistics are being computed

( defun init-counters ()
    ( setf *win1-count* 0 )
    ( setf *win2-count* 0 )
    ( setf *draw-count* 0 )
    ( setf *turn-count* 0 )
    ( setf *f1f2-count* 0 )
    ( setf *game-count* 0 )
    nil
)

( init-counters )

; Flexible counter incrementation

( defun increment (name)
    ( set name ( + ( eval name ) 1 ) )
    nil
)

; The main statistics computation program

( defun compute-statistics ( n )
    ( init-counters )
    ( play-game-n-times n )
    ( format t "*game-count* = ~A~%" *game-count* )
    ( format t "*turn-count* = ~A~%" *turn-count* )
    ( format t "*win1-count* = ~A~%" *win1-count* )
    ( format t "*win2-count* = ~A~%" *win2-count* )
    ( format t "*draw-count* = ~A~%" *draw-count* )
    ( format t "*f1f2-count* = ~A~%" *f1f2-count* )
)

; Program to play the game n times

( defun play-game-n-times ( n )
    (cond 
        ( ( > n 0 )
            ( play-game )
            ( play-game-n-times ( - n 1 ) )
        )
    )
)

