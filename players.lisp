;A Poker Texas Hold`Em engine in Common Lisp

;Copyright: Paulo Garcia (paulo.g@chula.ac.th)
;International School of Engineering, 
;Faculty of Engineering,
;Chulalongkorn University, Thailand

;Licensed under CC -BY-NC-SA 4.0
;(Creative Commons Attribution-NonCommercial-ShareAlike)

;format for your AI player
;All your code goes into this file 

; 6638110521 Reuben (Bi Nit)
; PLEASE IGNORE PREVIOUS SUBMISSION & JUDGE THIS SUBMISSION
; I had to fix a bug where the cache was only being cleared on the preflop
; basically moved the (clrhash hmm-cache) to below the preflop conditional that's it 

(let
	(
		; this is our 2-card starting hand
		; format is like (list 4 `H) for the 4 of hearts
		; the number is 1-13 (ace to king)
		; the suit is (`H `D `S `C)
		(hand nil)
		
		; this hash table holds all the opponent models (pi, A, B) for the HMM
		(opponent-models (make-hash-table))
		
		; idk might use this later but rn not much use
		(opponent-history (make-hash-table))
		
		; this keeps track of what everyone actually did (fold, call, raise)
		(observation-log (make-hash-table))
		
		; cache for HMM stuff so we don't recalculate the same numbers constantly
		; this is just to make things faster
		(hmm-cache (make-hash-table))
		
		; just a counter for which hand we're on
		(current-hand-id 0)
	)
	
	; the game engine calls this to give us our two cards
	(defun AI-set-hand (cards)
		(setf hand cards)
		; gotta clear that cache for the new hand
		; important so we don't use old hand data
		(clrhash hmm-cache)
		; bump the hand counter up
		(incf current-hand-id)
	)
	
	; returns our hand when the engine asks for it
	(defun AI-get-hand ()
		hand
	)

	; this function sets up the hidden markov models for the other players
	(defun initialize-hmm-models ()
		(let* (
			; these are the three things we're trying to guess about them
			(states '(strong draw bluff))
			; these are the three things we can actually see them do
			; we get this from the game state ez
			(observations '(fold call raise))
			
			; initial guess: P(Strong), P(Draw), P(Bluff) at the start
			; we're assuming they're mostly drawing or bluffing at the start
			(pi-vec (list 0.15 0.45 0.40)) 
			
			; the transition matrix (A)
			; how likely is their state to change from street to street?
			(matrix-A 
				(list
					; if they were strong: 80% stay strong, 10% draw, 10% bluff
					(list 0.8 0.1 0.1) ; was strong
					; if they were drawing: 30% strong, 50% draw, 20% bluff
					(list 0.3 0.5 0.2) ; was draw
					; if they were bluffing: 10% strong, 20% draw, 70% bluff
					(list 0.1 0.2 0.7) ; was bluff
				)
			)
			
			; the emission matrix (B) for noob1 & noob3
			; if they are in state X, what are they likely to do?
			; this is based on their super simple code (they never raise)
			(matrix-B-noob1
			; P(Observation): Fold Call Raise
				(list
					(list 0.1 0.9 0.0)   ; if strong (mostly call, never raise)
					(list 0.5 0.5 0.0)   ; if drawing (50/50 fold/call)
					(list 0.8 0.2 0.0)   ; if bluffing (mostly fold, they gave up)
				)
			)
			
			; emission matrix (B) for noob2
			; noob2 is a calling machine - they never fold or think
			; this is mostly junk now 'cause the logic is better in the main AI
			(matrix-B-noob2
			; P(Observation): Fold Call Raise
				(list
					(list 0.0 0.7 0.3) ; if strong (0% fold, 70% call, 30% raise)
					(list 0.0 0.7 0.3) ; if drawing (same)
					(list 0.0 0.7 0.3) ; if bluffing (same)

				)
			)
			)
			
			; stuff the models into the opponent-models hash table
			; the key is their player name, like 'noob1
			(setf (gethash 'noob1 opponent-models) 
				(list :states states :obs observations :pi pi-vec :A matrix-A :B matrix-B-noob1))
				
			(setf (gethash 'noob2 opponent-models) 
				(list :states states :obs observations :pi pi-vec :A matrix-A :B matrix-B-noob2))
				
			(setf (gethash 'noob3 opponent-models) 
				(list :states states :obs observations :pi pi-vec :A matrix-A :B matrix-B-noob1))
				
			(format t "HMM Models Initialized.~%")
		)
	)

	; this little function takes the game's action ('fold, 'in, or a number)
	; and translates it into our simple observations: 'fold, 'call, or 'raise
	(defun translate-action (player-list)
		(let ((action (nth 3 player-list))) ; the action is always the 4th item
			(cond
				((equal action 'fold) 'fold)
				((equal action 'raise) 'raise)
				; 'in' or any number counts as a call
				(t 'call)
			)
		)
	)

	; this runs every turn to see what the opponents did and log it
	(defun update-observation-log (game-state cards-on-table)
  		(let ((current-street
					; first, figure out where we are in the hand
					(cond
					((null cards-on-table) 'pre-flop)
					((= (length cards-on-table) 3) 'flop)
					((= (length cards-on-table) 4) 'turn)
					((= (length cards-on-table) 5) 'river)
					(t 'unknown))
				)
			)
			
			; if it's a new hand (pre-flop)
			(when (equal current-street 'pre-flop)
				; clear the action log to start fresh
				(clrhash observation-log)
				
			)
			; clear the HMM cache too, new hand means new predictions
			(clrhash hmm-cache)
		
			; look at every player
			(dolist (player game-state)
				(let ((player-name (nth 0 player)))
					; we only care about the noobs
					(when (member player-name '(noob1 noob2 noob3)) 
						(let ((current-obs (translate-action player)))
							; grab their history, or start a new one
							(let ((log (gethash player-name observation-log (list))))
								; add the new action for this street, like a key-value pair
								; log looks like (flop call turn raise)
								(setf (getf log current-street) current-obs)
								; save it back in the table
								(setf (gethash player-name observation-log) log)
							)
						)
					)
				)
			)
		)
	)

	; gets the list of actions for a player in the right order (pre-flop first)
	; ignore that i used log as my variable lol i didn't know it was a reserved keyword
	; however lisp doesn't raise an error so we cool
	(defun get-observation-sequence (player-name)
		(let ((log (gethash player-name observation-log)))
			(if (not log)
				nil ; no data yet, oh well
				; otherwise, build the sequence
				(remove nil (list
							(getf log 'pre-flop)
							(getf log 'flop)
							(getf log 'turn)
							(getf log 'river)
							)
				)
			)
		)
	)

	; turns the observation ('fold) into an index number (0)
	(defun obs-to-index (obs)
		(case obs
			('fold 0)
			('call 1)
			('raise 2)
			(t nil) ; oops, something went wrong
		)
	)

	; the forward algorithm
	; this is the math part that calculates the chance the opponent is in each hidden state
	(defun run-forward-algorithm (player-name)
  
		(let* ((model (gethash player-name opponent-models))
				(obs-seq (get-observation-sequence player-name)))
			
			; if we don't have a model or actions, we can't do anything
			(if (or (not model) (not obs-seq))
				(return-from run-forward-algorithm nil))
				
			(let* (
				; grab all the necessary matrices and vectors
				(pi-vec (getf model :pi)) ; starting probability
				(matrix-A (getf model :A)) ; transition matrix
				(matrix-B (getf model :B)) ; emission matrix
				(num-states (length (getf model :states))) ; should be 3 (strong, draw, bluff)
				
				; this will hold the current probabilities (alpha)
				(alpha (make-list num-states :initial-element 0.0))
				)
			
			; 1. first step: initialization 
			; just for the first action (pre-flop)
			(let* ((o1 (obs-to-index (first obs-seq))))
				(dotimes (i num-states) ; loop through each state i
				(setf (nth i alpha)
						(* (nth i pi-vec) ; P(state_i)
						(nth o1 (nth i matrix-B)))) ; P(o1 | state_i)
				))
				
			; 2. the loop: recursion 
			; run through the rest of the actions (flop, turn, river)
			(dolist (obs (rest obs-seq))
				(let* ((ot (obs-to-index obs))
					(alpha-prev alpha) ; the old probabilities
					(alpha-next (make-list num-states :initial-element 0.0))) ; the new ones
					
				(dotimes (j num-states) ; for the 'to' state j
					(let ((sum 0.0))
					(dotimes (i num-states) ; for the 'from' state i
						(incf sum (* (nth i alpha-prev) ; old alpha
									(nth j (nth i matrix-A))))) ; transition likelihood
					
					; calculate the new probability for state j
					(setf (nth j alpha-next)
							(* sum (nth ot (nth j matrix-B)))))) ; times the chance of seeing the action
				
				; update the current probabilities for the next round
				(setf alpha alpha-next)))
				
			; this is the final probability vector
			alpha)
		)
	)

	; this is the smart version that uses the cache
	; it runs the forward algorithm and cleans up the numbers (normalization)
	(defun get-state-probabilities (player-name)
		; first, check the cache
		(let ((cached (gethash player-name hmm-cache)))
			(when cached
				; if it's there, done!
				(return-from get-state-probabilities cached)))
		
		; calculate it if it's not cached
		(let ((alpha-final (run-forward-algorithm player-name)))
			(let ((result
					; if the HMM failed, just guess 1/3 for each
					(if (not alpha-final)
						(list 0.33 0.33 0.33)
						; otherwise, normalize the vector so the numbers add up to 1
						(let ((total (reduce #'+ alpha-final)))
							(if (or (null total) (<= total 0.0))
								; avoid dividing by zero if the math messed up
								(list 0.33 0.33 0.33)
								; divide by the total
								(list
									(/ (nth 0 alpha-final) total) ; P(Strong)
									(/ (nth 1 alpha-final) total) ; P(Draw)
									(/ (nth 2 alpha-final) total) ; P(Bluff)
								))))))
				; save the result for next time
				(setf (gethash player-name hmm-cache) result)
				result))
	)

	; checks if our hand has a pair
	; also checks to make sure the hand actually exists and has 2 cards
	(defun is-pair (hand)
		(and hand
			 (>= (length hand) 2)
			 (equal (first (first hand)) (first (second hand)))))

	; checks if our hand is suited
	; also checks to make sure the hand actually exists and has 2 cards
	(defun is-suited (hand)
		(and hand
			 (>= (length hand) 2)
			 (equal (second (first hand)) (second (second hand)))))

	; figures out how good our starting hand is (pre-flop)
	; it uses a 4-tier system
	(defun get-hand-tier (hand)
		; if we don't have a hand, it's trash
		(when (not hand)
			(return-from get-hand-tier 'T4-Trash))
		(let*
			(
				; get the card ranks
				(rank1 (first (first hand)))
				(rank2 (first (second hand)))
				; aces are rank 1, so change them to 14 so they score higher
				(r1 (if (= rank1 1) 14 rank1))
				(r2 (if (= rank2 1) 14 rank2))
				; find the high card and low card
				(high (max r1 r2))
				(low (min r1 r2))
				; calculates the gap between ranks
				(gap (- high low))
				(suited (is-suited hand))
				(paired (is-pair hand))
			)
			(cond
				; T1-Premium: the absolute best hands (AA-JJ, AK, AQ)
				((or
					(and paired (>= low 11)) ; big pairs
					(and (= high 14) (>= low 12)) ; big ace hands
				) 'T1-Premium)
				
				; T2-Strong: really good hands (TT-77, AJ, AT, KQ, KJ)
				((or
					(and paired (>= low 7)) ; medium pairs
					(and (= high 14) (>= low 10)) ; medium ace hands
					(and (= high 13) (>= low 11)) ; king/queen hands
				) 'T2-Strong)
				
				; T3-Playable: speculative hands (small pairs, suited aces, suited connectors)
				((or
					paired ; any other pair
					(and suited (= high 14)) ; any suited ace
					(and suited (<= gap 2) (>= low 7)) ; good suited connectors
				) 'T3-Playable)
				
				; T4-Trash: everything else (e.g., 7-2 offsuit)
				(t 'T4-Trash)
			)
		)
	)

	; counts how many cards of a certain suit are in a list
	; this is the safe version that checks for nil inputs
	(defun safe-count-suit (cards suit)
		(if (not cards)
			0
			(count-if 
				(lambda (card) 
					; make sure the card is real and has a suit
					(and card 
						 (>= (length card) 2)
						 (equal (second card) suit)))
				cards))
	)

	; gives a list of how many of each rank we have (aces are 14)
	; safe version that checks for nil inputs
	(defun safe-get-rank-counts (cards)
		(let ((rank-counts (make-list 15 :initial-element 0))) ; list of 15 zeros
			(when cards
				(dolist (card cards)
					; check the card is real and has a rank
					(when (and card (>= (length card) 1))
						(let* ((rank (first card)) 
							   (r (if (= rank 1) 14 rank))) ; convert ace to 14
							; make sure it's a valid rank (2 through 14)
							(when (and (numberp r) (>= r 2) (<= r 14))
								(incf (nth r rank-counts)))))))
			rank-counts
		)
	)

	; checks if we have 5 or more of the same suit (a flush)
	(defun has-flush (cards)
		(when (and cards (>= (length cards) 5))
			(or (>= (safe-count-suit cards 'H) 5)
				(>= (safe-count-suit cards 'D) 5)
				(>= (safe-count-suit cards 'S) 5)
				(>= (safe-count-suit cards 'C) 5)))
	)

	; checks if we have exactly 4 of the same suit (a flush draw)
	(defun has-flush-draw (cards)
		(when (and cards (>= (length cards) 4))
			(or (= 4 (safe-count-suit cards 'H))
				(= 4 (safe-count-suit cards 'D))
				(= 4 (safe-count-suit cards 'S))
				(= 4 (safe-count-suit cards 'C))))
	)

	; checks if we have 5 cards in a row (a straight)
	(defun has-straight (rank-counts)
		(when rank-counts
			(let ((unique-ranks (loop for i from 2 to 14 ; get all our ranks
									 when (> (nth i rank-counts) 0) 
									 collect i)))
				; check for the special ace-low straight (A-2-3-4-5)
				(when (and (find 14 unique-ranks) ; 14 = ace
						   (find 2 unique-ranks)
						   (find 3 unique-ranks)
						   (find 4 unique-ranks)
						   (find 5 unique-ranks))
					(return-from has-straight t))
				; check for all other 5-card straights
				(loop for high-card from 14 downto 6 do
					(let ((has-all-five t))
						(loop for i from 0 to 4 do
							(unless (find (- high-card i) unique-ranks)
								(setf has-all-five nil)))
						(when has-all-five
							(return-from has-straight t))))
				nil))
	)

	; checks if we have 4 cards in a row (a straight draw)
	(defun has-straight-draw (rank-counts)
		(when rank-counts
			(let ((unique-ranks (loop for i from 2 to 14 ; get all our ranks
									 when (> (nth i rank-counts) 0) 
									 collect i)))
				; check for the ace-low draw (A-2-3-4)
				(when (and (find 14 unique-ranks) ; 14 = ace
						(find 2 unique-ranks)
						(find 3 unique-ranks)
						(find 4 unique-ranks))
					(return-from has-straight-draw t))
				; check for all other 4-card draws
				(loop for high-card from 14 downto 5 do
					(let ((cards-in-sequence 0))
						(loop for i from 0 to 4 do ; check a 5-card window
							(when (find (- high-card i) unique-ranks)
								(incf cards-in-sequence)))
						(when (>= cards-in-sequence 4) ; need 4 for a draw
							(return-from has-straight-draw t))))
				nil))
	)

	; checks if our pocket pair is higher than the highest card on the board
	(defun has-overpair (hand cards-on-table)
		(when (and hand 
				   cards-on-table 
				   (is-pair hand) ; we need a pair in our hand
				   (>= (length cards-on-table) 3)) ; must be after the flop
			(let* ((our-rank (first (first hand)))
				   (our-r (if (= our-rank 1) 14 our-rank)))
				; get all the board card ranks
				(let ((board-ranks (remove-if #'null
										(mapcar (lambda (c) 
												  (when (and c (>= (length c) 1))
													(let ((r (first c)))
													  (if (= r 1) 14 r))))
												cards-on-table))))
					(when (and board-ranks (> (length board-ranks) 0))
						; find the biggest card on the board
						(let ((board-high (apply #'max board-ranks)))
							; check if our rank is bigger
							(and (numberp board-high) (> our-r board-high)))))))
	)

	; checks if one of our cards makes a pair with the highest card on the board
	(defun get-top-pair (hand cards-on-table)
		(when (and hand cards-on-table (>= (length cards-on-table) 3))
			; get all the board card ranks
			(let* ((board-ranks (remove-if #'null
									(mapcar (lambda (c)
											  (when (and c (>= (length c) 1))
												(let ((r (first c)))
												  (if (= r 1) 14 r))))
											cards-on-table))))
				(when (and board-ranks (> (length board-ranks) 0))
					; find the biggest card on the board
					(let ((board-high (apply #'max board-ranks)))
						; see if either of our cards matches it
						(or (and hand 
								 (= (if (= (first (first hand)) 1) 14 (first (first hand))) 
									board-high))
							(and hand
								 (>= (length hand) 2)
								 (= (if (= (first (second hand)) 1) 14 (first (second hand)))
									board-high)))))))
	)

	; figures out how strong our 5-7 card hand is *after* the flop
	; this one uses 6 tiers
	(defun get-post-flop-tier (hand cards-on-table)
		(when (or (not hand) (not cards-on-table))
			(return-from get-post-flop-tier 'T6-Air))
		(let* (
				(all-cards (append hand cards-on-table))
				(rank-counts (safe-get-rank-counts all-cards))
				
				; check for hands that are already made
				(has-4oak (find 4 rank-counts)) ; four of a kind
				(has-3oak (find 3 rank-counts)) ; three of a kind
				(num-pairs (count 2 rank-counts))
				(has-flush-made (has-flush all-cards))
				(has-straight-made (has-straight rank-counts))
				
				; check for hands that are close
				(has-flush-d (has-flush-draw all-cards))
				(has-straight-d (has-straight-draw rank-counts))
				
				; check the quality of our pair
				(has-over (has-overpair hand cards-on-table))
				(has-top-pair (get-top-pair hand cards-on-table))
				)
			
			(cond
			; T1-Nuts: basically unbeatable (quads, full house, flush, straight)
			(has-4oak 'T1-Nuts)
			((and has-3oak (>= num-pairs 1)) 'T1-Nuts) ; full house
			(has-flush-made 'T1-Nuts)
			(has-straight-made 'T1-Nuts)
			
			; T2-VeryStrong: super good hands (trips, two pair, overpair)
			(has-3oak 'T2-VeryStrong) ; trips
			((>= num-pairs 2) 'T2-VeryStrong) ; two pair
			(has-over 'T2-VeryStrong) ; overpair
			
			; T3-Strong: good hands (top pair)
			(has-top-pair 'T3-Strong)
			
			; T4-Decent: okay hands (any pair, big combo draws)
			((= num-pairs 1) 'T4-Decent) ; any other pair
			((and has-flush-d has-straight-d) 'T4-Decent) ; draw for both
			
			; T5-Weak: single draws (flush draw, straight draw)
			(has-flush-d 'T5-Weak)
			(has-straight-d 'T5-Weak)
			
			; T6-Air: nothing at all
			(t 'T6-Air)
			))
	)

	; counts how many players haven't folded yet
	(defun count-active-players (game-state)
		(when game-state
			(count-if (lambda (p) 
						(and p (not (equal (nth 3 p) 'fold)))) 
					  game-state))
	)

	; calculates the pot odds (how much we pay vs how much we can win)
	(defun calculate-pot-odds (cost pot)
		(if (or (not (numberp cost)) 
				(not (numberp pot))
				(<= cost 0)
				(<= pot 0))
			1.0 ; if the data is junk, just return a bad number
			(/ (float cost) (float (+ pot cost))))
	)


	; this is the main logic function, where the AI makes its choice
	(defun AI (cards-on-table game-state)
		; 1. first checks 
		
		; if the game state is messed up, just fold
		(when (or (not game-state) (zerop (length game-state)))
			(return-from AI 'fold))
		
		; initialize the models if it's the first time
		(when (zerop (hash-table-count opponent-models))
			(initialize-hmm-models))
		
		; log what everyone did this turn
		(update-observation-log game-state cards-on-table) 

		; 2. define a bunch of variables 
		(let*
			(
				; find our own state (chips, current bet, etc.)
				(state (find-if 
							(lambda (x) (and x (equal (nth 0 x) 'AI)))
							game-state))
				
				; pre-flop hand strength
				(hand-tier (get-hand-tier hand))
				
				; betting stuff
				(max-bet (let ((bets (remove-if #'null (mapcar #'third game-state))))
							(if bets (apply #'max bets) 0)))
				(my-bet (if state (nth 2 state) 0))
				(cost-to-call (max 0 (- max-bet my-bet)))
				(my-chips (if state (max 0 (nth 1 state)) 0))
				
				; calculate possible raise amounts
				; always capped by how many chips we have left
				(chips-after-call (max 0 (- my-chips cost-to-call)))
				(small-raise (min 60 chips-after-call))
				(med-raise (min 100 chips-after-call))
				(big-raise (min 150 chips-after-call))
				(huge-raise (min 800 chips-after-call)) ; basically all-in
				
				; post-flop hand strength
				(post-flop-tier (when cards-on-table
									(get-post-flop-tier hand cards-on-table)))
				
				; opponent stuff
				; who made the biggest bet?
				(aggressor
					(when (> cost-to-call 0)
						(let ((agg-player (find max-bet game-state :key #'third)))
							(when agg-player (nth 0 agg-player)))))
				
				; pot odds stuff
				; the total money in the pot
				(pot-size (let ((bets (remove-if #'null (mapcar #'third game-state))))
							(if bets (reduce #'+ bets) 0)))
				(pot-odds (calculate-pot-odds cost-to-call pot-size))
			)

			; safety check: if we don't have enough chips to call, we have to fold
			(when (and (> cost-to-call my-chips) (> cost-to-call 0))
				(return-from AI 'fold))

			; 3. main decision tree
			(cond
				; branch 1: pre-flop decisions
				((null cards-on-table)
					(cond
						; T1-Premium: raise big, these are monsters
						((equal hand-tier 'T1-Premium) 
							(if (> big-raise 0) big-raise 'in))
						; T2-Strong: raise medium, or fold if the bet is too crazy
						((equal hand-tier 'T2-Strong)
							(if (<= cost-to-call 100)
								; if no one has bet, we'll open-raise medium
								(if (and (= cost-to-call 0) (> med-raise 0))
									med-raise 'in)
								'fold))
						; T3-Playable: only call small bets
						((equal hand-tier 'T3-Playable)
							(if (<= cost-to-call 60) 'in 'fold))
						; T4-Trash: fold, unless it's free (just check)
						(t (if (= cost-to-call 0) 'in 'fold))))

				; branch 2: post-flop vs noob2
				; this is a special strategy because noob2 never folds
				; we just bet for pure value, no HMM needed
				((equal aggressor 'noob2)
					(cond
						; T1/T2: we have the goods. raise huge to get max money.
						((equal post-flop-tier 'T1-Nuts) 
							(if (> huge-raise 0) huge-raise 
								(if (> big-raise 0) big-raise 'in)))
						((equal post-flop-tier 'T2-VeryStrong)
							(if (> big-raise 0) big-raise
								(if (> med-raise 0) med-raise 'in)))
						; T3: strong hand. raise/call big bet.
						((equal post-flop-tier 'T3-Strong)
							(cond
								((and (= cost-to-call 0) (> med-raise 0)) med-raise)
								((<= cost-to-call 100) 'in)
								(t 'fold)))
						; T4: decent hand. call medium bets.
						((equal post-flop-tier 'T4-Decent)
							(cond
								((= cost-to-call 0) (if (> small-raise 0) small-raise 'in))
								((<= cost-to-call 80) 'in)
								(t 'fold)))
						; T5: weak draw. only call if it's super cheap.
						((equal post-flop-tier 'T5-Weak)
							(if (<= cost-to-call 50) 'in 'fold))
						; T6: air. can't bluff noob2. fold unless it's a free check.
						(t (if (= cost-to-call 0) 'in 'fold))))

				; branch 3: post-flop vs noob1/noob3
				; this is where we use the HMM results
				((and aggressor (member aggressor '(noob1 noob3)))
					; get the opponent's probabilities
					(let* ((probs (get-state-probabilities aggressor))
						   (p-strong (nth 0 probs))
						   (p-draw (nth 1 probs))
						   (p-bluff (nth 2 probs)))
						
						(cond
							; case a: HMM thinks they are super strong (>60%)
							((> p-strong 0.6)
								(cond
									; only play our best hands (T1, T2)
									((member post-flop-tier '(T1-Nuts T2-VeryStrong))
										(if (> big-raise 0) big-raise 'in))
									; call with top pair (T3) if it's cheap
									((equal post-flop-tier 'T3-Strong)
										(if (<= cost-to-call 60) 'in 'fold))
									; fold everything else, they probably have it
									(t 'fold)))
							
							; case b: HMM thinks they are bluffing (>45%)
							((> p-bluff 0.45)
								(cond
									; raise with our strong hands (T1, T2, T3) to punish their bluff
									((member post-flop-tier '(T1-Nuts T2-VeryStrong T3-Strong))
										(if (> big-raise 0) big-raise 'in))
									; call with decent hands (T4)
									((equal post-flop-tier 'T4-Decent)
										(if (<= cost-to-call 80) 'in 'fold))
									; try a 'hero call' with air (T5, T6) if the price is low
									(t (if (<= cost-to-call 40) 'in 'fold))))
							
							; case c: HMM is undecided (standard/cautious plan)
							(t
								(cond
									; raise with monsters
									((member post-flop-tier '(T1-Nuts T2-VeryStrong))
										(if (> big-raise 0) big-raise 'in))
									; call with top pair
									((equal post-flop-tier 'T3-Strong)
										(if (<= cost-to-call 80) 'in 'fold))
									; call with decent pair if cheap
									((equal post-flop-tier 'T4-Decent)
										(if (<= cost-to-call 50) 'in 'fold))
									; fold draws and air
									(t 'fold))))))

				; branch 4: post-flop when no one has bet yet (or we are first)
				(t
					; check which noobs are still in the pot
					(let* ((noob1-in (find 'noob1 game-state 
											:test (lambda (n p) 
													(and (equal (first p) n)
														 (not (equal (nth 3 p) 'fold))))))
						   (noob2-in (find 'noob2 game-state 
											:test (lambda (n p) 
													(and (equal (first p) n)
														 (not (equal (nth 3 p) 'fold))))))
						   (noob3-in (find 'noob3 game-state 
											:test (lambda (n p) 
													(and (equal (first p) n)
														 (not (equal (nth 3 p) 'fold)))))))
						
						(cond
							; case a: noob2 is in.
							; bet aggressively for value, he'll call with anything
							(noob2-in
								(cond
									((equal post-flop-tier 'T1-Nuts)
										(if (> huge-raise 0) huge-raise 'in))
									((equal post-flop-tier 'T2-VeryStrong)
										(if (> big-raise 0) big-raise 'in))
									((equal post-flop-tier 'T3-Strong)
										(if (> med-raise 0) med-raise 'in))
									((equal post-flop-tier 'T4-Decent)
										(if (> small-raise 0) small-raise 'in))
									; just check our draws and air, no need to bluff this guy
									(t 'in)))
							
							; case b: noob1 or noob3 is in (and noob2 is out).
							; use HMM to decide if a bluff will work.
							((or noob1-in noob3-in)
								; target the one who is still in
								(let* ((target (if noob1-in 'noob1 'noob3))
									   (probs (get-state-probabilities target))
									   (p-bluff (nth 2 probs))) ; this is P(Their_Hidden_State = Bluff)
									(cond
										; if HMM thinks they're weak/bluffable (>40%)
										; we can bet to push them off
										((> p-bluff 0.4)
											(cond
												; value bet strong hands
												((member post-flop-tier '(T1-Nuts T2-VeryStrong))
													(if (> big-raise 0) big-raise 'in))
												((member post-flop-tier '(T3-Strong T4-Decent))
													(if (> med-raise 0) med-raise 'in))
												; KEY: bluff with our draws and air
												((equal post-flop-tier 'T5-Weak)
													(if (> small-raise 0) small-raise 'in))
												(t (if (> small-raise 0) small-raise 'in))))
										
										; if HMM thinks they're strong, don't bluff
										; only bet if we have value
										(t
											(cond
												; value bet monsters
												((member post-flop-tier '(T1-Nuts T2-VeryStrong))
													(if (> big-raise 0) big-raise 'in))
												; value bet top pair
												((equal post-flop-tier 'T3-Strong)
													(if (> med-raise 0) med-raise 'in))
												; check everything else
												(t 'in))))))
							
							; case c: everyone else folded
							; (just a cleanup case)
							(t
								(cond
									((member post-flop-tier '(T1-Nuts T2-VeryStrong))
										(if (> big-raise 0) big-raise 'in))
									((equal post-flop-tier 'T3-Strong)
										(if (> med-raise 0) med-raise 'in))
									((equal post-flop-tier 'T4-Decent)
										(if (> small-raise 0) small-raise 'in))
									(t 'in)))))))
		)
	)
)

(defun last-player-in (game-state)
	(let
		(
			(num-n-folded
				(reduce 
					`+
					(mapcar
						(lambda (x)
							(if (equal (nth 3 x) `fold)
								0
								1
							)
						)
						game-state
					)
				)
			)
		)
		(if (equal num-n-folded 1)
			t
			nil
		)
	)
)

;simple player, never raises 
(let
	(
		(hand nil)
	)
	(defun noob1-set-hand (cards)
		(setf hand cards)
	)
	(defun noob1-get-hand ()
		hand
	)
	(defun noob1 
		(
			cards-on-table
			game-state	
		)
		;noob1 never raises, either checks/calls or folds
		(if (last-player-in game-state)
			`in
			(if (> (random 10) 7)
				`fold
				`in
			)
		)		
  )
)

;simple player, never folds
(let
	(
		(hand nil)
	)
	(defun noob2-set-hand (cards)
		(setf hand cards)
	)
	(defun noob2-get-hand ()
		hand
	)
	(defun noob2
		(
			cards-on-table
			game-state	
		)
		(let
			(
				;our current state: chips, current bet
				(state 	(find-if 
									(lambda (x) 
										(if (equal (nth 0 x) `noob2)
											t
											nil
										)
									)
									game-state
								)
				)
			)
			;noob2 never folds, 
			;either checks/calls or raises big blind
			;if it has enough money
			(if (> (random 10) 7)
				(if (>= (nth 1 state) 40)
					40
					(nth 1 state)
				)
				`in
			)
		)		
  )
)

;simple player, never raises
(let
	(
		(hand nil)
	)
	(defun noob3-set-hand (cards)
		(setf hand cards)
	)
	(defun noob3-get-hand ()
		hand
	)
	(defun noob3
		(
			cards-on-table
			game-state	
		)
		;noob3 never raises, either checks/calls or folds
		(if (last-player-in game-state)
			`in
			(if (> (random 10) 7)
				`fold
				`in
			)
		)	
  )		
)