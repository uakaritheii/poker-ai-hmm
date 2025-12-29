# Texas Hold'em Poker AI

All work is in players.lisp.

A poker AI using Hidden Markov Models for opponent modeling. Achieved **60% win rate** over 500 games against 3 baseline opponents (vs. 25% expected in a 4-player game).

```
Results (500 games):
AI: 60%  |  NOOB2: 39.2%  |  NOOB1: 0.40%  |  NOOB3: 0.40%
```

## Approach

### Hidden Markov Model (for NOOB1/NOOB3)

Models opponents with three hidden states (`Strong`, `Drawing`, `Bluffing`) inferred from observable actions (`Fold`, `Call`, `Raise`).

- **Forward Algorithm** computes P(state | observed actions) after each betting round
- When P(Bluffing) > 40%: bet/raise to push them off hands or make hero calls
- When P(Strong) > 60%: play conservatively, fold marginal hands

### Exploitative Strategy (for NOOB2)

NOOB2 never folds. HMM is useless here since their actions don't correlate with hand strength.

Instead: **value bet aggressively** with any decent hand. No bluffing (they'll call anyway).

### Hand Evaluation

| Pre-flop | Example Hands |
|----------|---------------|
| Tier 1 (Premium) | AA, KK, QQ, AKs |
| Tier 2 (Strong) | JJ, TT, AQs, KQs |
| Tier 3 (Playable) | Medium pairs, suited connectors |
| Tier 4 (Fold) | Everything else |

Post-flop evaluates: straight/flush draws, pairs, two-pair, trips, etc.

## Project Structure

```
├── players.lisp    # AI implementation (~600 lines of original code)
├── poker.lisp      # Game engine (provided)
├── main.lisp       # Entry point (provided)
├── eval.py         # Win rate calculator. Run this to verify winrate
└── winrate.txt     # result of running (test-full-AI) 500 times
```

## Usage

```bash
sbcl --load main.lisp
```
```lisp
(test-full-AI)  ; run one game
```

```lisp
; run 500 games
; result is pasted on winrate.txt
(loop for i from 1 to 500
      do (let* ((game-log (with-output-to-string (*standard-output*)
                           (test-full-AI)))
                (search-string "Game finished:"))
           (with-input-from-string (s game-log)
             (loop for line = (read-line s nil nil)
                   while line
                   do (when (and (> (length line) (length search-string))
                                 (string= (subseq line 0 (length search-string)) search-string))
                        (format t "~a~%" line))))))

```

## Course

Artificial Intelligence — Chulalongkorn University (ISE)

**AI implementation in players.lisp is original work.** Game engine by Professor Paulo Garcia (CC-BY-NC-SA 4.0). 
