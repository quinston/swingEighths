swingEighths =
#(define-music-function (parser location duration musak) (real? ly:music?) 
"For each beat, lengthens the notes in the onbeat to `duration` of the beat, and shortens the rest to 1-`duration` of the beat. 

If there is a note that spans both the on and off beats, the whole beat is spared." 

(define (getAllNotesAndRests musak)
  (let
   (
    (notes '())
   )
   (cond 
    ((ly:music? musak)
      (let
       (
        (hasElements (ly:music-property musak 'elements #f))
        (hasElement (ly:music-property musak 'element #f))
       )
       (cond
        (hasElements (append notes (getAllNotesAndRests hasElements)))
        (hasElement (append notes (getAllNotesAndRests hasElement)))
        ; A singular music object?
        ((memv (ly:music-property musak 'name) '(NoteEvent RestEvent)) (append notes musak))
        (else '())
       )
     ) 
    )
    ((ly:music-list? musak) (append notes (map getAllNotesAndRests musak)))
    (else '())
   )
  )
 )
 (define (getNoteDuration note) (define ggg (ly:music-property note 'duration)) (ly:moment-main (ly:duration-length ggg)))
 (define  (getOneHalfBeat takenNotes leftoverNotes nextNoteStartTime runningLength)
#!
  (display "so far:")
  (display runningLength)
  (display "\n")
  (display leftoverNotes)
  (display "\n")
!#
  (cond 
   ((equal? runningLength 1/8) (list takenNotes leftoverNotes nextNoteStartTime))
   ((null? leftoverNotes) (list '() leftoverNotes nextNoteStartTime))
   ((< runningLength 1/8) 
    (let 
     (
      (lastNoteDuration (getNoteDuration (car leftoverNotes)))
     )
;     (display "getonehalfbeat")
     (getOneHalfBeat 
      (append takenNotes (list (car leftoverNotes))) 
      (cdr leftoverNotes) 
      (+ nextNoteStartTime (getNoteDuration (car leftoverNotes)))
      (+ runningLength (getNoteDuration (car leftoverNotes)))
     )
    )
   )
   (else (list '() leftoverNotes nextNoteStartTime))
  )
 )
 ; Drop notes until we are at the next beat. Used after a failure to get a half beat
 (define (skipToNextBeat leftoverNotes nextNoteStartTime)
  (cond 
   ; out of notes
   ((null? leftoverNotes) 
#!
    (display "skip to:")
    (display nextNoteStartTime)
    (display "\n")
!#
    '()
   )
   ; we've completed a beat. we're done.
   ((integer? (/ nextNoteStartTime 1/4)) leftoverNotes)
   ; keep going
   (else 
    (display "skiptonextbeat")
    (skipToNextBeat (cdr leftoverNotes) (+ nextNoteStartTime (getNoteDuration (car leftoverNotes))))
   )
  )
 )

 (define (printHalfBeats leftoverNotes nextNoteStartTime)
  ; Get a half beat.
  (define newLeftoverNotes (getOneHalfBeat  '() leftoverNotes nextNoteStartTime 0))
  (cond
   ; were there no notes to pull from
   ((null? leftoverNotes) '())
   (else
#!
    (display "binkBunk\n")
    (display leftoverNotes)
    (display "\n")
    (display newLeftoverNotes)
    (display "\n")
    (display (car newLeftoverNotes))
    (display "\n" )
    (display (null? (car newLeftoverNotes)))
    (display "\n")
    (display (list? (car newLeftoverNotes)))
    (display "\n")
    (display (length+ (car newLeftoverNotes)))
    (display "\n")
    (display "---------\n")
!#
    (cond 
     ; Couldn't get a half beat
     ((null?  (car newLeftoverNotes))
      (display "bunk\n")
      ; Could not get a half beat. Skip.
      (printHalfBeats (skipToNextBeat (cadr newLeftoverNotes) (caddr newLeftoverNotes)) (ceiling (caddr newLeftoverNotes)))
     )
     (else 
      (display (car newLeftoverNotes))
      (display "boink\n")
      (set! newLeftoverNotes (getOneHalfBeat  '() (cadr newLeftoverNotes) (caddr newLeftoverNotes) 0))
      (display (car newLeftoverNotes))
      (display "boink\n")
      (printHalfBeats (cadr newLeftoverNotes) (caddr newLeftoverNotes))
     )
    )
   )
  )
 )

(define (alterDuration notes multiplier)
 (map 
  (lambda (note)
   (let* 
    (
     (oldDuration (ly:music-property note 'duration))
     (oldFactor (ly:duration-factor oldDuration))
    )
    (set! 
     (ly:music-property note 'duration)
     (ly:make-duration (ly:duration-log oldDuration) (ly:duration-dot-count oldDuration) (* (car oldFactor) multiplier) (cdr oldFactor))
    )
   )
  )
  notes
 )
)

(define (lengthenDownShortenUp leftoverNotes nextNoteStartTime)
  ; Get a half beat.
  (define newLeftoverNotes (getOneHalfBeat  '() leftoverNotes nextNoteStartTime 0))
  (cond
   ; Were there no notes to pull from?
   ((null? leftoverNotes) '())
   (else
    (cond 
     ( (null? (car newLeftoverNotes))
      ; Could not get a half beat. Skip.
      (lengthenDownShortenUp (skipToNextBeat (cadr newLeftoverNotes) (caddr newLeftoverNotes)) (ceiling (caddr newLeftoverNotes)))
     )
     (else 
      (alterDuration (car newLeftoverNotes) (* 2 duration))
      (display "\ndown\n")
      (display (car newLeftoverNotes))
      (set! newLeftoverNotes (getOneHalfBeat  '() (cadr newLeftoverNotes) (caddr newLeftoverNotes) 0))
      (alterDuration (car newLeftoverNotes) (* 2 (- 1 duration)))
      (display "\nup\n")
      (display (car newLeftoverNotes))
      (lengthenDownShortenUp (cadr newLeftoverNotes) (caddr newLeftoverNotes))
     )
    )
   )
  )
 )
(lengthenDownShortenUp (getAllNotesAndRests musak) 0)
musak
)
