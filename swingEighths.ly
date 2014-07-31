swingEighths = #(define-music-function (parser location duration musak) (number? ly:music?) 
"For each beat, lengthens the notes in the onbeat to `duration` of the beat, and shortens the rest to 1-`duration` of the beat. 

If there is a note that spans both the on and off beats, the whole beat is spared." 

(define getNextNote
'0)

(define (alterDuration musak)
 (define (getAllNotes musak)
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
        (hasElements (append notes (getAllNotes hasElements)))
        (hasElement (append notes (getAllNotes hasElement)))
        ; A singular music object?
        ((equal? (ly:music-property musak 'name) 'NoteEvent) (append notes musak))
        (else '())
       )
     ) 
    )
    ((ly:music-list? musak) (append notes (map getAllNotes musak)))
    (else '())
   )
  )
 )
 (display (getAllNotes musak))
 (let* 
  (
   (oldDuration (ly:music-property (first (ly:music-property musak 'elements)) 'duration))
   (oldFactor (ly:duration-factor oldDuration))
  )
  (set! 
   (ly:music-property (first (ly:music-property musak 'elements)) 'duration)
   (ly:make-duration (ly:duration-log oldDuration) (ly:duration-dot-count oldDuration) (* (car oldFactor) duration) (cdr oldFactor))
  )
 )
 musak
)

(alterDuration musak)

)
