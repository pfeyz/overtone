(def instrument (comp smooth-saw midi->hz note))

(def old-nokia-ringtone
  [[1/8 :C4] [1/8 :C4] [1/8 :Eb4] [1/8 :E4]
   [1/8 :F4] [1/8 :rest] [1/8 :Eb4] [1/8 :C4]
   [1/8 :rest] [1/8 :C4] [1/8 :Eb4] [1/8 :F4]
   [1/8 :G4] [1/8 :G4] [1/8 :F4] [1/8 :Eb4]])

(comment
  (play-pattern old-nokia-ringtone instrument (now))

  ;; loop pattern forever
  (play-pattern (cycle old-nokia-ringtone) instrument (now))

  ;; the on-downbeat macro makes it easier to evalute separate expressions and
  ;; keep them in time with each other.
  (on-downbeat
   (play-pattern (cycle old-nokia-ringtone) instrument (now))

   (wait 1/16 ;; start second pattern 1/16 note after the first
         (play-pattern (cycle old-nokia-ringtone) instrument (now))))
  )


(comment
  ;; phasing
  (let [notes [[1/8 :C4] [1/8 :D4] [1/8 :B4] [1/8 :G4]
               [1/8 :rest]
               [1/8 :G5] [1/8 :A5] [1/8 :E6]
               [1/8 :D5] [1/8 :C5] [1/8 :B4]
               [1/8 :rest]]
        stretched-notes (for [[duration note] notes]
                          [(* 1.005 duration) note])]

    (on-downbeat
     (play-pattern (cycle notes) instrument (now))
     (play-pattern (cycle stretched-notes) instrument (now))))

  (on-downbeat
   (play-pattern (concat
                  (take (* 6 32) (cycle [[1/8 :C2]]))
                  (cycle (concat
                          (take (* 6 4) (cycle [[1/8 :A1]]))
                          (take (* 6 4) (cycle [[1/8 :C2]])))))
                 instrument (now)))

  (on-downbeat
      (play-pattern (cycle [[3/16 :C4]]) instrument (now))
      (play-pattern (cycle [[5/16 :D4]]) instrument (now))
      (play-pattern (cycle [[7/16 :E4]]) instrument (now)))
)
