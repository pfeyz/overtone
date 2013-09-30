(ns coderdojonyc.lib
    (:gen-class))
(use 'overtone.live)

(defonce metro (metronome 120))

;; We use a saw-wave that we defined in the oscillators tutorial
(definst saw-wave [freq 440 attack 0.01 sustain 0.1 release 0.01 vol 0.8] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(definst smooth-saw [freq 440 attack 0.01 sustain 0.1 release 0.01 vol 0.5]
  (let [amp-env (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
        filter-env (env-gen (lin-env 0 (* sustain 0.5) release) 1 1 0 1)
        signal (saw freq)]
    (lpf (* signal amp-env vol)
         (-> filter-env (* (i-rand 1000 2000)) (+ 300)))))

;; Let's make easier to pass notes
(defn saw-by-note [music-note]
    (saw-wave (midi->hz (note music-note))))

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw-by-note note)))

; this function will play our sound at whatever tempo we've set our metronome to 
(defn looper [nome sound current-beat increment final-beat]
    (let [beat-time (nome current-beat)]
        (at beat-time (sound))
	(when (> final-beat current-beat)
	 (looper nome sound (+ current-beat increment) increment final-beat)
    )))


(defn beat-length
  "Returns the length in milliseconds of a beat according to metronome
  `nome`. With `length`, returns the length in MS of a beat-duration; 1 = whole
  note, 1/4 = quarter-note, etc."

  ([nome]
     (/ (metro-tock nome) (metro-bpb nome)))
  ([nome length]
     (* length 4 (beat-length nome))))

(defn play-pattern
  "Iterates through a pattern of [beat-duration note] pairs using
  apply-at. Treats the pattern as a single voice; note events happen
  sequentially.

  (def shave-and-a-haircut
      [[3/16 :C5] [1/8 :G4] [1/16 :G4] [3/16 :A4] [3/16 :G4]
       [3/16 :rest] [3/16 :B4] [3/16 :C5] [3/16 :rest]])

  ;; play a pattern
  (play-pattern shave-and-a-haircut saw-by-note (now))

  ;; loop pattern
  (play-pattern (cycle shave-and-a-haircut) saw-by-note (now))
"

  [score instrument start-time]
  (when-not (empty? score)
    (let [[duration note] (first score),
          next-time (+ start-time (beat-length metro duration))]
      (when-not (= :rest note)
        (at start-time (instrument note)))
      (apply-at next-time
                play-pattern (rest score) instrument next-time []))))

(defmacro on-downbeat
  "Like a do form that delays evaluation until the next downbeat on the global
meto"
  ([& body]
     (let [nome (gensym)]
       `(let [~nome metro]
          (-on-downbeat ~nome (fn [] ~@body )) []))))

(defn -on-downbeat
  ([nome fun]
     (-on-downbeat nome fun 0))
  ([nome fun offset]
     (apply-at (nome (+ (* (metro-bar nome) (metro-bpb nome))
                        (* offset (metro-bpb nome)))) fun [])))

(defmacro wait [beats & body]
  " TODO: This should become the basic function the other music-rhythm
   scheduling functions depend on."
  (let [nome (gensym)]
    `(let [~nome metro]
       (-on-downbeat ~nome (fn [] ~@body) ~beats))))

(defmacro with-delays
  "Accepts body as pairs beat-durations and expressions and defers evaluation of
  each expression by the amount of its corresponding beat-duration.

  Each pair is scheduled starting from the moment of evaluation, not
  sequentially.

  (with-delays
     0 (first-part)
     1/4 (second-part))
     4 (third-part)

  Evaluating this expression would execute (first-part)
  immediately, (second-part) a quarter-note after evaluation, and (third-part)
  four bars after evaluation.
"
  ([& body]
     (let [nome (gensym)]
       `(let [~nome metro]
          (-with-delays ~nome (quote ~body) )))))

(defn -with-delays [nome pairs]
  "TODO: This should use the wait function instead of -on-downbeat."
  (loop [[offset fun & rst] pairs]
    (when fun
      (-on-downbeat nome #(eval fun) offset)
      (recur rst))))
