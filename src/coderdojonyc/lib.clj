(ns coderdojonyc.lib
    (:gen-class))
(use 'overtone.live)

;; We use a saw-wave that we defined in the oscillators tutorial
(definst saw-wave [freq 440 attack 0.01 sustain 0.1 release 0.01 vol 0.8] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

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
`nome`. With `length` argument, returns the length of a musical duration; 1 =
whole note, 1/4 = quarter-note, etc."

  ([nome]
     (/ (metro-tock nome) (metro-bpb nome)))
  ([nome length]
     (* length 4 (beat-length nome))))

(defonce metro (metronome 120))

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
  " TODO: This should be the basic function the other music-rhythm scheduling
functions depend on."
  (let [nome (gensym)]
    `(let [~nome metro]
       (-on-downbeat ~nome (fn [] ~@body) ~beats))))

(defmacro with-delays
  "Aceepts pairs of delays and expressions and defers eveluation by that many
  beats.

  (with-delays
     0 (first-part)
     1/4 (second-part))
     4 (third-part)

  Would execute the (first-part) right away, (first-part) a quarter-note after
  the start, and (third-part) four bars after the start.
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
