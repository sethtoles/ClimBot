(ns climBot.core
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata))

(def settings {:max-argument-depth 5
               :max-conditional-depth 3
               :max-block-length 3
               :max-start-length 40
               :min-start-length 15
               :time-limit 30000
               :tournament-size 7})

(def robot (arduino :firmata "/dev/tty.usbserial-A6008l3q"))

(def controller (arduino :firmata "/dev/tty.usbserial-A7006RXX"))
(Thread/sleep 2000)

(def pins {:button [2 3 4 5] :servo [6 7 8 9 10 11 12 13] :sensor [0 1 2 3 4 5]})

(map #(pin-mode robot % SERVO) (pins :servo))

(def servo-mins {6 115, 7 115, 8 100, 9 100, 10 105, 11 105, 12 80, 13 80}) 

(map #(enable-pin robot :digital %) (pins :button))

(map #(pin-mode robot % INPUT) (pins :button))

(map #(enable-pin robot :analog %) (pins :sensor))

(map #(pin-mode controller % SERVO) (range 2 5))

(enable-pin controller :analog 0)

(enable-pin controller :digital 5)

(pin-mode controller 5 INPUT)

(analog-write controller 3 50)

(defn servo-write
  [pin value]
  (analog-write robot pin 
                (let [servo-min (get servo-mins pin)]
                  (cond (< 180 value) 180
                        (> servo-min value) servo-min
                        :else value))))

(defn sensor-read
  [pin]
  (+ 80 (int (/ (* (analog-read robot pin) 100) 1023))))

(defn button-read
  [pin]
  (digital-read robot pin))

(defn measure
  []
  (if (> 570 (analog-read controller 0))
    0
    (- (analog-read controller 0) 570)))

(defn millis
  []
  (long (/ (System/nanoTime) 1000000)))

(defn rand-comparator
  []
  (rand-nth '[< > =]))

(defn pd
  [a b]
  (if (zero? b)
    0
    (int (/ a b))))

(defn rand-operator
  []
  (rand-nth '[+ - * pd]))

(defn rand-argument
  ([]
    (rand-argument (settings :max-argument-depth)))
  ([max-depth]
    (if-not (zero? max-depth)
      (rand-nth [(rand-int 180)
                 (list 'sensor-read (rand-nth (pins :sensor)))
                 (list (rand-operator) (rand-argument (dec max-depth)) (rand-argument (dec max-depth)))])
      (rand-nth [(rand-int 180)
                 (list 'sensor-read (rand-nth (pins :sensor)))]))))

(defn rand-test
  []
  (rand-nth [(list (rand-comparator) (list 'sensor-read (rand-nth (pins :sensor)))
                   (rand-argument))
             (list '= (list 'button-read (rand-nth (pins :button)))
                   (rand-nth [(list 'button-read (rand-nth (pins :button)))
                              (rand-int 2)]))]))

(defn rand-command
  ([]
    (rand-command (settings :max-conditional-depth)))
  ([max-depth]
    (let [non-recursive
          (list (list 'servo-write (rand-nth (pins :servo)) (rand-argument))
                (list 'Thread/sleep (rand-int 500)))]
      (if (zero? max-depth)
        (rand-nth non-recursive)
        (rand-nth (conj non-recursive
                        (list* 'when (rand-test)
                               (repeatedly
                                 (inc (rand-int (settings :max-block-length)))
                                 #(rand-command (dec max-depth))))))))))

(defn generate-individual
  []
  (repeatedly 
    (+ (settings :min-start-length) (rand-int (- (settings :max-start-length) (settings :min-start-length))))
    #(rand-command)))

(defn count-whens
  [coll]
  (apply + (for [i coll]
             (if (= (first i) 'when)
               1
               0))))

(defn nth-when-at
  [coll num]
  (loop [index 0
         count 0]
    (if (= count num)
      (dec index)
      (if (= (first (nth coll index)) 'when)
        (recur (inc index)
               (inc count))
        (recur (inc index)
               count)))))
    
(defn grow
  ([individual]
    (grow individual (rand-command)))
  ([individual command]
    (let [in-betweens (inc (count individual))
          pos (rand-int (+ in-betweens (count-whens individual)))]
      (if (< in-betweens pos)
        (let [when-pos (nth-when-at individual (- pos in-betweens))]
          (concat (take when-pos individual)
                  [(concat (take 2 (nth individual when-pos))
                           (grow (drop 2 (nth individual when-pos)) command))]
                  (drop when-pos individual)))
        (concat (take pos individual)  
                [command]
                (drop pos individual))))))

(defn shrink
  [individual]
  (let [length (count individual)
        pos (inc (rand-int (+ length (count-whens individual))))]
    (if (< length pos)
      (let [when-pos (nth-when-at individual (- pos length))]
        (concat (take when-pos individual)
                [(concat (take 2 (nth individual when-pos))
                         (shrink (drop 2 (nth individual when-pos))))]
                (drop (inc when-pos) individual)))
      (concat (take (dec pos) individual)  
              (drop pos individual)))))

(defn inject
  ([individual]
    (inject individual (rand-command)))
  ([individual command]
    (let [length (count individual)
          pos (inc (rand-int (+ length (count-whens individual))))]
      (if (< length pos)
        (let [when-pos (nth-when-at individual (- pos length))]
          (concat (take when-pos individual)
                  [(concat (take 2 (nth individual when-pos))
                           (inject (drop 2 (nth individual when-pos)) command))]
                  (drop (inc when-pos) individual)))
        (concat (take (dec pos) individual)  
                [command]
                (drop pos individual))))))
          
(defn extract
  [individual]
  (let [length (count individual)
        pos (rand-int (+ length (count-whens individual)))]
    (if (> length pos)
      (nth individual pos)
      (extract (drop 2 (nth individual (nth-when-at individual (- (inc pos) length))))))))

(defn reset-robot
  []
  (servo-write 6 170)
  (servo-write 7 120)
  (servo-write 8 180)
  (servo-write 9 100)
  (servo-write 10 180)
  (servo-write 11 120)
  (servo-write 12 135)
  (servo-write 13 115))

(defn clutch
  [state]
  (if (= 1 state)
    (analog-write controller 3 72)
    (analog-write controller 3 25)))

(defn stop-reset
  []
  (analog-write controller 2 92)
  (analog-write controller 4 90))

(defn timed-eval
  ([]
    (timed-eval (millis) (settings :time-limit) (generate-individual)))
  ([start-time time-limit code]
  (if (< (- (millis) start-time) time-limit)
    (if (seq? (first code))
      (doseq [cmd code]
        (timed-eval start-time time-limit cmd))
      (if (= (first code) 'when)
        (if (eval (second code))
          (doseq [cmd (nnext code)]
            (timed-eval start-time time-limit cmd)))
        (eval code))))))

(defn liftoff
  []
  (clutch 1)
  (analog-write controller 2 60)
  (Thread/sleep 800)
  (analog-write controller 2 92)
  (let [start-time (millis)
        lift-time (+ (- 0 (* 0.0667 (* (measure) (measure))))
                     (* 71.598 (measure))
                     9000)]
    (while (< (millis) (+ start-time lift-time))
      (analog-write controller 4 180))
    (analog-write controller 4 90)))

(defn lower-robot
  []
  (while (or (< 0 (measure)) (= 1 (digital-read controller 5)))
    (if (< 0 (measure))
      (analog-write controller 2 180)
      (analog-write controller 2 92))
    (if (= 1 (digital-read controller 5))
      (analog-write controller 4 (int (- 90 (/ (- 420 (measure)) 13))))
      (analog-write controller 4 90)))
  (analog-write controller 2 20)
  (analog-write controller 4 180)
  (Thread/sleep 2000)
  (let [start-time (millis)]
    (while (< (millis) (+ start-time 2200))    
      (analog-write controller 2 180)
      (if (= 1 (digital-read controller 5))
        (analog-write controller 4 0) 
        (analog-write controller 4 90))))
  (stop-reset)
  (Thread/sleep 1000))

(defn run-individual
  [individual]
  (timed-eval (millis) (settings :time-limit) individual)
  (let [fitness (analog-read controller 0)]
    (liftoff)
    (reset-robot)
    (lower-robot)
    (clutch 0)
    fitness))

(defn sort-by-fit
  [population]
  (sort-by #(get (meta %) :fitness) 
           (for [i population]
             (with-meta (seq i) {:fitness (run-individual i)}))))
               
(defn mutate
  [individual]
  (let [method (rand-int 3)]
    (case method
      0 (inject individual)
      1 (grow individual)
      2 (shrink individual))))                         
                           
(defn select
  ([population]
    (select population (settings :tournament-size)))
  ([population tournament-size]
    (let [size (count population)]
      (nth population
           (apply min (repeatedly tournament-size #(rand-int size)))))))
    
(defn evolve
  [popsize duration]
  (let [filename (str "Run-Results-" (millis))]
    (println "Starting evolution...")
    (loop [generation 1
           population (sort-by-fit (repeatedly popsize #(generate-individual)))]
      (let [best (first population)]
        (println "======================")
        (spit filename
              (apply str "\n \n" "Generation:" generation "\n" (interpose "\n" population))
              :append true)
        (println "Generation:" generation)
        (println "Highest climb:" (get (meta best) :fitness))
        (if (< generation duration) 
          (recur 
            (inc generation)
            (sort-by-fit      
              (concat
                (repeatedly (* 1/2 popsize) #(mutate (select population)))
                (repeatedly (* 1/4 popsize) #(inject (select population)
                                                     (extract (select population))))
                (repeatedly (* 1/4 popsize) #(select population))))))))))