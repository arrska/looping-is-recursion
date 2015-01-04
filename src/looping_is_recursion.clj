(ns looping-is-recursion)

(defn power [base exp]
  (let [helper 
        (fn [acc n] 
          (if (zero? n) acc
          (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (= 1 (count a-seq)) (first a-seq)
    (empty? a-seq) nil
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond
      (= i (count a-seq)) nil
      (pred (nth a-seq i)) i
      :else (recur (inc i)))))
;
;  (loop [i 0
;         rest-seq a-seq]
;    (cond 
;      (empty? rest-seq) nil
;      (pred (first rest-seq)) i
;      :else (recur (inc i) (rest rest-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n (dec (count a-seq))]
    (if (neg? n)
      (/ sum (count a-seq))
      (recur 
        (+ sum (nth a-seq n)) 
        (dec n)))))

(defn parity [a-seq]
  (loop [i (dec (count a-seq))
         a-set #{}]
    (if (neg? i) a-set
      (recur
        (dec i)
        (let [elem (nth a-seq i)] 
          ((if (contains? a-set elem) disj conj) a-set elem))))))

;  (loop [i (dec (count a-seq))
;         a-map {}]
;    (if (neg? i)
;      (map first (filter (fn [[k v]] (odd? v)) a-map))
;      (recur (dec i) 
;             (let [val-a (nth a-seq i)
;                   n (or (get a-map val-a) 0)]
;               (assoc a-map val-a (inc n)))))))

(defn fast-fibo [n]
  (loop [Fi   1
         Fi-1 0
         i    1]
    (cond
      (zero? n) 0
      (= n i) Fi
      :else (recur (+ Fi Fi-1) Fi (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         i 0]
    (cond
      (= i (count a-seq)) a-seq
      (contains? seen (nth a-seq i)) (take i a-seq)
      :else (recur (conj seen (nth a-seq i)) (inc i)))))

