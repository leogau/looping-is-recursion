(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond
                  (= 0 n) 1
                  (= 1 n) acc
                  :else (recur (* acc base) (dec n))))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (nil? (rest a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (if (and (empty? seq1) (empty? seq2))
                   true
                   (if (not (= (first seq1) (first seq2)))
                     false
                     (recur (rest seq1) (rest seq2)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (if (empty? s)
      nil
      (if (pred (first s))
        i
        (recur (inc i) (rest s))))))

(defn avg [a-seq]
  (loop [num 0
         den 0
         s a-seq]
    (if (empty? s)
      (/ num den)
      (recur (+ num (first s)) (inc den) (rest s)))))

(defn parity [a-seq]
  (loop [s a-seq
         result #{}
         toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]
    (if (empty? s)
      result
      (if (contains? result (first s))
        (recur (rest s) (toggle result (first s)) toggle)
        (recur (rest s) (conj result (first s)) toggle)))))
      

(defn fast-fibo [n]
  (loop [l 1
         r 1
         num 2]
    (cond
     (= n 0) 0
     (= n 1) 1
     (= n num) r
     :else (recur r (+ l r) (inc num)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         result #{}]
    (if (empty? s)
      (into [] result)
      (if (contains? result (first s))
        (into [] result)
        (recur (rest s) (conj result (first s)))))))
