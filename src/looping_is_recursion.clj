(ns looping-is-recursion)

(defn power [base exp]
  (let [accs (fn [base exp acc]
               (if ( = 0 exp) acc
                   (recur base (- exp 1) (* acc base))))]
    (accs base exp 1)))

(defn last-element [[f & r]]
  (if (empty? r)
    f
    (recur r)))

(defn seq= [seq1 seq2]
  (if (not ( = (first seq2)
               (first seq1)))
    false
    (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq) nil
      (if (pred (first a-seq))
        (first a-seq)
        (recur pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [count 0 acc 0 sec a-seq]
    (if (empty? sec) (/ acc count)
        (recur (inc count) (+ acc (first sec)) (rest sec)))))

(defn parity [a-seq]
  (loop [odds #{} sec a-seq]
    (if (empty? sec) odds
     (recur ((if (contains? odds (first sec))
               disj
               conj) odds (first sec)) (rest sec)))))

(defn fast-fibo [n]
  (loop [it 2 fna 1 fn-1 0]
    (cond
     (= it n) (+ fna fn-1)
     :else (recur (inc it) (+ fna fn-1) fna))))

(defn cut-at-repetition [a-seq]
  (loop [acc #{} seq a-seq ret []]
    (if (contains? acc (first seq))
      ret
      (recur (conj acc (first seq)) (rest seq) (conj ret (first seq))))))
