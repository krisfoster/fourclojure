(ns fourclojure.core)

;;;
;;; 21
;;;

(defn prob21 [coll idx]
  (if (seq coll)
    (if (= idx 0)
      (first coll)
      (recur (rest coll) (dec idx)))))

(defn prob21-test []
  (and
   (= (prob21 '(4 5 6 7) 2) 6)
   (= (prob21 [:a :b :c] 0) :a)
   (= (prob21 [1 2 3 4] 1) 2)
   (= (prob21 '([1 2] [3 4] [5 6]) 2) [5 6])))

;;;
;;; 22
;;;
(defn prob22 [coll]
  (loop [coll coll cnt 0]
    (if (seq coll)
      (recur (rest coll) (inc cnt))
      cnt)))

(defn prob22-test []
  (and
   (= (prob22 '(1 2 3 3 1)) 5)
   (= (prob22 "Hello World") 11)
   (= (prob22 [[1 2] [3 4] [5 6]]) 3)
   (= (prob22 '(13)) 1)
   (= (prob22 '(:a :b :c)) 3)))

;;;
;;; 23
;;;

(defn prob23 [coll]
  (loop [ncoll () coll coll]
    (if (seq coll)
      (recur (conj ncoll (first coll)) (rest coll))
      ncoll)))

(defn prob23-non-tco-1 [coll]
  (if (seq coll)
    (conj
     (prob23-non-tco (next coll))
     (first coll))
    []))

(defn prob23-non-tco-2 [coll]
  (if (seq coll)
    (cons
     (list (first coll))
     (prob23-non-tco (next coll))
     )
    nil))

(defn prob23-test []
  (and
   (= (prob23 [1 2 3 4 5]) [5 4 3 2 1])
   (= (prob23 (sorted-set 5 7 2 7)) '(7 5 2))
   (= (prob23 [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))


;;;
;;; 24
;;;
(defn prob24 [coll]
  (reduce + 0 coll))

(defn prob24-test []
  (and
   (= (prob24 [1 2 3]) 6)
   (= (prob24 (list 0 -2 5 5)) 8)
   (= (prob24 #{4 2 1}) 7)
   (= (prob24 '(0 0 -1)) -1)
   (= (prob24 '(1 10 3)) 14)))

;;;
;;; 25
;;;
(defn prob25 [coll]
  (filter odd? coll))

(defn prob25-test []
  (and
   (= (prob25 #{1 2 3 4 5}) '(1 3 5))
   (= (prob25 [4 2 1 6]) '(1))
   (= (prob25 [2 2 4 6]) '())
   (= (prob25 [1 1 1 3]) '(1 1 1 3))))

;;;
;;; 26
;;;
(defn prob26-fib-naive [x]
  (cond
    (= x 1) 1
    (= x 2) 1
    :else (+ (prob26 (dec (dec x)))
             (prob26 (dec x)))))

(defn prob26-fib-tco-0 [x]
  (if (< x 2)
    1
    (let [fib (fn fib [a b n]
                (if (= n 2)
                  (+ a b)
                  (recur b (+ a b) (dec n))))]
      (fib 0 1 x))))

(defn prob26-fib-tco-1 [x]
  (if (< x 2)
    1
    (loop [a (bigint 0) b (bigint 1) n x]
      (if (= n 2)
        (+ a b)
        (recur b (+ a b) (dec n))))))

(def prob26-fib-tco-2
  (memoize prob26-fib-tco-1))

(map fourclojure.core/prob26-fib-tco-2 (range 100))


(defn prob26 [x]
  (let [f (fn [x]
            (if (< x 2)
              1
              (loop [a (bigint 1) b (bigint 1) n x]
                (if (= n 2)
                  (+ a b)
                  (recur b (+ a b) (dec n))))))
        mf (memoize f)]
    (map mf (range x))))

(defn prob26-test []
  (and
   (= (prob26 3) '(1 1 2))
   (= (prob26 6) '(1 1 2 3 5 8))
   (= (prob26 8) '(1 1 2 3 5 8 13 21))))

;;;
;;; 27
;;;
;; Write a function which returns true if the given sequence is a palindrome.
;;
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;;

(defn prob27 [coll]
  (= (seq coll) (reverse coll)))

(defn prob27-test []
  (and
   (false? (prob27 '(1 2 3 4 5)))
   (true? (prob27 "racecar"))
   (true? (prob27 [:foo :bar :foo]))
   (true? (prob27 '(1 1 3 3 1 1)))
   (false? (prob27 '(:a :b :c)))))


;;;
;;; 28
;;;
;; Write a function which flattens a sequence.
;;
;; Restrictions (please don't use these function(s)): flatten
;; (defn prob28-old [coll]
;;   (loop [out [] coll coll]
;;     (let [head (first coll)
;;           tail (rest coll)]
;;       (cond
;;         (nil? head) out
;;         (seq? head) (recur out head)
;;         :else (recur (conj out head))))))


(defn p28-helper [acc coll]
  (let [head (first coll)
        tail (next coll)]
    (cond
      (nil? head) acc
      (coll?))))

;;(defn prob28 [coll])


;;; First attempt at this. This will likely blow the stack for
;;; complicated input lists.
(defn prob28-helper [acc coll]
  (let [head (first coll)
        tail (next coll)]
    (cond
      (nil? head) acc
      (sequential? head) (prob28-helper (prob28-helper acc head) tail)
      :else (prob28-helper (conj acc head) tail))))

(defn prob28-first [coll]
  (prob28-helper [] coll))


;;; Second attempt that tries to combine the above 2 functions into
;;; a single definition
(defn prob28 [coll]
  (let [helper (fn helper [acc coll]
                 (let [head (first coll)
                       tail (next coll)]
                   (cond
                     (nil? head) acc
                     (sequential? head) (helper (helper acc head) tail)
                     :else (helper (conj acc head) tail))))]
    (helper [] coll)))

;;; Third attempt that doesn't blow the stack
;;; the following is maximental's soln. from the site - whioh is nice
;; particularly like the destructing args for head and tail

(defn f [[h & t]]
  (if h
    ;; Should this be sequential? instead?
    (if (coll? h)
      (concat (f h) (f t))
      (cons h (f t)))))

;; immo's soln. - which is succinct
(defn ff [x]
  (if (coll? x) (mapcat ff x) [x]))

(defn prob28-test []
  (and
   (= (prob28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
   (= (prob28 ["a" ["b"] "c"]) '("a" "b" "c"))
   (= (prob28 '((((:a))))) '(:a))))
(prob28-test)

;; 4Clojure Question 29
;;
;; Write a function which takes a string and returns a new string containing
;; only the capital letters.
;;
(defn prob29 [coll]
  (apply str (filter #(Character/isUpperCase %) coll)))

(defn prob29-test []
  (= (prob29 "HeLlO, WoRlD!") "HLOWRD")
  (empty? (prob29 "nothing"))
  (= (prob29 "$#A(*&987Zf") "AZ"))


;; 4Clojure Question 30
;;
;; Write a function which removes consecutive duplicates from a sequence.
;;

;; Following is a very functional way of solving this.
(defn prob30 [coll]
  (loop [acc [] old-head "" [h & t] coll]
    (cond
      (nil? h) acc
      (= old-head h) (recur acc h t)
      :else (recur (conj acc h) h t))))


;; Is there a more idiomatic clojure way?
;; Following is taken from Grimoire
(defn prob28-idiomatic [coll]
  (map last (partition-by identity coll)))

(defn prob30-test []
  (= (apply str (prob30 "Leeeeeerrroyyy")) "Leroy")
  (= (prob30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
  (= (prob30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))

;; 4Clojure Question 31
;;
;; Write a function which packs consecutive duplicates into sub-lists.
;;
(defn prob31 [coll]
  (partition-by identity coll))

;; My actual solution on 4clojure
;;
;;(partial partition-by identity)

(defn prob31-test []
  (= (prob31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
  (= (prob31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
  (= (prob31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))

;; 4Clojure Question 32
;;
;; Write a function which duplicates each element of a sequence.
;;
(defn prob32 [coll]
  (mapcat #(list % %) coll))

;; A nice solution from the site is:
;; (fn [xs] (interlave xs xs))

(defn prob32-test []
  (= (prob32 [1 2 3]) '(1 1 2 2 3 3))
  (= (prob32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
  (= (prob32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
  (= (prob32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

(prob32-test)

;; 4Clojure Question 33
;;
;; Write a function which replicates each element of a sequence a variable
;; number of times.
;;
(defn prob33 [coll times]
  (mapcat #(take times (repeat %)) coll))

;; Actual posted solution
;;(fn f [xs x] (mapcat #(take x (repeat %)) xs))

(defn prob33-test []
  (= (prob33 [1 2 3] 2) '(1 1 2 2 3 3))
  (= (prob33 [:a :b] 4) '(:a :a :a :a :b :b :b :b))
  (= (prob33 [4 5 6] 1) '(4 5 6))
  (= (prob33 [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
  (= (prob33 [44 33] 2) [44 44 33 33]))

;; 4Clojure Question 34
;;
;; Write a function which creates a list of all integers in a given range.
;;
;; Restrictions (please don't use these function(s)): range
;;
(defn prob34 [low hi]
  (take (- hi low) (iterate inc low)))

;;; A more recursive / functional solution would be:
(defn prob34-func [lo hi]
  (loop [acc '() cnt (- hi lo) x (- hi 1)]
    (cond
      (<= cnt 0) acc
      :else (recur (cons x acc) (dec cnt) (dec x)))))

(defn prob34-test []
  (= (prob34-func 1 4) '(1 2 3))
  (= (prob34-func -2 2) '(-2 -1 0 1))
  (= (prob34-func 5 8) '(5 6 7)))

(prob34-test)

;; 4Clojure Question 35
;;
;; Clojure lets you give local names to values using the special let-form.
;;
(def prob35 7)

(defn prob35-test []
  (= prob35 (let [x 5] (+ 2 x)))
  (= prob35 (let [x 3, y 10] (- y x)))
  (= prob35 (let [x 21] (let [y 3] (/ x y)))))


;; 4Clojure Question 36
;;
;; Can you bind x, y, and z so that these are all true?
;;
(defn prob36-test []
  (= 10 (let [x 7 y 3 z 1] (+ x y)))
  (= 4 (let  [x 7 y 3 z 1] (+ y z)))
  (= 1 (let  [x 7 y 3 z 1] z)))

;; 4Clojure Question 37
;;
;; Regex patterns are supported with a special reader macro.
;;
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; 4Clojure Question 38
;;
;; Write a function which takes a variable number of parameters and returns the
;; maximum value.
;;
;; Restrictions (please don't use these function(s)): max, max-key
;;
(defn prob38-func
  ([x] x)
  ([x & xs]
   (loop [max x head x xs xs]
     (cond
       (nil? head) max
       (> head max) (recur head (first xs) (rest xs))
       :else (recur max (first xs) (rest xs))))))

;;; The actual soln I posted - maybe a little more idiomatic?
(defn prob38 [& args]
  (first (apply sorted-set-by > args)))


(defn prob38-test []
  (= (prob38 1 8 3 4) 8)
  (= (prob38 30 20) 30)
  (= (prob38 45 67 11) 67))

;; 4Clojure Question 39
;;
;; Write a function which takes two sequences and returns the first item from
;; each, then the second item from each, then the third, etc.
;;
;; Restrictions (please don't use these function(s)): interleave
;;

(defn prob39 [] nil)

(defn prob39-test []
  (= (prob39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
  (= (prob39 [1 2] [3 4 5 6]) '(1 3 2 4))
  (= (prob39 [1 2 3 4] [5]) [1 5])
  (= (prob39 [30 20] [25 15]) [30 25 20 15]))

;;;
;;; 83
;;;

(defn prob83
  ([] false)
  ([x] false)
  ([x & xs]
   (and (not (every? true? (cons x xs)))
        (not (every? false? (cons x xs))))))

(defn prob83-test []
  (and
   (= false (prob83 false false))
   (= true (prob83 true false))
   (= false (prob83 true))
   (= true (prob83 false true false))
   (= false (prob83 true true true))
   (= true (prob83 true true true false))))
