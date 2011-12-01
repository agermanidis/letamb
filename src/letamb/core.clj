(ns letamb.core
  (:use [clojure.contrib.combinatorics :only [cartesian-product]]))

(defmacro letamb
  "Usage: (let-amb seq-exprs body-expr)

  Based on McCarthy's original amb operator. 

  (letamb [x (range 10)] (do (assert (= x 3)) x)) => 3"
  [seq-exprs & body-expr]
  (let [names (take-nth 2 seq-exprs)
        value-ranges (map eval (take-nth 2 (next seq-exprs)))
        possible-values (apply cartesian-product value-ranges)]
    `(let [body# (fn [[~@names]] (try (do ~@body-expr)
                                     (catch AssertionError e# ::error#)))]
       (loop [[v# & vs#] '~possible-values]
         (let [result# (body# v#)]
           (if (= result# ::error#)
             (if (nil? vs#)
               (throw (Exception. "Out of options!"))
               (recur vs#))
             result#))))))



