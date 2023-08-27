(ns sg.flybot.pullable.core2
  "try use zipper to rewrite core"
  (:require
   [clojure.zip :as zip]
   [zippo.core :as zippo]))

(defn data-error [reason & {:as data}]
  (merge data #:error{:reason reason}))

;----------------------
;## Query pattern

(defn next-move
  "most code are copied from clojure.zip, works just like `zip/next`,
   while it attached direction metadata where the node travels from."
  [loc]
  (let [move  (fn [dir f]
                (fn [l]
                  (when-let [n (f l)]
                    (vary-meta n assoc ::direction dir))))
        down  (move :down zip/down)
        upright (move :upright zip/right)
        right (move :right zip/right)]
    (if (zip/end? loc)
      loc
      (or
       (and (zip/branch? loc) (down loc))
       (right loc)
       (loop [p loc]
         (if-let [n (zip/up p)]
           (or (upright n) (recur n))
           [(zip/node p) :end]))))))

(defn loc-seq 
  "sequence of `loc` using next-move"
  [loc]
  (->> (iterate next-move loc)
       (take-while (complement zip/end?))))

(defn on-zip
  [f x]
  (->> (zippo/coll-zip x)
       (loc-seq)
       (map f)))

^:rct/test
(comment
  (def f #(some-> % meta ::direction))
  (on-zip f [3 4]) ;=> [nil :down :right]
  (on-zip f [[2] 3]) ;=> [nil :down :down :upright]
  (on-zip f {:a {:b 5} :c 3})
  )
