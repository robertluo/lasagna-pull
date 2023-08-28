(ns sg.flybot.pullable.core2
  "try use zipper to rewrite core"
  (:require
   [clojure.zip :as zip]
   [zippo.core :as zippo]))

;;--------------------
;; ##zip basic improve

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

^:rct/test
(comment
  (defn on-zip [x] (->> (zippo/coll-zip x) (loc-seq) (map #(some-> % meta ::direction))))
  (on-zip [3 4]) ;=> [nil :down :right]
  (on-zip [[2] 3]) ;=> [nil :down :down :upright]
  (on-zip {:a {:b 5} :c 3})
  )

(defn data-error [reason & {:as data}]
  (merge data {:error/reason reason}))

(defn editing [x]
  (cond
    (map? x) #(select-keys % (keys x))
    :else identity))

(let [dirs {:right   [zip/right]
            :down    [zip/down]
            :up      [zip/up]
            :upright [zip/up zip/right]}]
  (defn loc->f
    [ptn-loc] 
    (let [direction (some-> ptn-loc meta ::direction)
          fs (conj (get dirs direction []) (fn [loc] (zip/edit loc (editing (zip/node ptn-loc)))))]
      fs)))

(defn pattern->f
  [x]
  (comp zip/root
        (->> (zippo/coll-zip x)
             (loc-seq)
             (mapcat loc->f)
             (reverse)
             (apply comp))))

^:rct/test
(comment
  ((pattern->f {:a '? :b '?}) (zippo/coll-zip {:a 1 :b 2 :c 4})) ;=> {:a 1 :b 2}
  ((pattern->f {:a {:b '?}}) (zippo/coll-zip {:a {:b 1} :c 2})) ;=> {:a {:b 1}}
  )