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
  (let [move    (fn [dir f]
                  (fn [l]
                    (when-let [n (f l)]
                      (vary-meta n update ::direction (fnil conj []) dir))))
        down    (move :down zip/down)
        up      (move :up zip/up)
        right   (move :right zip/right)]
    (if (zip/end? loc)
      loc
      (or
       (and (zip/branch? loc) (down loc))
       (right loc)
       (loop [p loc]
         (if-let [n (up p)]
           (or (right n) (recur n))
           [(zip/node p) :end]))))))

(defn loc-seq 
  "sequence of `loc` using next-move"
  [loc]
  (->> (iterate next-move loc)
       (take-while (complement zip/end?))))

^:rct/test
(comment 
  (defn travel-plan
    [pattern]
    (-> (->> (zippo/coll-zip pattern) (loc-seq) (last))
        meta ::direction))
  (travel-plan [3 4]) ;=>  [:down :right]
  (travel-plan [[2] 3]) ;=> [:down :down :up :right]
  (travel-plan (array-map :a {:b 5} :c 3)) 
  ;=>> [:down :down :right :down :down :right :up :up :up :right :down :right]
  )

(defn data-error [reason & {:as data}]
  (merge data {:error/reason reason}))

(defn- editing [x]
  (cond
    (map? x) #(into (array-map) (select-keys % (keys x)))
    :else nil))

(defn query-of
  "compiles a pattern into a function, the result function can be used to
   query data."
  [pattern]
  (let [dirs {:up zip/up :down zip/down :right zip/right}]
    (fn [data]
      (let [locs (->> (zippo/coll-zip pattern) (loc-seq))
            directions (some->> locs last meta ::direction (map dirs))
            fs (->> locs 
                    (map (fn [loc] 
                           (if-let [f (editing (zip/node loc))]
                             #(zip/edit % f)
                             identity))))]
        (reduce (fn [v f] (if-let [loc (f v)] loc (reduced nil)))
                (zippo/coll-zip data) 
                (concat (interleave fs directions) [zip/root]))))))

^:rct/test
(comment
  ((query-of {:a '? :b '?}) {:a 1 :b 2 :c 4}) ;=> {:a 1 :b 2}
  ((query-of {:a {:b '?} :c '?}) {:a {:b 1 :a1 :3} :c 2 :d 4}) ;=> {:a {:b 1} :c 2}
  )