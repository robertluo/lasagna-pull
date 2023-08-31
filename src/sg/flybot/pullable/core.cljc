; Copyright. 2022, Flybot Pte. Ltd.
; Apache License 2.0, http://www.apache.org/licenses/

(ns ^:no-doc sg.flybot.pullable.core
  "Implementation of queries.
   
   A query is a function which can extract k v from data."
  (:require
   [sg.flybot.pullable.util :refer [data-error error?]]
   [sg.flybot.pullable.core.option :as option]
   [sg.flybot.pullable.core.context :as context]))

;;---------------------
;;# Core of pullable
;;
;; Pullable queries target data structure by using `DataQuery`, it
;; can `-run` on `data`, then call a `acceptor` to accept the result,
;; a `k` and a `v`.
;;
;; An acceptor is a context which will accept kv then produce the
;; result in its own discretion. For example, a value-only acceptor
;; can skip `k`, while a map acceptor should associate kv pairs.
;;
;; ## Core protocols and functions

(defprotocol Acceptor
  "An acceptor receives kv pair and produce a result"
  (-accept
    [acpt k v]
    "accept `k`,`v` pair, returns resulting data"))

(defprotocol DataQuery
  "DataQuery can query data then pass the key information to an acceptor"
  (-run
   [query data acceptor]
   "run `query` based on `data`, using `acceptor` to accept the result")
  (-id
    [query]
    "returns the id (a.k.a key) of the query")
  (-default-acceptor
    [query]
    "return the default acceptor if the query is running independently"))

(defn run-query
  "runs a query `q` on `data` using `acceptor` (when nil uses q's default acceptor)"
  ([q data]
   (run-query q data nil))
  ([q data acceptor]
   (-run q data (or acceptor (-default-acceptor q)))))

(defn run-bind
  [q data]
  (let [fac  (-> q meta ::context)
        rslt (run-query q data)
        m    (context/-finalize fac {})]
    (when rslt (assoc m '&? rslt))))

;;--------------------------
;; ## Implementation
;;
;; ### Acceptors

(extend-protocol Acceptor
  nil
  (-accept [_ _ _] nil))

(defn- map-acceptor
  "an acceptor of a map `m`"
  [m]
  (reify Acceptor
    (-accept [_ k v]
      (cond
        (nil? k) nil
        (nil? v) m
        :else
        (vary-meta (assoc m k v) merge (meta v))))))

^:rct/test
(comment
  (-accept (map-acceptor {}) :foo "bar") ;=> {:foo "bar"}
  (-accept (map-acceptor {}) :foo nil) ;=> {}
  (-accept (map-acceptor {}) nil "bar") ;=> nil
  )

(defn- vector-acceptor []
  (reify Acceptor
    (-accept [_ k v] [k v])))

(defn- value-acceptor
  "returns an acceptor of value only"
  []
  (reify Acceptor
    (-accept [_ _ v] v)))

;;------------------------
;;### DataQueries
;;

(defn fn-query
  "a query using a function to extract data
   - `k` the id of the query
   - `f` function takes data as its argument, returns extracted data"
  ([k]
   (fn-query k #(if (associative? %) (get % k) (data-error % k "expect associative"))))
  ([k f]
   (reify DataQuery
     (-id [_] k)
     (-default-acceptor [_] (map-acceptor {}))
     (-run [_ data acceptor]
       (-accept acceptor k (f data))))))

^:rct/test
(comment
  ;fn-query returns empty map when not found
  (run-query (fn-query :a) {:b 3}) ;=> {}
  )

(defn join-query
  "returns a joined query of two queries
    - `parent` is a query, extract data, pass it to `child`, then incorporate the result
    - `child` is a query run under `parent` context."
  [parent child]
  (reify DataQuery
    (-id [_] (-id parent))
    (-default-acceptor [_] (-default-acceptor parent))
    (-run [this data acceptor]
      (let [parent-data (-run parent data (value-acceptor))]
        (-accept
         acceptor
         (-id this)
         (if (or (nil? parent-data) (error? parent-data))
           parent-data
           (-run child parent-data (-default-acceptor child))))))))

^:rct/test
(comment
  ;join query can do nested query
  (def q (join-query (fn-query :a) (fn-query :b)))
  (run-query q {:a {:b 2}}) ;=> {:a {:b 2}}
  (run-query q {}) ;=> {}
  (run-query q {:a "bar"}) ;=>> {:a {:b error?}}
  )

(defn vector-query
  "returns a query composed by `queries`, like `core/get-keys`
   - `queries` are subqueries run in same context."
  [queries]
  (reify DataQuery
    (-id [_] (map -id queries))
    (-default-acceptor [_] (value-acceptor))
    (-run [this data acceptor]
      (-accept
       acceptor
       (-id this)
       (transduce
        (map #(run-query % data))
        (fn
          ([acc] acc)
          ([acc item] (if item (merge acc item) (reduced nil))))
        {}
        queries)))))

^:rct/test
(comment
  ; vector-query are queries on different keys
  (def vq (vector-query [(fn-query :a) (fn-query :b)]))
  (run-query vq {:a 5}) ;=> {:a 5}
  (run-query vq {:a 3 :b 4 :c 5}) ;=> {:a 3 :b 4}
  )

(defn seq-query
  "returns a query operate on sequence of maps
   - `q` is a query run on every item of the input sequence"
  [q]
  (reify DataQuery
    (-id [_] (-id q))
    (-default-acceptor [_] (value-acceptor))
    (-run [this coll acceptor]
      (if (sequential? coll)
        (-accept
         acceptor
         (-id this)
         (transduce
          (map #(run-query q %))
          conj
          []
          coll))
        (data-error coll (-id this) "expect sequential data")))))

^:rct/test
(comment
  (def sq (seq-query (fn-query :a)))
  (run-query sq {:a 2}) ;=>> error?
  (run-query sq [{:a 1} {:a 2 :b 3} {}]) ;=> [{:a 1} {:a 2} {}]
  )

(defn filter-query
  "returns a query filter the result with `q` only when it satisfies `pred`
   - `q` is a query
   - `pred` is a predict with arguments of `data` and value result from `q` as its argument"
  [q pred]
  (reify DataQuery
    (-id [_] (-id q))
    (-default-acceptor [_] (-default-acceptor q))
    (-run [this data acceptor]
      (let [d (run-query q data (value-acceptor))]
        (-accept acceptor (when (pred data d) (-id this)) nil)))))

^:rct/test
(comment
  (def fq (filter-query (fn-query :a) (fn [_ d] (odd? d))))
  ;;if pred success, the result does not contain its data
  (run-query fq {:a 1}) ;=> {}
  ;;if pred fail, filter query voids the result
  (run-query fq {:a 2}) ;=> nil
  ;;Using in a vector query, it can make the whole result nil
  (run-query (vector-query [fq (fn-query :b)]) {:a 0 :b 4 :c 5}) ;=> nil
  (run-query (vector-query [fq (fn-query :b)]) {:a 1 :b 4 :c 5}) ;=> {:b 4}
  )

(defn named-query 
  "returns a named query"
  [q v-name]
  (reify DataQuery
    (-id [_] (-id q))
    (-default-acceptor [_] (-default-acceptor q))
    (-run
      [this data acceptor]
      (let [bindings (some-> data meta ::bindings)
            v-value  (if bindings (get bindings v-name ::unbound) ::unbound)]
        (if (= v-value ::unbound)
          (let [v    (-run q data (value-acceptor))
                rslt (-accept acceptor (-id this) v)]
            (vary-meta rslt update ::bindings assoc v-name v))
          (-run (filter-query q (fn [_ v] (= v v-value))) data acceptor))))))

^:rct/test
(comment
  (def nq (named-query (fn-query :a) '?a))
  ;first time bindings
  ((juxt identity meta) (run-query nq {:a 1})) ;=>> [{:a 1} {::bindings {'?a 1}}]
  ;bound
  (run-query nq ^{::bindings {'?a 2}} {:a 1}) ;=> nil
  (def nq (join-query (fn-query :a) (named-query (fn-query :b) '?b)))
  ((juxt identity meta) (run-query nq {:a {:b 1}})) ;=>> [{:a {:b 1}} {::bindings {'?b 1}}] 
  )


(defn context-of
  ([modifier finalizer]
   (reify context/QueryContext
     (-wrap-query
       [_ q args]
       (reify DataQuery
         (-id [_] (-id q))
         (-default-acceptor [_] (-default-acceptor q))
         (-run [_ data acceptor]
           (let [[k v] (->> (-run q data (vector-acceptor)) (modifier args))]
             (-accept acceptor k v)))))
     (-finalize
       [_ m]
       (finalizer m)))))

(defn named-query-factory
  "returns a new NamedQueryFactory
   - `a-symbol-table`: an atom of map to accept symbol-> val pair"
  ([]
   (named-query-factory (transient {})))
  ([symbol-table]
   (let [set-val! (fn [sym v] (get (assoc! symbol-table sym v) sym))]
     (context-of
      (fn [[sym] [k v :as pair]]
        (if (and (some? sym) (some? k))
          (let [old-v    (get symbol-table sym ::not-found)
                rslt
                (condp = old-v
                  v           v
                  ::not-found (set-val! sym v)
                  ::invalid   ::invalid
                  (set-val! sym ::invalid))]
            [(when (not= rslt ::invalid) k) rslt])
          pair))
      #(into % (filter (fn [[_ v]] (not= ::invalid v))) (persistent! symbol-table))))))

(defn- mock-query [k v]
  (reify DataQuery
    (-id [_] k)
    (-default-acceptor [_] (map-acceptor {}))
    (-run [_ _ _] [k v])))

^:rct/test
(comment
  (defn try-named [init k v]
    (let [fac (named-query-factory (transient init))]
      (-> (context/-wrap-query fac (mock-query k v) ['?a]) (run-bind {}))
      (context/-finalize fac {})))
  (try-named {} :foo "bar")  ;=> {?a "bar"}
  (try-named {'?a "none"} :foo "bar") ;=> {}
  (try-named {'?a "bar"} :foo "bar") ;=> {?a "bar"}
  (try-named {'?a ::invalid} :foo "bar") ;=> {}
  (try-named {'?a ::invalid} :foo "bar") ;=> {} 
  ;;failed binding does not affect existing bindings
  (try-named {'?a "bar"} nil "bar") ;=> {?a "bar"}
  )

;;### post-process-query
;; It is common to process data after it matches

(defn post-process-query
  "A query decorator post process its result. Given query `child`, the function
   `f-post-process` may change its value and return.
      - child: a query
      - f-post-process: [:=> [:kv-pair] [:kv-pair]]"
  ([child f-post-process]
   (reify DataQuery
     (-id [_] (-id child))
     (-default-acceptor [_] (-default-acceptor child))
     (-run [this data acceptor]
       (let [v (some-> (run-query child data (vector-acceptor))
                       (f-post-process)
                       (second))]
         (-accept acceptor (-id this) v))))))

;;## Post processors

;; Post processors apply after a query, abbreciate to `pp`

(defn decorate-query
  "Returns a query which is `q` decorated by options specified by `pp-pairs`."
  ([q pp-pairs]
   (reduce
    (fn [acc [pp-type pp-value]]
      (post-process-query
       acc
       (option/apply-post #:proc{:type pp-type :val pp-value})))
    q pp-pairs)))

(defn query-maker
  "returns a query making function which takes a vector as argment and construct a query."
  [context]
  (let [named-fac (named-query-factory)
        context (context/composite-context [context named-fac])
        f-map {:fn     fn-query
               :vec    vector-query
               :join   join-query
               :filter filter-query
               :seq    seq-query
               :named  (fn [q & args] (context/-wrap-query named-fac q args))
               :deco   decorate-query}]
    (fn [[query-type & args]]
      (with-meta
        (let [f (get f-map query-type)]
          (if f
            (context/-wrap-query context (apply f args) nil)
            (throw (ex-info "unknown query type" {:type query-type}))))
        {::context context}))))
