; Copyright. 2022, Flybot Pte. Ltd.
; Apache License 2.0, http://www.apache.org/licenses/

(ns sg.flybot.pullable
  "Pull from data structure by using pattern.
   
   Pattern is a DSL in clojure data structure, specify how to extract
   information from data." 
  (:require [sg.flybot.pullable.core :as core]
            [sg.flybot.pullable.pattern :as ptn] 
            [sg.flybot.pullable.util :as util]))

;; ## APIs

(def ^:dynamic *data-schema* nil)

(defn query
  "Returns a query function from `pattern`. A query function can be used to extract information
   from data. Query function takes `data` as its single argument, if data matches the pattern,
   returns a map of resulting data and output variable bindings, the whole matching result is in
   `'&?` binding and other named logical variable bindings.
   
   A pattern is a clojure data structure, in most cases, a pattern looks like the data
   which it queries, for example, to query data `{:a {:b {:c 5}} :d {:e 2}}` to extract
   `:c`, `:e`, you might use a pattern `'{:a {:b {:c ?c}} :d {:e ?e}}`. By marking the
   information you are interested in by logic variable like `?c`, `?e`, or by anonymous
   variable `?`, you can get informatioon from the potentially massive data.

     - Map pattern: `{...}`, mimic the shape of the map to be queried
   
     - Filter pattern: If you specified a concrete value for a given key in a map, it works
       like a filter, i.e., pattern `{:a ? :b 1}` will return an empty map on data `{:a 1 :b 2}`.
       If you use same logical variabl in multiple places of a pattern, it has to be the
       same value, so if we can not satisfy this, the matching fails. i.e., 
       pattern `'{:a ?x :b ?x}` returns an empty map on data `{:a 2 :b 3}`.
   
     - Options: by enclosing a key in map pattern with a list `()`, and optional pairs of
       options, you can process value after a match or miss. i.e., pattern
       `{(:a :not-found ::ok) ?}` on data `{:b 5}` having a matching result of `{:a ::ok}`.
       Various options supported, and can be defined by multimethod of `core/apply-post`.
   
     - Sequence pattern: For sequence of maps, using `[]` to enclose it. Sequence pattern
       can have an optional variable and options. i.e., pattern 
       `'[{:a ?} ?]` on `[{:a 1} {:a 3} {}]` has a matching result of
       `[{:a 1} {:a 3} {}]`. 
   
   Advanced arguments. Sometimes you need subqueries sharing information among them.
     - You can pass an additional `context` to it, which is a `QueryContext` instance made by
       `context-of` function."
  ([pattern]
   (query pattern nil))
  ([pattern context]
   #?(:clj
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (util/optional-require
       [sg.flybot.pullable.schema :as schema]
       #_{:clj-kondo/ignore [:unresolved-namespace]}
       (schema/check-pattern! *data-schema* pattern)
       nil))
   (fn [data]
     (-> context
         core/query-maker
         (ptn/->query pattern ptn/filter-maker)
         (core/run-bind data)))))

^:rct/test
(comment
  (query {:a 3}) ;=>> fn?
  (query 3) ;throws=>> #:error{:class clojure.lang.ExceptionInfo}
  )

(defn context-of
  "returns a query context of function `modifier` and function `finalizer`(default `clojure.core/identity`),
   this could be used to:
     - modify query result, the value returned by `modifier` will be in the query result.
     - collect information in all query results.
   Information on arguments:
   - `modifier`: returns a key-value pair which will contains in the query result. argurments are:
     - `args`: a vector that might passed to this.
     - `key-value`: a key-value pair returned by original query running result.
   - `finalizer`: accept a single map and returns any value which will be called when we've done querying."
  ([modifier]
   (context-of modifier identity))
  ([modifier finalizer]
   (core/context-of modifier finalizer)))

(defn run-query
  "Query `data` on `pattern`, returns a pair. See `query` function. Convinient for one-time use pattern."
  ([pattern data]
   (run-query query pattern data))
  ([f pattern data]
   ((f pattern) data)))

^:rct/test
(comment
  ; issue 67 failed case
  ((query '{:user/items [{:reward/store {:store/identity    "id2"
                                         :store/description ?desc}
                          :item/expired-at ?expired-at}]})
   {:user/items [{:reward/store {:store/identity    "id1"
                                 :store/description "description"}
                  :item/expired-at 100}
                 {:reward/store {:store/identity    "id2"
                                 :store/description "descriptio2"}
                  :item/expired-at 200}]})
  ;=> {?desc "descriptio2" ?expired-at 200
  ;    &? #:user{:items [nil #:item{:expired-at 200}]}}
  )

(defmacro qfn 
  "returns an anonymous query function on `pattern` takes a single argument `data`:
   Returns `nil` if failed to match, otherwise, all logical variables in the pattern
   can be used in `body`. The whole query result stored in `&?`.
   See `query`'s documentation for syntax of the pattern." 
  {:style/indent 1}
  [pattern & body]
  (let [syms (-> (util/named-lvars-in-pattern pattern) vec)]
    `(let [q# (query ~pattern)]
       (fn [data#]
         (when-let [{:syms ~syms} (q# data#)]
           ~@body)))))

^:rct/test
(comment
  ;;logical vars only bound on succeeding
  ((qfn '[{:a ?a :b 2}] ?a)
   [{:a 1 :b 1} {:a 2 :b 2} {:a 3 :b 1}]) ;=> 2
  ;;failed
  ((qfn '{:a ?x :b {:c ?c :d ?x}} [?c ?x]) {:a 1 :b {:c 2 :d 2}}) ;=> nil
  ;;testing context
  (defn my-ctx []
    (let [shared (transient [])]
      (context-of
       (fn [_ [k v]] (when (number? v) (conj! shared v)) [k v])
       #(into % {:shared (persistent! shared)}))))
  ((query '{:a ? :b ?a} (my-ctx)) {:a 3 :b 4}) ;=> {&? {:a 3, :b 4} :shared [4 4 3], ?a 4}
  
  (macroexpand-1 '(qfn {:a ?a :b ?b} (+ ?a ?b)))
  ((qfn '{:a ?a :b ?b} (+ ?a ?b)) {:a 1 :b 2}) ;=> 3
  ) 

#?(:clj
   (defmacro with-data-schema 
     [schema & body]
     `(binding [*data-schema* ~schema]
        ~@body)))

^:rct/test
(comment
  (macroexpand-1 '(with-schema nil nil))
  (with-data-schema [:map [:a :int]] (qfn '{:a 3})) ;=>> fn?
  (with-data-schema [:map [:a :int]] (qfn '{:a "3"})) ;throws=>> some?
  )