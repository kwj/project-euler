(ns macro)

(defmacro polygonal-numbers
  [sym-name f]
  (let [aux (gensym)
        p1 (gensym)
        p2 (gensym)]
    `(def ^:private ~sym-name
       (letfn [(~aux [~p1 ~p2]
                 (lazy-seq (cons (~f ~p1 ~p2)
                                 (~aux (~f ~p1 ~p2) (inc ~p2)))))]
         (cons 1 (~aux 1 2))))))
