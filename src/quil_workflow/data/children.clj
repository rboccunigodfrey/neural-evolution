(ns quil-workflow.data.children
  (:use [quil-workflow.data.objects-zones]
        [quil-workflow.evolution.mutation]
        [quil-workflow.evolution.selection]))

(defn make-child [ind id]
  (assoc ind :id id
             :position [(rand-nth (range 5 800 5))
                        (rand-nth (range 5 600 5))]
             :age 0
             :pr false
             :color [(rand-int 170) (rand-int 170) (rand-int 170)]
             :direction -1
             :pedigree (inc (:pedigree ind))))

(defn gen-children
  "Generates children, applying selection and mutation"
  [population objects redzones greenzones m-method gen-size]
  (loop [children [] child-success 0]
    (if (= gen-size child-success)
      children
      (let [cand-child
            ((mutation-method m-method)
             (make-child
               (rand-nth
                 (filter-redzones
                   (filter-greenzones population greenzones) redzones)) child-success))]
        (if (or (collided-any-ind? cand-child children)
                (collided-any-obj? cand-child objects))
          (recur children child-success)
          (recur (conj children cand-child) (inc child-success)))))))
