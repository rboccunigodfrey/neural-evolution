(ns neural-evolution.data.children
  (:use [neural-evolution.data.objects-zones]
        [neural-evolution.data.individuals]
        [neural-evolution.evolution.mutation]
        [neural-evolution.evolution.selection]))

(defn make-child [ind id]
  (assoc ind :id id
             :position [(rand-nth (range 5 800 5))
                        (rand-nth (range 5 600 5))]
             :age 0
             :pr false
             :color [(min 255 (* 25 (:kill-count ind) #_(min 0 (- (:kill-count ind) (:gather-count ind)))))
                     0
                     (min 255 (* 25 (:gather-count ind) #_(min 0 (- (:gather-count ind) (:kill-count ind)))))]
             :direction -1
             :energy (:start-energy ind)
             :killing-id -1
             :gathering-id -1
             :kill-cooldown 0
             :gather-cooldown 0))

(defn gen-children
  "Generates children, applying selection and mutation"
  [population objects redzones greenzones m-method s-method gen-size genome-size]
  (loop [children [] child-success 0]
    (if (= gen-size child-success)
      children
      (let [slow-rate 0.01
            fast-rate 0.075
            filtered-pop (filter-redzones
                           (filter-greenzones ((select-method s-method) population) greenzones) redzones)
            cand-child
            (if (empty? filtered-pop)
              ((mutation-method m-method fast-rate)
               (make-child
                 (rand-nth (take 10 (sort-by :energy > population)))
                 child-success))
              (if (< 20 (count filtered-pop))
                (nth [((mutation-method m-method fast-rate)
                            (make-child
                              (rand-nth (take 20 (sort-by :energy > population)))
                              child-success))
                           ((mutation-method m-method slow-rate)
                           (make-child
                             (rand-nth filtered-pop)
                             child-success))]
                     (rand-nth (conj (repeat (int (- 5 (/ (count filtered-pop) 4))) 0) 1)))
                ((mutation-method m-method slow-rate)
                 (make-child
                   (rand-nth filtered-pop)
                   child-success))))]
        (if (or (collided-any-ind? cand-child children)
                (collided-any-obj? cand-child objects))
          (recur children child-success)
          (recur (conj children cand-child) (inc child-success)))))))
