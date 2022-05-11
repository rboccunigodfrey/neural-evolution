; Adaptation of David Randall Miller's evolutionary program by Rafael Boccuni-Godfrey and Aryan Raval
; https://www.youtube.com/watch?v=N3tRFayqVtk

(ns quil-workflow.core
  (:use [quil-workflow.views.textual]
        [quil-workflow.views.visual]))


(defn -main [view]
  (case view
    :textual (evolve-agents 500 300 12 100 :right :replace)
    :visual (defonce sketch (animate-agents))))

(-main :visual)