; Adaptation of David Randall Miller's evolutionary program by Rafael Boccuni-Godfrey and Aryan Raval
; https://www.youtube.com/watch?v=N3tRFayqVtk

;
; MIT License
;
; Copyright (c) 2021-2022 David R. Miller and other contributors. For the
; exact contribution history, see the revision logs at the project page
; https://github.com/davidrmiller/biosim4.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(ns neural-evolution.core
  (:use [neural-evolution.views.textual]
        [neural-evolution.views.visual]))


(defn -main [view]
  (case view
    :textual (evolve-agents 500 300 12 100 :right :replace)
    :visual (defonce sketch (animate-agents))))

#_(-main :visual)