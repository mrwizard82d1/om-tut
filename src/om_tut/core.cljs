;; Remember that one *must* re-run `lein figwheel` when changing the top namespace form.
(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)

(defonce app-state
  (atom {:people [{:type :student :first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
                  {:type :student :first "Alyssa" :middle-initial "P" :last "Hacker"
                   :email "aphacker@mit.edu"}
                  {:type :professor :first "Gerald" :middle "Jay" :last "Sussman" 
                   :email "metacirc@mit.edu" :classes [:6001 :6946]}
                  {:type :student :first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
                  {:type :student :first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
                  {:type :professor :first "Harold" :last "Abelson" :email "evalapply@mit.edi"
                   :classes [:6001]}]
         :classes {:6001 "The Structure and Interpretation of Computer Programs"
                   :6946 "The Structure and Interpretation of Classical Mechanics"
                   :1806 "Linear Algebra"}}))

(defn middle-name [{:keys [middle middle-initial]}]
  (cond 
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn student-view [student owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil (display-name student)))))

(defn professor-view [professor owner]
  (reify
    om/IRender
    (render [_]
      (dom/li nil
              (dom/div #js {:key (:first professor)} (display-name professor))
              (dom/label nil "Classes")
              (apply dom/ul nil
                     (map #(dom/li nil %) (:classes professor)))))))

(defmulti entry-view (fn [person _] (:type person)))

(defmethod entry-view :student
  [person owner] (student-view person owner))

(defmethod entry-view :professor
  [person owner] (professor-view person owner))

(defn people [data]
  (->> data
       :people
       (mapv (fn [p] 
               (if (:classes p)
                 (update-in p 
                            [:classes]
                            (fn [cs]
                              (mapv (:classes data) cs)))
                 p)))))

(defn registry-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "registry"}
               (dom/h2 nil "Registry")
               (dom/div nil (om/build-all entry-view (people data)))))))

(defn classes-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "classes"}
               (dom/h2 nil "Classes")
               (apply dom/ul nil
                      (map #(dom/li nil %) (vals (:classes data))))))))

(om/root registry-view
         app-state
         {:target (. js/document (getElementById "registry"))})

(om/root classes-view
         app-state
         {:target (. js/document (getElementById "classes"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
