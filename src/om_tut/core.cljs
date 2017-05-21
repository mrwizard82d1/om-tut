(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

(om/root
  (fn [data owner]
    ;; Render the `:list` element of `app-state` as an undorderd list with each item corresponding
    ;; to an animal above.
    (om/component (apply dom/ul nil
                         (map (fn [text] (dom/li nil text)) (:list data)))))
  app-state
  {:target (. js/document (getElementById "app0"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
