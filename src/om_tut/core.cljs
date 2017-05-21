(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

;; Render each list item with the specified background color
(defn stripe [text background-color]
  (let [item-style #js {:backgroundColor background-color}]
    (dom/li #js {:style item-style} text)))

(om/root
  (fn [data owner]
    ;; This component now uses the full power of the ClojureScript language to construct a
    ;; "complicated" UI without the use of templates.
    (om/component (apply dom/ul #js {:className "animals"}
                         ;; Alternate the stripes between the two colors, yellow (`#ff0`) and 
                         ;; white (`#fff`)
                         (map stripe (:list data) (cycle ["#ff0" "#fff"])))))
  app-state
  {:target (. js/document (getElementById "app0"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
