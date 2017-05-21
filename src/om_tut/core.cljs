(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

(om/root
  (fn [data owner]
    ;; Render the `:list` element of `app-state` as an undordered list with each item corresponding
    ;; to an animal above.
    ;;
    ;; The second argument to `dom/...` is a map containing tag attributes. Because the React library
    ;; uses "className" (instead of `class`), we use the same keyword.
    ;;
    ;; Remember, the attribute "map" must *actually be* a JavaScript object - so it must be prefaced
    ;; with the reader literal `#js`. When the reader encounters this literal, it automagically 
    ;; compiles the following map into a JavaScript object. (Additionally, if `#js` preceeds a vector, 
    ;; it compiles the following vector into a JavaScript array.)
    ;;
    ;; As I discovered through "hard-won experience," neglect this literal, and the generated tag
    ;; will contain *no* attributes.
    ;;
    ;; You have been warned!
    (om/component (apply dom/ul #js {:className "animals"}
                         (map (fn [text] (dom/li nil text)) (:list data)))))
  app-state
  {:target (. js/document (getElementById "app0"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
