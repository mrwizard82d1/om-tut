;; Remember that one *must* re-run `lein figwheel` when changing the top namespace form.
(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/om-tut/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state
  (atom {:contacts [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
                    {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
                    {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
                    {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
                    {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
                    {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]}))

(defn parse-contact [contact-str]
  (let [[;; Split the contact into three pieces: first, middle and last
         first middle last :as parts] (string/split contact-str #"\s+")
        ;; If contact only split into *two* components (no `last`), exchange `last` and `middle`
        [first last middle] (if (nil? last)
                              [first middle]
                              [first last middle])
        ;; Remove the "." from the middle initial
        middle (when middle (string/replace middle "." ""))
        ;; Remember the size of the middle (distinguishes between "middle name" and "middle initial")
        c (if middle 
            (count middle) 
            0)]
    ;; If the contact has at least two parts (otherwise, return `nil`)
    (when (>= (count parts) 2)
      ;; I must have a first and last name
      (cond-> {:first first :last last}
        ;; If `middle` has only a single character, add a `:middle-initial` item to the returned value
        (== c 1) (assoc :middle-initial middle)
        ;; If `middle` has 2 or more characters, add a `middle` item to the returned value
        (>= c 2) (assoc :middle middle)))))

(defn middle-name [{:keys [middle middle-initial]}]
  (cond 
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn add-contact [data owner]
  ;; The new contact is the value of parsing the value of the "new-contact" component
  (let [new-contact (-> (om/get-node owner "new-contact")
                        .-value
                        parse-contact)]
    ;; If I have a new contact
    (when new-contact
      ;; Add it to the application state by conjoining the new contact to the list of contacts
      (om/transact! data :contacts #(conj % new-contact))
      ;; And clear the input field when successfully added
      (om/set-state! owner :text ""))))

(defn contact-view [contact owner]
  (reify 
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
              (dom/span nil (display-name contact))
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} 
                          "Delete")))))

(defn contains-digit?
  "Returns a 'truthy' value if `to-test` contains a digit."
  [to-test]
  (re-find #"[0-9]" to-test))

;; I do not understand why we extract the `:text` component of the state.
(defn handle-change [e owner {:keys [text]}]
  ;; Remember, the `..` function translates to the JavaScript expression `e.target.value`
  (let [new-value (.. e -target -value)]
    (if-not (contains-digit? new-value)
      (om/set-state! owner :text new-value)
      ;; The following expression is *not* needed in React; however, as the tutorial explains,
      ;; "...React's internals [clash] a bit with Om's optimization of always rendering on
      ;; `requestAnimationFrame`.
      (om/set-state! owner :text text))))

(defn contacts-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
      ;; The initial state of the component is a map containing two pieces: a channel to communicate
      ;; delete messages and the (unparsed) text of the new contact to add
      {:delete (chan)
       :text ""})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go-loop [contact (<! delete)]
          (om/transact! data
                        :contacts
                        (fn [xs]
                          (vec (remove #(= contact %) xs))))
          (recur (<! delete)))))
    om/IRenderState
    (render-state [this state]
      (dom/div nil
               (dom/h2 nil "Contact list")
               (apply dom/ul 
                      nil
                      (om/build-all contact-view 
                                    (:contacts data)
                                    {:init-state state}))
               (dom/div nil
                        ;; Additionally, the `:text` element of `state` contains the (unparsed) value 
                        ;; of the new contact to be added. (Note that we *always* overwrite whatever
                        ;; the user types with this value. :) )
                        (dom/input #js {:type "text"
                                        :ref "new-contact"
                                        :value (:text state)
                                        ;; Responds to `onChange` event by handling the change event.
                                        :onChange #(handle-change % owner state)})
                        (dom/button #js {:onClick #(add-contact data owner)} 
                                    "Add contact"))))))

(om/root contacts-view
         app-state
         {:target (. js/document (getElementById "contacts"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
