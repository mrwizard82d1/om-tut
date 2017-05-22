(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

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

(defn middle-name [{:keys [middle middle-initial]}]
  (cond 
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn contact-view [contact owner]
  ;; We change the interface we render from `om/IRender` to `om/IRenderState`. The `IRender` is incapable
  ;; of receiving component state; `IRenderState`, however, can receive component state from the React
  ;; framework.
  (reify 
    om/IRenderState
    ;; `render-state` receives the component state as its second argument. We extract the `delete` value
    ;; (which is a `core.async` channel).
    (render-state [this {:keys [delete]}]
      (dom/li nil
              (dom/span nil (display-name contact))
              ;; The `onClick` event puts the contact to delete on the channel. (I'm uncertain the
              ;; reason that we dereference `contact` in the call to `put!`.)
              (dom/button #js {:onClick (fn [e] (put! delete @contact))} 
                          "Delete")))))

(defn contacts-view [data owner]
  ;; I think of `data` as application state (per React); however, based on the documentation of
  ;; `om/transact!`, I believe data, in the Om world, is actually a cursor into the application 
  ;; state. I need to understand cursors more. From the documentation, Om application keep their 
  ;; application state in a single atom (think "Redux store"). However, just like with Redux 
  ;; stores, individual components only care about a *portion* of the application state. An Om 
  ;; `Cursor` models a portion of the application state. The implementation details of a `Cursor`
  ;; involve packaging the reference to the application state with a path to the component(s) of
  ;; interest (a vector of keys into the application state).
  (reify
    om/IInitState
    (init-state [_]
      ;; The initial state of the component is a map containing a `core.async/chan(nel)` as the value
      ;; of the `:delete` key. 
      ;;
      ;; The tutorial explanation emphasizes that we *must not* allocate the channel used for
      ;; used for communication in a `let` binding *outside* reify. It explains that the function
      ;; `contacts-view` (and *any* component) may be invoked many, many times. If we allocated the
      ;; channel in a `let` binding, the component would *not* be idempotent. (It would depend on 
      ;; data allocated *outside* the function. I have a vague grasp of this issue; however, I
      ;; will probably need to encounter the problems it causes "in the wild" to fully appreciate
      ;; it. Sigh...
      {:delete (chan)})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        ;; When I'm about to mount this component, I create an infinite loop that attempts to take the 
        ;; contact to delete from the `delete` channel (parking / blocking if none exist) and then 
        ;; deleting that contact from the application state (`data`).
        (go
          (loop []
            (let [contact (<! delete)]
              ;; The function `om/transact!` is the primary function for changing application state.
              ;; This function takes a cursor identifying the state affected by the change, an optional
              ;; "korks" (key or sequence of keys) further identifying the portion of application state 
              ;; affected, and a function updating that (small) portion of the state.
              (om/transact! data 
                            :contacts
                            ;; We *must* use `vec` to convert the lazy sequence returned by `remove`
                            ;; to a fully realized sequence. (We can only store realized sequences
                            ;; in our application state.
                            (fn [xs]
                              (vec (remove #(= contact %) xs))))
              (recur))))))
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/div nil
               (dom/h2 nil "Contact list")
               (apply dom/ul 
                      nil
                      ;; `om/build-all` takes an optional third argument, a map of additional (but 
                      ;; limited) options. The `:init-state` option expects a map describing the 
                      ;; initial state to set on the component. (The documantion of `:init-state`
                      ;; indicates that the state returned from the `om/IInitState` protocol 
                      ;; implementation is merged *into* this component state. I do not 
                      ;; understand this description / operation. It seems to be a "no-op." Hmmm.
                      (om/build-all contact-view 
                                    (:contacts data)
                                    {:init-state {:delete delete}}))))))

(om/root contacts-view
         app-state
         {:target (. js/document (getElementById "contacts"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
