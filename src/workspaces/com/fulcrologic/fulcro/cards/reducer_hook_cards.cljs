(ns com.fulcrologic.fulcro.cards.reducer-hook-cards
  (:require
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.rendering.multiple-roots-renderer :as mroot]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.mutations :as m]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.react.hooks :as hooks]))

;; Map from prop -> reducer that can generate that prop from state
(defonce reducers (atom {}))

(defn register-reducer!
  "Register a global prop name as something that is derived via a function of the db. The `reducer` is that function,
   which must be a `(fn [app change] value-of-registered-prop)` where `change` is a map that will at least contain
   a :before and :after key of the state before and after the last thing that happened.
   Side-effecting reducers (ones that load or invoke further mutations) are still a research effort, but might be ok???"
  [prop-key reducer]
  (swap! reducers assoc prop-key reducer))

;; Side effect to add a reducer for a given generated prop
(register-reducer! :generated/data (fn [app {:keys [after]}]
                                     (+ (get-in after [:component/id ::top :x])
                                       (get-in after [:component/id ::top :y]))))

(defn reducer-tx-hook
  "A transaction hook that looks at mounted components to see what registered props need to be re-generated, and then
   updates that data on the components that are mounted and are asking for it."
  [app {:keys [before after] :as delta}]
  ;; 1. Fulcro has an index for all of the component classes that query for a given prop
  (let [{::app/keys [state-atom runtime-atom]} app
        {::app/keys [indexes]} @runtime-atom
        {:keys [prop->classes class->components]} indexes
        ;; 2. our reducers registry has all of the generated prop names. We could optimize this to only update the ones that
        ;; have mounted instances...but let's leave that for later optimizations.
        ;; For now: we'll just calculate ALL of our generated data:
        generated-data (reduce-kv (fn [acc prop-key reducer]
                                    (assoc acc prop-key (reducer app delta))) {} @reducers)]
    ;; Now we use the indexes to update the instances that want the data
    (log/info "Running reducer tx hook")
    (swap! state-atom
      (fn [state-map]
        (reduce
          (fn [state1 prop]
            (let [affected-classes    (log/spy :info (prop->classes prop))
                  affected-components (reduce (fn [acc cls] (into acc (class->components cls)))
                                        #{}
                                        affected-classes)]
              (reduce (fn [state2 c]
                        (let [path (log/spy :info (conj (comp/get-ident c) prop))]
                          (assoc-in state2 path (get generated-data prop))))
                state1
                affected-components)))
          state-map
          (keys generated-data))))))

(defsc Child [this {:generated/keys [data]
                    :child/keys     [id name] :as props}]
  {:query         [:child/id :child/name :generated/data]
   :ident         :child/id
   :initial-state {:child/id :param/id :child/name :param/name}}
  (dom/div
    (dom/p "Name:" name)
    (dom/p "Data:" data)))

(def ui-child (comp/factory Child {:keyfn :child/id}))

(defsc Top [this {:keys [x children generated/data] :as props}]
  {:query         [:x :y {:children (comp/get-query Child)} :generated/data]
   :ident         (fn [] [:component/id ::top])
   :initial-state {:x        1
                   :y        3
                   :children [{:id 1 :name "Joe"}]}}
  (dom/div
    (dom/p "Generated data: " data)
    (dom/button {:onClick (fn [] (m/set-integer! this :x :value (inc x)))} "Bump x")
    (mapv ui-child children)))

(ws/defcard floating-reducer-hook-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root?      true
     ::ct.fulcro/root            Top
     ::ct.fulcro/persistence-key ::app
     ;; NOTE: Just invented tx-hook...simple add to tx-processing
     ::ct.fulcro/app             {:tx-hook reducer-tx-hook}}))
