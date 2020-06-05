(ns com.fulcrologic.fulcro.cards.reducer3-cards
  "An experiment of using React Hooks for *pulling* reduced data into leaf components instead of pushing it.
   Lends a number of advantages, including being able to disconnect the react elements from the normal Fulcro
   data graph."
  (:require
    ["react" :refer [createElement]]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.dom :as dom :refer [div button p ul]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [taoensso.timbre :as log]))

(defonce reducers (atom {}))

(defn register-reducer!
  [prop-key reducer]
  (swap! reducers assoc prop-key reducer))

;; Side effect to add a reducer for a given generated prop
(register-reducer! :generated/data
  (fn [app {:keys [after]}]
    (+ 42 (get-in after [:component/id ::top :x]))))

(register-reducer! :all-known-sales/total
  (fn [app {:keys [after]}]
    (let [sales (vals (:sale/id after))]
      (reduce
        (fn [acc {:sale/keys [amount]}]
          (+ acc amount))
        0
        sales))))

;; map from prop -> setters
(defonce subscribers (atom {}))

(defn register-subscriber! [prop setter!] (swap! subscribers update prop (fnil conj #{}) setter!))
(defn deregister-subscriber! [prop setter!] (swap! subscribers update prop disj setter!))
(defn push-value! [prop new-value]
  (doseq [set-value! (get @subscribers prop)]
    (set-value! new-value)))

(defn use-reduced-value [prop]
  (let [[v set-v!] (hooks/use-state nil)]
    (hooks/use-effect (fn *setup []
                        (register-subscriber! prop set-v!)
                        (fn *tear-down []
                          (deregister-subscriber! prop set-v!))) [prop])
    v))

(declare Top)

(defn reducer-tx-hook
  [app {:keys [before after] :as delta}]
  (let [{::app/keys [state-atom]} app
        generated-data (reduce-kv (fn [acc prop-key reducer]
                                    (assoc acc prop-key (reducer app delta))) {} @reducers)]
    (swap! state-atom assoc ::reductions generated-data)
    (doseq [k (keys generated-data)]
      (push-value! k (get generated-data k)))))

(defsc Sale [this {:sale/keys [amount]}]
  {:query         [:sale/id :sale/amount]
   :ident         :sale/id
   :initial-state {:sale/id :param/id :sale/amount :param/amount}}
  (div "Sale: " amount))

(def ui-sale (comp/factory Sale {:keyfn :sale/id}))

(defsc Child [this {:child/keys [id name sales] :as props}]
  {:query         [:child/id :child/name {:child/sales (comp/get-query Sale)}]
   :ident         :child/id
   :initial-state {:child/id    :param/id
                   :child/sales :param/sales
                   :child/name  :param/name}
   :use-hooks?    true}
  (let [data (use-reduced-value :generated/data)]
    (div
      (p (str name ", with generated data: " data))
      (ul
        (map ui-sale sales)))))

(def ui-child (comp/factory Child {:keyfn :child/id}))

(defn add-sale* [state-map child-id amount]
  (merge/merge-component state-map Sale {:sale/id (random-uuid) :sale/amount amount}
    :append [:child/id child-id :child/sales]))

(defmutation new-sale [{:keys [amount]}]
  (action [{:keys [state]}]
    (swap! state add-sale* 2 amount)))

(defn LeafNode []
  (let [data (use-reduced-value :generated/data)]
    (div "Leaf node with subscription: " (str data))))

(def ui-leaf-node #(createElement LeafNode))

(defsc Top [this {:keys [x children] :as props}]
  {:query         [:x {:children (comp/get-query Child)}
                   [::reductions :all-known-sales/total]]
   :ident         (fn [] [:component/id ::top])
   :initial-state {:x        1
                   :children [{:id 1 :name "Joe" :sales [{:id 1 :amount 11.95}]}
                              {:id 2 :name "Sam" :sales [{:id 2 :amount 10.05}]}
                              {:id 3 :name "Sally" :sales [{:id 3 :amount 2.05} {:id 4 :amount 2.45}]}
                              {:id 4 :name "Barbara" :sales []}]}}
  (div
    (p "All known sales total: " (get props [::reductions :all-known-sales/total]))
    (ui-leaf-node)
    (button {:onClick (fn [] (m/set-integer! this :x :value (inc x)))} (str "Bump x " x))
    (button {:onClick (fn [] (comp/transact! this [(new-sale {:amount (rand-int 10)})]))} "Sell something!")
    (mapv ui-child children)))

(ws/defcard reducer3-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root?      true
     ::ct.fulcro/root            Top
     ::ct.fulcro/persistence-key ::app
     ;; NOTE: Just invented tx-hook...simple add to tx-processing
     ::ct.fulcro/app             {:tx-hook reducer-tx-hook}}))
