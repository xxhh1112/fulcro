(ns com.fulcrologic.fulcro.cards.subscription-cards
  "An experiment of using React Hooks for *pulling* reduced data into leaf components instead of pushing it.
   Lends a number of advantages, including being able to disconnect the react elements from the normal Fulcro
   data graph."
  (:require
    ["react" :refer [createElement]]
    [com.fulcrologic.fulcro.rendering.keyframe-render2 :as kr2]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.dom :as dom :refer [div button p ul]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]))

;; workspaces workaround
(defonce app-atom (atom nil))

;; map from query -> set of setters for subscribers
(defonce subscribers (atom {}))

(defn register-subscriber! [query setter!] (swap! subscribers update query (fnil conj #{}) setter!))
(defn deregister-subscriber! [query setter!] (swap! subscribers update query disj setter!))
(defn push-value! [query new-value]
  (doseq [set-value! (get @subscribers query)]
    (set-value! new-value)))

(defn use-query-subscription [query]
  (let [[v set-v!] (hooks/use-state nil)]
    (hooks/use-effect (fn *setup []
                        (register-subscriber! query set-v!)
                        (when @app-atom
                          (let [state-map     (app/current-state @app-atom)
                                initial-value (fdn/db->tree query state-map state-map)]
                            (set-v! initial-value)))
                        (fn *tear-down []
                          (deregister-subscriber! query set-v!)))
      ;; have to use the hash of the query to prevent re-register on each render, because it is
      ;; query is built on each render and will not be identical from js perspective
      [(hash query)])
    v))

(declare Top)

(defn render!
  "Custom renderer that uses normal kf2/render!, but also sends updates to query subscribers."
  [app options]
  (kr2/render! app options)
  (let [state-map (app/current-state app)]
    (doseq [query (keys @subscribers)
            :let [setters (get @subscribers query)]]
      (when (seq setters)
        (let [result (fdn/db->tree query state-map state-map)]
          (doseq [send! setters]
            (send! result)))))))

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
                   :child/name  :param/name}}
  (div
    (p (str name))
    (ul
      (map ui-sale sales))))

(def ui-child (comp/factory Child {:keyfn :child/id}))

(defn add-sale* [state-map child-id amount]
  (merge/merge-component state-map Sale {:sale/id (random-uuid) :sale/amount amount}
    :append [:child/id child-id :child/sales]))

(defmutation new-sale [{:keys [amount]}]
  (action [{:keys [state]}]
    (swap! state add-sale* 2 amount)))

(defn use-child-sales
  "Composition of query into more reusable form"
  [child-id]
  (let [child-ident  [:child/id child-id]
        query-result (use-query-subscription [{child-ident (comp/get-query Child)}])
        sales        (get-in query-result [child-ident :child/sales])
        total        (reduce + 0 (map :sale/amount sales))]
    total))

;; A plain function, using React hooks
(defn LeafNode []
  (let [total (use-child-sales 2)]
    (div "Subscriber to Sam's Sales: " total)))

;; Plain react function as element: plain react createElement
(def ui-leaf-node #(createElement LeafNode))

(defsc Top [this {:keys [x children]}]
  {:query         [:x {:children (comp/get-query Child)}]
   :ident         (fn [] [:component/id ::top])
   :initial-state {:x        1
                   :children [{:id 1 :name "Joe" :sales [{:id 1 :amount 11.95}]}
                              {:id 2 :name "Sam" :sales [{:id 2 :amount 10.05}]}
                              {:id 3 :name "Sally" :sales [{:id 3 :amount 2.05} {:id 4 :amount 2.45}]}
                              {:id 4 :name "Barbara" :sales []}]}
   :use-hooks?    true}
  (let [sally-sales (use-child-sales 3)]
    (div
      (ui-leaf-node)
      (button {:onClick (fn [] (comp/transact! this [(new-sale {:amount (rand-int 10)})]))} "Sell something!")
      (div (str "Sally has sold: " sally-sales))
      (mapv ui-child children))))

(ws/defcard subscription-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root?      true
     ::ct.fulcro/root            Top
     ::ct.fulcro/persistence-key :app
     ::ct.fulcro/app             {:client-did-mount  (fn [app] (reset! app-atom app))
                                  :optimized-render! render!}}))
