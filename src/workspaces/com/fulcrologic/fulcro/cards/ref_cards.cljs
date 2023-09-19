(ns com.fulcrologic.fulcro.cards.ref-cards
  (:require
    ["react" :as react]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.data-fetch :as df]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.inspect.preload]
    [com.fulcrologic.fulcro.inspect.inspect-client :as i]
    [com.fulcrologic.fulcro.networking.http-remote :as http]
    [com.fulcrologic.fulcro.networking.json-api-middleware :as jmw]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom :refer [div input]]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.dom.events :as evt]))

(defsc CustomInput [this {:keys [forward-ref label value onChange] :as props}]
  {}
  (dom/div :.ui.field
    (input (cond-> {:name  (str label)
                    :value (str value)}
             forward-ref (assoc :ref forward-ref)
             onChange (assoc :onChange onChange)))
    (dom/label {:htmlFor (str label)} label)))

(def ui-custom-input (comp/factory CustomInput))

(def HooksUI (comp/sc ::HooksUI {:use-hooks? true}
               (fn [this props]
                 (let [[v setv!] (hooks/use-state "")
                       input-ref (hooks/use-ref nil)]
                   (hooks/use-effect
                     (fn []
                       (let [input (.-current input-ref)]
                         (when input
                           (js/console.log input)
                           (.focus input)))

                       (fn []))
                     [(.-current input-ref)])
                   (div
                     (dom/h4 "My Form")
                     (ui-custom-input
                       {:label       "My Input"
                        :value       v
                        :forward-ref input-ref
                        :onChange    (fn [v] (setv! v))}))))))

(ws/defcard ref-hooks-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? false
     ::ct.fulcro/root       HooksUI}))

(def StdUI (comp/sc
             ::StdUI
             {:query             [:id :value]
              :ident             :id
              :initLocalState    (fn [^js this props] (set! (.-inputref this) (react/createRef)))
              :initial-state     (fn [_] {:id    42
                                          :value "Bob"})
              :componentDidMount (fn [^js this]
                                   (let [input-ref (.-current (.-inputref this))]
                                     (when input-ref
                                       (.focus input-ref))))}
             (fn render* [^js this {:keys [value]}]
               (let [input-ref (.-inputref this)]
                 (div
                   (dom/h4 "My Form")
                   (ui-custom-input
                     {:label       "My Input"
                      :value       value
                      :forward-ref input-ref
                      :onChange    (fn [v] (m/set-string!! this :value :value (evt/target-value v)))}))))))

(ws/defcard ref-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? true
     ::ct.fulcro/root       StdUI}))

(defonce app (app/fulcro-app {:remotes
                              {:json
                               (http/fulcro-http-remote {:url                 "https://date.nager.at/api/v3"
                                                         :request-middleware  (jmw/wrap-json-request)
                                                         :response-middleware (jmw/wrap-json-response)})}}))
(defsc Person [this props]
  {:query [:person/id :person/name]
   :ident :person/id})

(defsc Country [this props]
  {:query [:country/countryCode :country/name]
   :ident :country/countryCode})

(comment
  (i/app-started! app)
  (df/load! app :ignored Country
    {:params {::jmw/uri "/AvailableCountries"}
     :remote :json})

  ;; Mutations default to POST, but can be set
  (defmutation update-person [{:person/keys [id name]}]
    (json [env]
      (-> env
        (m/returning Person)
        (jmw/using-method :put)
        (jmw/using-uri "/person"))))

  ;; Sends JSON PUT with JSON body of {'id': id, 'name': name}, expects a JSON
  ;; response with the same (returning Person) and normalizes it into app state.
  (comp/transact! app [(update-person {::jmw/method :put
                                       ::jmw/uri    "/person"})]))
