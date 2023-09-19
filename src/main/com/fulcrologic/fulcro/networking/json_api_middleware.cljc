(ns com.fulcrologic.fulcro.networking.json-api-middleware
  "Support for using low-level HTTP/JSON network APIs with Fulcro http remotes. These middleware functions
   give a way to acurately send the correct requests to the server, transform the response into fulcro-compatible (and
   normalizable) shape so that using such APIs are seamless from `df/load!` and mutations.

   Install a fulcro-http-remote with this as middleware:

   (app/fulcro-app
     {:remotes
       {:json
        (http/fulcro-http-remote {:url \"https://example.com/api\"
                                  :request-middleware (wrap-json-request)
                                  :response-middleware (wrap-json-response)})}})

   (defsc Person [this props]
     {:query [:person/id :person/name]
      :ident :person/id}
     ...)

   ;; Load and normalize a person from an HTTP GET using JSON
   ;; Sends https://example.com/api/person?id=42
   ;; and normalizes the response {'id': 42, 'name': 'Bob'}
   ;; targeting and such all works.
   (df/load! app :ignored/person Person
     {:params {::jmw/uri \"/person\"
               :id 42}
      :remote :json})

   ;; Mutations default to POST, but can be set
   (defmutation update-person [{:person/keys [id name]}]
     (action [env] ..normal local change...)
     (json [env]
       (-> env
         (m/returning Person)
         (jmw/using-method :put)
         (jmw/using-uri \"/person\"))))

   ;; Sends JSON PUT with JSON body of {'id': id, 'name': name}, expects a JSON
   ;; response with the same (returning Person) and normalizes it into app state.
   (comp/transact this [(update-person {::jmw/method :put
                                        ::jmw/uri \"/person\"
                                        :person/name \"Jill\"})])
   "
  (:require
    #?(:clj
       [clojure.data.json :as json])
    [clojure.set :as set]
    [clojure.string :as str]
    [edn-query-language.core :as eql]
    [taoensso.encore :as enc]
    [taoensso.timbre :as log]))

(defn- mutation? [{:keys [params
                          type
                          children] :as ast-node}]
  (or (= type :call)
    (= :call (some-> children first :type))))

(defn jsonify [edn]
  #?(:cljs (.stringify js/JSON (clj->js edn))
     :clj  (json/write-str edn)))
(defn parse-json-string [s]
  #?(:clj
     (json/read-str s)
     :cljs
     (js->clj (.parse js/JSON s))))

(defn edn->json-api-request
  "Returns a map with:

  :body <json string> (if any)
  :method :get|:post|...
  :query-string \"?...\" - if the method is :get, and there are parameters to the load/mutation.
  "
  [edn]
  (let [{:keys [params] :as n} (eql/query->ast1 edn)
        {::keys [method uri]} params
        method       (or method (if (mutation? n) :post :get))
        params       (->> params
                       (enc/filter-keys #(not= "com.fulcrologic.fulcro.networking.json-api-middleware"
                                           (namespace %)))
                       (enc/map-keys name))
        q            (when (= :get method) (enc/format-query-string params))
        body         (if (= :get method)
                       ""
                       (jsonify params)
                       )
        query-string (when (seq q) (str "?" q))]
    (cond-> {:method method
             :body   body}
      uri (assoc :uri uri)
      query-string (assoc :query-string query-string))))

(defn handle-json-request
  "Client Remote Middleware to convert queries and mutations into low-level HTTP get/post/etc. JSON-based
   requests.

  See namespace docstring.
  "
  ([handler {eql   :body
             :keys [url headers] :as request}]
   (let [{:keys [uri method query-string body]} (edn->json-api-request eql)
         response-type ""                                   ; xhrio response type (DEFAULT)
         url           (cond
                         (and
                           (string? uri)
                           (str/starts-with? uri "/")) (str url uri query-string)
                         (and
                           (string? uri)
                           (str/starts-with? uri "http")) (str uri query-string))
         headers       (assoc headers
                         "Content-Type" "application/json"
                         "Accept" "application/json")]
     (handler (merge request {:body          body
                              :original-eql  eql
                              :url           url
                              :headers       headers
                              :method        method
                              :response-type response-type})))))

(defn wrap-json-request
  "Fulcro HTTP remote middleware for converting Fulcro EQL loads/mutations into HTTP JSON requests.

  See namespace docstring."
  ([] (wrap-json-request identity))
  ([handler]
   (fn [req] (handle-json-request handler req))))

(defn json-key-map
  "For an EQL query `q` (which is a vector) return a map whose keys are the expected JSON strings
   in a result, and whose values are the desired EDN keys."
  [{:keys [children] :as ast-node}]
  (into {}
    (map (juxt (comp name :key) :key))
    children))

(defn rename-json-keys [query plain-edn]
  (let [ast (eql/query->ast query)
        km  (json-key-map ast)]
    (cond
      (vector? plain-edn)
      (mapv #(rename-json-keys query %) plain-edn)

      (map? plain-edn)
      (let [{:keys [children]} ast
            k->ast-node (zipmap (map :key children) children)]
        (reduce-kv
          (fn [m plain-k value]
            (enc/if-let [k (get km plain-k)
                         {:keys [type query]} (k->ast-node k)]
              (if (= :join type)
                (assoc m k (rename-json-keys query value))
                (assoc m k value))
              m))
          {}
          plain-edn))

      :else plain-edn)))

(defn json-str->query-response [query json-string]
  (let [plain-edn (parse-json-string json-string)]
    (rename-json-keys query plain-edn)))

(defn handle-json-response [handler {:keys [body error outgoing-request] :as response}]
  (enc/catching
    (let [{:keys [dispatch-key query]} (eql/query->ast1 (:original-eql outgoing-request))
          fulcro-response (json-str->query-response query body)]
      (handler (assoc response :body {dispatch-key fulcro-response})))
    e
    (do
      (log/error e "handle-json-response threw an unexpected exception")
      (assoc response :body {} :status-code 417))))

(defn wrap-json-response
  ([] (wrap-json-response identity))
  ([handler]
   (fn [req] (handle-json-response handler req))))

(comment
  (:query (eql/query->ast1 `[({:ignored [:country/countryCode :country/name]} {:com.fulcrologic.fulcro.networking.json-api-middleware/uri "/AvailableCountries"})]))
  (:query (eql/query->ast1 `[{(:foo {:a 1}) [:bar]}]))
  )
