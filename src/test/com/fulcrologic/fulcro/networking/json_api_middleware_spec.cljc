(ns com.fulcrologic.fulcro.networking.json-api-middleware-spec
  (:require
    [com.fulcrologic.fulcro.networking.json-api-middleware :as jmw]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]
    [fulcro-spec.core :refer [=> assertions component specification]]))

(specification "Converting EDN to a JSON Request"
  (component "For queries"
    (component "Without parameters"
      (let [{:keys [method body query-string]} (jmw/edn->json-api-request [{:root [:foo/bar]}])]
        (assertions
          "Uses :get"
          method => :get
          "Leaves the body empty"
          body => ""
          "Has no query string"
          query-string => nil))
      (let [{:keys [method uri]} (jmw/edn->json-api-request `[{(:root {::jmw/uri    "/boo"
                                                                       ::jmw/method :x}) [:foo/bar]}])]
        (assertions
          "Can override the method"
          method => :x
          "Can specify a custom uri"
          uri => "/boo")))
    (component "With parameters"
      (let [{:keys [query-string]} (jmw/edn->json-api-request `[{(:root {:x/a 1 :foo/bar "hello there!"}) [:foo/bar]}])]
        (assertions
          "Properly encode parameters as a query string"
          query-string => #?(:cljs "?a=1&bar=hello%20there!"
                             :clj  "?a=1&bar=hello%20there%21")))))
  (component "For mutations"
    (component "Without parameters"
      (let [{:keys [uri method body query-string]} (jmw/edn->json-api-request `[(f {::jmw/uri "/f"})])]
        (assertions
          "Honors uri"
          uri => "/f"
          "Uses :post"
          method => :post
          "Makes the body empty JSON object"
          body => "{}"
          "Has no query string"
          query-string => nil)))
    (component "With parameters"
      (let [{:keys [body]} (jmw/edn->json-api-request `[(f {:x/a 1})])]
        (assertions
          "encodes parameters into JSON"
          body => "{\"a\":1}")))))

(specification "handle-json-request"
  (component "For a query with a relative URI"
    (let [{:keys [url headers]} (jmw/handle-json-request identity {:url  "http://default"
                                                                   :body `[{(:x {::jmw/uri "/foo"}) [:y]}]})]
      (assertions
        "Sets JSON in headers"
        headers => {"Content-Type" "application/json", "Accept" "application/json"}
        "Uses the URL from the request object for relative URLs"
        url => "http://default/foo")))
  (component "For a query with an absolute URI"
    (let [{:keys [url headers]} (jmw/handle-json-request identity
                                  {:url  "http://default"
                                   :body `[{(:x {::jmw/uri "http://g.com/foo"}) [:y]}]})]
      (assertions
        "Uses the abs URL"
        url => "http://g.com/foo"))))

(specification "json-key-map" :focus
  "Can create a map from json key to expected EDN key"
  (assertions
    (jmw/json-key-map (eql/query->ast [:foo/bar :boo/Bah])) => {"bar" :foo/bar
                                                                "Bah" :boo/Bah}))

(specification "json-str->query-response"
  (component "To-one"
    (assertions
      "Can convert plain keys to their expected EQL keys."
      (jmw/json-str->query-response [:person/name {:person/address [:address/city]}]
        (jmw/jsonify {:name    "Bob"
                      :address {:city "NY"}}))
      =>
      {:person/name    "Bob"
       :person/address {:address/city "NY"}}

      "Ignores keys that were not in the original query"
      (jmw/json-str->query-response [:person/name {:person/address [:address/city]}]
        (jmw/jsonify {:name    "Bob"
                      :age     55
                      :size    42
                      :address {:city "NY"}}))
      =>
      {:person/name    "Bob"
       :person/address {:address/city "NY"}}))
  (component "To-many"
    (assertions
      "Can convert plain keys to their expected EQL keys."
      (jmw/json-str->query-response [:person/name {:person/address [:address/city]}]
        (jmw/jsonify [{:name    "Bob"
                       :address {:city "NY"}}
                      {:name    "Alice"
                       :address [{:city "LA"} {:city "Fra"}]}]))
      =>
      [{:person/name    "Bob"
        :person/address {:address/city "NY"}}
       {:person/name    "Alice"
        :person/address [{:address/city "LA"}
                         {:address/city "Fra"}]}])))
