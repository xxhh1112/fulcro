(ns fulcro-todomvc.main
  (:require
    [fulcro-todomvc.ui :as ui]
    [com.fulcrologic.fulcro.rendering.keyframe-render2 :as kr2]
    [com.fulcrologic.fulcro.offline.browser-edn-store :as browser-store]
    [com.fulcrologic.fulcro.offline.load-cache :as load-cache]
    [com.fulcrologic.fulcro.offline.durable-mutations :as durable-mutations]
    [com.fulcrologic.fulcro.offline.tempid-strategy :as tempid-strategy]
    [com.fulcrologic.fulcro.networking.http-remote :as http]
    [com.fulcrologic.fulcro.networking.websockets :as fws]
    [com.fulcrologic.fulcro.algorithms.timbre-support :refer [console-appender prefix-output-fn]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.data-fetch :as df]
    [taoensso.timbre :as log]))

(defonce remote #_(fws/fulcro-websocket-remote {:auto-retry?        true
                                                :request-timeout-ms 10000}) (http/fulcro-http-remote {}))

(defonce edn-store (browser-store/browser-edn-store "todomvc-store"))

(defonce app (-> (app/fulcro-app {:remotes          {:remote remote}
                                  :client-did-mount (fn [_]
                                                      (log/merge-config! {:output-fn prefix-output-fn
                                                                          :appenders {:console (console-appender)}}))})
               (load-cache/with-load-cache edn-store)
               (durable-mutations/with-durable-mutations edn-store (tempid-strategy/->TempIDisRealIDStrategy))))

(comment (::app/config app))

(defn start []
  (app/mount! app ui/Root "app")
  ;; Load the todo list, using an empty default, but coming from cache it network is down.
  (load-cache/load! app [:list/id 1] ui/TodoList []))

(defn ^:dev/after-load reload []
  (app/mount! app ui/Root "app"))

(comment
  (app/set-root! app ui/Root {:initialize-state? true})
  (app/mounted? app)
  (df/load! app [:list/id 1] ui/TodoList)
  (app/mount! app ui/Root "app" {:initialize-state? false})
  @(::app/state-atom app)
  (fws/stop! remote)
  )
