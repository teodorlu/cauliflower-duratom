(ns torque
  (:require [ring.util.codec]))

;; ── commands ──────────────────────────────────────────────────────────────

(defn invoke
  "Create a Datastar expression to invoke the given command

  Invoke the #'increase command on click:

      {:data-on:click (invoke #'increase)}

  Invoke the #'plus command with parameter delta=5:

      {:data-on:click (invoke #'plus {\"delta\" 5})}

  Invoke the #'plus command and disable the input element:

      {:data-on:click (invoke #'plus nil {:disable true})}"
  ([command-var]
   (invoke command-var nil nil))
  ([command-var command-params]
   (invoke command-var command-params nil))
  ([command-var command-params {:keys [disable]}]
   (->> [(when disable "el.disabled = true;")
         (str "@post('" (-> command-var meta :command/uri)
              (when command-params (str "?" (ring.util.codec/form-encode command-params)))
              "')")]
        (filterv some?))))

(defn commands-by-uri [command-vars]
  (let [command-map (into {} (map (juxt (comp :command/uri meta) identity)) command-vars)]
    (when (not= (count command-vars) (count command-map))
      (throw (ex-info "Invalid commands, overlapping URI"
                      {:conflicts (->> command-vars
                                       (group-by (comp :command/uri meta))
                                       (remove (fn [[_uri vars]]
                                                 (<= 2 (count vars))))
                                       (into {}))})))
    command-map))

;; ── server ────────────────────────────────────────────────────────────────

(require '[org.httpkit.server :as httpkit])

(defonce !server (atom nil))

(defn halt-server! []
  (swap! !server #(when % (httpkit/server-stop! %))))

(defn serve! [handler opts]
  (halt-server!)
  (reset! !server (httpkit/run-server handler
                                      (merge {:port 7777}
                                             opts
                                             {:legacy-return-value? false}))))
