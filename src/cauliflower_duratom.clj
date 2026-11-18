(ns cauliflower-duratom
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [duratom.core]
   [hiccup2.core :as h]
   [nextjournal.garden-id :as garden-id]
   [nextjournal.garden-email :as garden-email]
   [org.httpkit.server :as httpkit]
   [ring.middleware.session]
   [ring.util.codec]
   [torque :refer [invoke]])
  (:import
   (java.time DayOfWeek LocalDate)
   (java.time.temporal TemporalAdjusters)))

(def availability {:monday [{:start 9 :end 10}
                            {:start 12 :end 15}]
                   :wednesday [{:start 8 :end 10}
                               {:start 12 :end 14}]})

(def owner-email (or (System/getenv "GARDEN_OWNER_EMAIL") "my-email@example.com"))

;; ── date stuff ────────────────────────────────────────────────────────────

(defn now [] (str (LocalDate/now)))

(defn before? [a b]
  (.isBefore (LocalDate/parse a) (LocalDate/parse b)))

(defn start-of-week [date]
  (str (LocalDate/.with (LocalDate/.plusDays (LocalDate/parse date) 1)
                        (TemporalAdjusters/previous DayOfWeek/MONDAY))))

(defn offset-week [date offset]
  (str (LocalDate/.plusWeeks (LocalDate/parse date) offset)))

(defn dates-of-week [start-date]
  (map (fn [offset] (str (LocalDate/.plusDays (LocalDate/parse start-date) offset))) (range 7)))

(defn date->keyword [day]
  (keyword (str/lower-case (str (LocalDate/.getDayOfWeek (LocalDate/parse day))))))

(comment
  (before? "2024-09-16" "2024-09-17") ;=> true
  (before? "2024-09-17" "2024-09-16") ;=> false
  (dates-of-week (start-of-week (now)))
  (start-of-week (now))
  (offset-week (start-of-week (now)) 1)
  (dates-of-week (offset-week (start-of-week (now)) 1))
  (date->keyword "2024-01-01") ;=> :monday
  ,)

;; ── duratom ───────────────────────────────────────────────────────────────

(def duratom
  (duratom.core/duratom :local-file
                        :file-path (str (System/getenv "GARDEN_STORAGE") "/duratom.edn")
                        :commit-mode :sync
                        :init {}))
;; shape:
{:state/per-user {"teodorlu" {:week "2026-11-09"}}
 :state/appointments {"2026-10-26" {9 "teodor@local"
                                    12 "teodor@local"
                                    13 "teodor@local"
                                    14 "teodor@local"}
                      "2026-11-11" {8 "teodor@local"
                                    12 "teodor@local"}}}

;; ── system ────────────────────────────────────────────────────────────────

(def dev?
  (nil? (System/getenv "GARDEN_TOKEN")))

(defn print-exception [{:exception/keys [origin data message]}]
  (println (str "Exception at: " origin
                "\n  message: " message
                "\n  data:"
                "\n    " (if (map? data)
                           (str/join "\n    " (for [[k v] data] (str (pr-str k) " " (pr-str v))))
                           (pr-str data)))))

(defonce !system
  (atom {:httpkit/server nil
         :datastar/connections {}
         :duratom/duratom duratom
         :exception/report-fn #'print-exception}))

(comment
  @(:duratom/duratom @!system)
  (swap! !system assoc :exception/report-fn #'print-exception)
  (system->state @!system)
  ,)

(defn report-exception!
  ([origin e] (report-exception! origin e nil))
  ([origin e extra-data]
   (when-let [report-fn (:exception/report-fn @!system)]
     (let [message (ex-message e)
           data (ex-data e)]
       (report-fn {:exception/origin origin
                   :exception/message message
                   :exception/data (merge extra-data data)})))))

(defn system->state [system]
  (-> (select-keys @(:duratom/duratom system) [:state/per-user :state/appointments])
      (assoc :world/today (now))))

;; ── command declaration for render fns  ───────────────────────────────────

(declare next-week prev-week
         book-appointment unbook-appointment
         clear-appointments)

;; ── app & render ──────────────────────────────────────────────────────────

(defn find-appointment [state date hour]
  (get-in state [:state/appointments date hour]))
#_(find-appointment state "2026-11-16" 9)

(defn free? [state availability date hour]
  (and (some (fn [{:keys [start end]}] (<= start hour end))
             (availability (date->keyword date)))
       (not (find-appointment state date hour))))
#_(free? state {:monday [{:start 12 :end 15}]} "2024-01-01" 15)

(defn render-day [state date]
  [:div
   [:strong (str (name (date->keyword date)))]
   [:span " (" date ")"]
   (for [hour (mapcat #(range (:start %) (:end %)) (availability (date->keyword date)))
         :let [status (cond (free? state availability date hour) :free

                            (= (find-appointment state date hour)
                               (-> state :session/login :user/email))
                            :mine

                            :else nil)]
         :when status] ; :free or :mine
     [:div
      [:input {:type "checkbox"
               :checked (= status :mine)
               :data-on:change
               (if (= status :free)
                 (invoke #'book-appointment {"date" date "hour" hour} {:disable true})
                 (invoke #'unbook-appointment
                         {"date" date
                          "hour" hour}
                         {:disable true}))}]
      (str hour ":00")])])

(defn render-week [state start-of-week]
  [:div#week
   [:div.grid
    (for [date (dates-of-week start-of-week)
          :when (and (availability (date->keyword date))
                     (not (before? date (:world/today state))))]
      (render-day state date))]
   [:div {:role "group" :style "margin-top: 1em"}
    [:button.outline (if (or (before? start-of-week (:world/today state))
                             (= start-of-week (:world/today state)))
                       {:disabled true}
                       {:data-on:click (invoke #'prev-week nil {:disable true})})
     "← prev week"]
    [:button.outline {:data-on:click (invoke #'next-week nil {:disable true})}
     "next week →"]]])

(defn find-current-week [state user]
  (get-in state [:state/per-user (:user/username user) :week]))

(defn count-appointments [state]
  (transduce (comp (map val) (map count)) + 0 (:state/appointments state)))

(defn index [state]
  (let [user (:session/login state)
        username (:user/username user)]
    [:main#app.container
     [:p (if username
           [:span (str username " ") [:a {:href garden-id/logout-uri} "(logout)"]]
           [:a {:href garden-id/login-uri} "login"])]
     (when username
       (render-week state (or (find-current-week state user)
                              (start-of-week (:world/today state)))))
     (when dev?
       [:p (str (count-appointments state)) " appointments. "
        [:button {:data-on:click (invoke #'clear-appointments nil {:disable true})} "Clear appointments"]])
     [:p "Source: " [:a {:href "https://github.com/teodorlu/cauliflower-duratom"}
                     "github.com/teodorlu/cauliflower-duratom"]]]))

(def routes
  {[:get "/"] #'index})

;; ── fat morph ─────────────────────────────────────────────────────────────

(declare req->state)

(defmethod print-method org.httpkit.server.AsyncChannel [ch ^java.io.Writer w]
  (.write w "#httpkit/channel ")
  (print-method (str ch) w))

(defn sse-patch-elements [html]
  (str "event: datastar-patch-elements\n"
       (apply str (map #(str "data: elements " % "\n") (str/split-lines html)))
       "\n"))

(defn re-render! []
  (doseq [[ch conn-state] (:datastar/connections @!system)]
    (let [state (merge (system->state @!system) conn-state)
          encode (:connection/encoder conn-state)]
      (httpkit/send! ch
                     (encode (sse-patch-elements (str (h/html {:mode :html} (index state)))))
                     false))))

(defn stream [req]
  (httpkit/as-channel
   req
   {:on-open
    (fn [ch]
      (let [encoder+headers (torque/pick-encoder req)]
        (httpkit/send! ch {:status 200
                           :headers (into {"content-type" "text/event-stream"
                                           "cache-control" "no-cache"}
                                          (:headers encoder+headers))}
                       false)
        (swap! !system assoc-in [:datastar/connections ch] (req->state req))
        (swap! !system assoc-in [:datastar/connections ch :connection/encoder] (:encode encoder+headers))))

    :on-close
    (fn [ch _status]
      (swap! !system update :datastar/connections dissoc ch))}))

(comment
  (:datastar/connections @!system)
  )

;; ── commands ──────────────────────────────────────────────────────────────

(defn update-week [offset state _params]
  (let [user (:session/login state)
        current (or (find-current-week state user)
                    (start-of-week (:world/today state)))]
    [[:duratom/assoc-in [:state/per-user (:user/username user) :week] (offset-week current offset)]]))

(defn book-appointment
  {:command/uri "/book-appointment"}
  [state {:strs [date hour]}]
  (let [login (:session/login state)]
    [[:duratom/assoc-in [:state/appointments date (parse-long hour)] (:user/email login)]
     [:garden-email/send {:to {:email owner-email}
                          :subject (format "%s (%s) booked appointment at %s %s:00"
                                           (:user/name login) (:user/email login) date hour)}]
     [:garden-email/send {:to {:email (:user/email login)}
                          :subject (format "You booked an appointment at %s %s:00" date hour)}]]))

(defn unbook-appointment
  {:command/uri "/unbook-appointment"}
  [state {:strs [date hour] :as params}]
  (let [login (:session/login state)]
    [[:duratom/dissoc-in [:state/appointments date (parse-long hour)]]
     [:garden-email/send {:to {:email owner-email}
                          :subject (format "%s (%s) has canceled their appointment at %s %s:00"
                                           (:user/name login) (:user/email login) date hour)}]
     [:garden-email/send {:to {:email (:user/email login)}
                          :subject (format "You cancelled an appointment at %s %s:00" date hour)}]]))

(defn next-week
  {:command/uri "/next-week"}
  [state params]
  (update-week 1 state params))

(defn prev-week
  {:command/uri "/prev-week"}
  [state params]
  (update-week -1 state params))

(defn clear-appointments
  {:command/uri "/clear-appointments"}
  [_ _]
  (when dev?
    [[:duratom/dissoc-in [:state/appointments]]]))

(def commands-by-uri
  (torque/commands-by-uri
   #{#'next-week
     #'prev-week
     #'book-appointment
     #'unbook-appointment
     #'clear-appointments}))

(defn duratom-assoc-in [system ks v]
  (swap! (:duratom/duratom system) assoc-in ks v))

(defn duratom-dissoc-in [system ks]
  (cond
    (= 1 (count ks))
    (swap! (:duratom/duratom system) dissoc (first ks))

    (< 1 (count ks))
    (swap! (:duratom/duratom system) update-in (butlast ks) dissoc (last ks))))

(defn garden-email-send [_ email]
  (garden-email/send-email! email))

(def effect-handlers
  {:duratom/assoc-in #'duratom-assoc-in
   :duratom/dissoc-in #'duratom-dissoc-in
   :garden-email/send #'garden-email-send})

(defonce ^{:doc "Print an effect's data before effectuating"} print-effect? #{})
#_(def print-effect? #{:garden-email/send})

(defn effectuate! "Effectuate effects sequentially. Halt if one fails."
  [effects]
  (doseq [[kind & args :as effect] effects
          :when effect]
    (if-let [handler (get effect-handlers kind)]
      (try
        (when (print-effect? kind)
          (prn effect))
        (apply handler @!system args)
        (catch Exception e
          (report-exception! :effectuation e)))
      (throw (ex-info (str "Unsupported effect kind: " kind) {:kind kind})))))

(defn valid-effect? [effect]
  (and (vector? effect)
       (keyword? (first effect))))

(defn dispatch-command! [state req command-handler]
  (httpkit/as-channel
   req
   {:on-open
    (fn [ch]
      (httpkit/send! ch {:status 202})
      (let [params (some-> req :query-string ring.util.codec/form-decode)]
        (try
          (let [effects (command-handler state params)]
            (when-let [malformed (first (remove valid-effect? effects))]
              (throw (ex-info "Malformed effect" {:malformed malformed :effects effects})))
            (effectuate! effects))
          (catch Exception e
            (report-exception! :command-dispatch e {:handler command-handler
                                                    :params params})))))}))

;; ── machinery ─────────────────────────────────────────────────────────────

(defn req->state [req]
  (when-let [user (get-in req [:session :user])]
    {:session/login
     (set/rename-keys user
                      {:uuid :user/uuid
                       :email :user/email
                       :name :user/name
                       :username :user/username})}))

(defn create-state [req]
  (merge (system->state @!system)
         (req->state req)))

(def head
  [:head
   [:meta {:charset "UTF-8"}]
   [:title "Cauliflower/Duratom"]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
   [:script {:type "module" :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js"}]
   [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2.0.6/css/pico.min.css"}]])


(defonce !last-req (atom nil))

(defn create-dev-state "REPL helper to get a state to pass to functions."
  []
  (create-state @!last-req))

(defn app [{:keys [request-method uri] :as req}]
  (reset! !last-req req)
  (cond
    (= :head request-method)
    {:status 202}

    (and (= :get request-method) (= "/stream" uri))
    (stream req)

    (and (= :get request-method)
         (contains? routes [request-method uri]))
    {:status 200
     :headers {"content-type" "text/html"}
     :body (str (h/html {:mode :html}
                  (h/raw "<!DOCTYPE html>\n")
                  [:html
                   head
                   [:body {:data-init "@get('/stream')"}
                    (when dev?
                      [:section.container
                       [:nav [:ul [:li [:a {:href garden-email/outbox-url} "sent emails"]]]]
                       [:hr]])
                    ((get routes [request-method uri])
                     (create-state req))]]))}

    (and (= :post request-method)
         (contains? commands-by-uri uri))
    (do (dispatch-command! (create-state req)
                           req
                           (commands-by-uri uri))
        (re-render!))))

(defonce wrapped-app ; defonce prevents eval-bffer from logging out everyone
  (-> #'app
      garden-id/wrap-auth
      garden-email/wrap-with-email
      ring.middleware.session/wrap-session))

;; ── lifecycle ─────────────────────────────────────────────────────────────

(defn serve! [opts]
  (torque/serve! #'wrapped-app opts))

;; ── dev ───────────────────────────────────────────────────────────────────

;; Refresh connected clients on buffer eval
(when dev? (re-render!))
