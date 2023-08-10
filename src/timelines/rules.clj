(ns timelines.rules
  (:require
   [odoyle.rules :as o]
   [timelines.time :as t]))

(comment

  (do

    (def rules
      (o/ruleset
       {::print-time
        [:what
         [::time ::total tt]
         :then
         (println tt)]

        ::test1
        [:what
         [name a "hi"]
         :when
         (= name 'hello)
         :then
         (println (str "Attribute: " a))]

        ::test2
        [:what
         [e a v]
         :then
         (println (str "Entity: " e ", value: " v))]}))

    (def *session
      (atom (reduce o/add-rule (o/->session) rules)))

    (defn insert! [e a v]
      (swap! *session #(-> % (o/insert e a v) o/fire-rules))
      nil))

  (insert! 'hello :bob "hehe")

  (insert! 'hello :ahhh "hi")

  (insert! :nick :dave "hi")

  @*session
  ;; create session and add rule

  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::time ::total 200)
               o/fire-rules)))

  (o/fire-rules (o/insert @*session ::time ::total 120))

  (o/fire-rules @*session))

(def rules
  (o/ruleset
   {::change-modes
    [:what
     [id :event/type :event/key]
     [id :event/key :esc]
     [id :event.key/action 0]
     [::editor :editor/mode mode]
     :then (o/insert! ::editor :editor/mode
                      (if (= mode :normal)
                        :insert :normal))]

    ::print-mode
    [:what
     [::editor :editor/mode mode]
     :then (println (str "Mode: " mode))]

    ::print-all-keystrokes
    [:what
     [id :event/type :event/key]
     [id :event.key/action a]
     [id :event/key k]
     [id :event/time t]
     :then
     (println (str "Key was pressed! " {:key k :action a :time t}))]}))

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

(defonce *id-counter (atom 0))
(comment (reset! *id-counter 0))

(swap! *session #(o/insert % ::editor :editor/mode :normal))

(defn handle-key-event [key action]
  (let [id (swap! *id-counter inc)
        time (t/now)]
    (swap! *session
           (fn [session]
             (-> session
                 (o/insert id {:event/type :event/key
                               :event.key/action action
                               :event/key key
                               :event/time time})
                 o/fire-rules)))))

(handle-key-event :h 1)
(handle-key-event :esc 0)

()
