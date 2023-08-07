(ns timelines.rules
  (:require
   [odoyle.rules :as o]))

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

(o/fire-rules @*session)
