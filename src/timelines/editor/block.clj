(ns timelines.editor.block
  (:require [timelines.graphics :as g]
            [timelines.graphics :refer :all]
            [timelines.protocols :refer :all]
            [clojure.spec.alpha :as s]
            [timelines.globals :refer :all]
            [timelines.colors :refer :all]
            [timelines.defaults :refer :all]
            [timelines.editor.state :refer :all]
            [flow-storm.api :as fs-api]
            [timelines.specs :refer :all]
            [timelines.api :refer :all]))

(comment
  (fs-api/local-connect))

(defn arrange-horizontally [offset records]
  (reduce (fn [acc record]
            (let [prev-x (if (empty? acc) 0 (:x (last acc)))
                  prev-width (if (empty? acc) 0 (get-width (last acc)))
                  new-x (+ prev-x prev-width (if (empty? acc) 0 offset))]
              (conj acc (assoc record :x new-x))))
          []
          records))

;; Specs
(do
  (s/def ::drawable any?)
  (s/def ::atom (s/or :t #(= % 't)
                      :sym symbol?
                      ;; :map map?
                      :str string?
                      :num number?
                      :keyword keyword?))

  (s/def ::map map?)
  (s/def ::quoted-list (s/and #(or (list? %)
                                   (instance? clojure.lang.Cons %))
                              (s/cat :quote-op #(= 'quote %)
                                     :elements list?)))

  (s/def ::vector (s/and vector? (s/coll-of ::expr)))

  (s/def ::fn-call (s/cat :f (s/or :atom ::atom
                                   :vec ::vector)
                          :args (s/* ::expr)))
  (s/def ::expr (s/or
                 :atom ::atom
                 ;; :quoted-list ::quoted-list
                 :vec ::vector
                 :fn-call ::fn-call)))

(def font-base-size 20)

(def *type-fonts
  ;; TODO @flexibility this should support signal font sizes too, but it errors out
  (atom {:f (font (clojure.core/+ font-base-size 4))
         :arg (font 23)
         :t (font (clojure.core/+ font-base-size 3))}))

(def *type-paints
  (atom {:f (paint red)
         :str (paint green)
         :sym (paint blue)
         :arg (paint black)
         :t (paint white)}))

(defn type-font [x]
  (get @*type-fonts x default-text-font))

(defn type-paint [x]
  (get @*type-paints x default-paint))

(do

  #_(def f-height (get-height (text "A" 0 0
                                    (type-font :f)
                                    (type-paint :f))))
  #_(def arg-height (get-height (text "A" 0 0
                                      (type-font :arg)
                                      (type-paint :arg))))

  (def fn-args-y-offset 0)
  (def fn-args-spacing 15)
  ;;
  )

(defn ->tree
  "Attempt to parse a data structure to a valid tree"
  [x]
  (let [parse-result (s/conform ::expr x)]
    (if (= :clojure.spec.alpha/invalid parse-result)
      (throw (Exception. (do (s/explain ::expr x)
                             "Invalid tree, see s/explain printout")))
      parse-result)))

;; (s/fdef atom->drawable
;;   :args ::atom
;;   :ret ::drawable)

(declare node->drawable)
;; Atoms
(defn atom->drawable [[type x]]
  (let [font (type-font type)
        paint (type-paint type)
        string (pr-str x)]
    (text string 0 0 font paint)))

(declare ->drawable)

;; Compound types
(defn fn-call->drawable [{:keys [f args] :as e}]
  (let [x-padding 0
        y-padding 0
        space-before-args 0
        f (-> (node->drawable f)
              (assoc :font (type-font :f)
                     :paint (type-paint :f)))
        ;; TODO @performance this should probably be clojure.core/+ instead
        ;; but leaving it as a sig for now to see how it performs
        args-x-offset (+ (get-width-impl f)
                         fn-args-spacing)
        args (->drawable args)
        ;; Offset args
        args (container (+ fn-args-spacing
                           args-x-offset)
                        fn-args-y-offset
                        (arrange-horizontally fn-args-spacing args)
                        #_(loop [acc []
                                 offset 0
                                 args args]
                            (if (empty? args)
                              acc
                              (let [arg (first args)]
                                (recur (conj acc (update arg :x + offset))
                                       (+ offset (get-width-impl arg) arg-whitespace-width)
                                       (rest args))))))
        width (+ x-padding (get-width f) space-before-args (get-width args))
        height (+ y-padding (get-height f))
        background (-> (rect 0 -20 width height)
                       (apply-paint (paint palette-white)))
        children [background f args]]
    (container 0 0 children)))

#_(defn arrange-with-spacing [spacing nodes]
    (loop [acc []
           nodes nodes
           running-offset 0]
      (if (empty? nodes)
        acc
        (let [obj (first nodes)]
          (recur (conj acc (update :x + running-offset))
                 (rest nodes)
                 (+ running-offset (get-width obj)))))))

(def vec-arg-spacing 10)

(defn vec->drawable [body]
  (let [body (mapv ->drawable body)
        body (arrange-horizontally vec-arg-spacing body)
        max-height (apply max (mapv get-height body))
        background (-> (rect 0 (+ 3 (- max-height)) (get-width body) max-height)
                       (apply-paint (paint white :stroke)))] ;; TODO find correct height
    (container 0 0 [background body])))

(defn node->drawable [[type body]]
  (case type
    :atom (atom->drawable body)
    :fn-call (fn-call->drawable body)
    :vec (vec->drawable body)
    :quoted-list nil
    nil))

#_(defn ->drawable [n]
    (if (-> n first keyword?)
    ;; It's a parsed node
      (node->drawable n)
    ;; It's a vector of nodes
      (mapv ->drawable n)))

(defn ->block [e]
  (-> e ->tree node->drawable))

(defonce *selected (atom nil))

(defn blocks []
  (into []
        (let [num 5]
          (for [i (clojure.core/range num)]
            (let [x (+ 100 (* 300 (/ i num)))
                  y (+ 200 (* 300 (/ i num)))
                  w 200
                  h 200]
              (rect x y w h))))))

(comment

  (sample-at (blocks) 1)

  (def test-exprs '[1
                    (+ 1 2)
                    (- 3 (* 1.2 8))
                    t
                    (+ 1 t)
                    (fromList [1 2 3] t)])

  (clojure.pprint/pprint (map ->tree test-exprs))
  ;;
  )
