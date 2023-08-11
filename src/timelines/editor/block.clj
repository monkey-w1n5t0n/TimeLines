(ns timelines.editor.block
  (:require [timelines.graphics :as g]
            [timelines.graphics :refer :all]
            [timelines.protocols :refer :all]
            [clojure.spec.alpha :as s]
            [timelines.globals :refer :all]
            [timelines.colors :refer :all]
            [timelines.defaults :refer :all]
            [timelines.editor.state :refer :all]
            [timelines.specs :refer :all]
            [timelines.api :refer :all]))

;; Specs
(do
  (s/def ::drawable any?)
  (s/def ::atom (s/or :t #(= % 't)
                      :sym symbol?
                      :map map?
                      :str string?
                      :num number?
                      :keyword keyword?))

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
                 :quoted-list ::quoted-list
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
  (def arg-whitespace-width
    (->
     (width (text "a" 0 0 (type-font :arg) (type-paint :arg)))
     (* 0.9)))

  (def f-height (height (text "A" 0 0 (type-font :f) (type-paint :f))))
  (def arg-height (height (text "A" 0 0 (type-font :arg) (type-paint :arg))))

  (def arg-y-offset 0 #_(- (- f-height arg-height)))
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
  (let [f (-> (node->drawable f)
              (assoc :font (type-font :f)
                     :paint (type-paint :f)))
        ;; TODO @performance this should probably be clojure.core/+ instead
        ;; but leaving it as a sig for now to see how it performs
        args-x-offset (+ (width f)
                         fn-args-spacing)
        args (->drawable args)
        ;; Offset args
        args (container (+ arg-whitespace-width
                           args-x-offset)
                        arg-y-offset
                        (loop [acc []
                               offset 0
                               args args]
                          (if (empty? args)
                            acc
                            (let [arg (first args)]
                              (recur (conj acc (update arg :x + offset))
                                     (+ offset (width arg) arg-whitespace-width)
                                     (rest args))))))
        width 150
        height 30
        background (-> (rect 0 -20 width height)
                       (apply-paint (paint palette-white)))]
    (container 0 0 [background f args])))

(defn node->drawable [[type body]]
  (case type
    :atom (atom->drawable body)
    :fn-call (fn-call->drawable body)
    :vec (->drawable body)
    :quoted-list nil
    nil))

(defn ->drawable [n]
  (if (-> n first keyword?)
    ;; It's a parsed node
    (node->drawable n)
    ;; It's a vector of nodes
    (mapv ->drawable n)))

(defn blocks []
  (let [x 200
        y 300
        b (-> '(foo 1 :bar (+ bar t)) ->tree ->drawable)]
    (def v b)
    (-> b (assoc :x x :y y))))

(comment
  (def test-d
    (-> '(foo 1 :bar) ->tree ->drawable)))

;;
(comment

;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;

  (def amp-expr '(+ 0.5 (* 0.2 (sine01 (* twoPi (slow 2 bar))))))
  (def melody-expr '(from-list [0 0 3 0 2 2 5 12] bar))

  (def amp-sig (eval amp-expr))
  (def melody-sig (eval melody-expr))
  (def freq-expr (list 'midinote->freq melody-expr)))
