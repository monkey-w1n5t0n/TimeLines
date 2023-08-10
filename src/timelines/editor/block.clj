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
  (s/def ::frawable any?)
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

(def *type-fonts
  (atom {:f (font 28)
         :arg (font 23)
         :t (font 29)}))

(def *type-paints
  (atom {:f (paint red)
         :str (paint green)
         :sym (paint blue)
         :t (paint white)}))

(defn type-font [x]
  (get @*type-fonts x default-text-font))

(defn type-paint [x]
  (get @*type-paints x default-paint))

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

(declare tree->drawables)
;; Atoms
(defn atom->drawable [[type x]]
  (let [font (type-font type)
        paint (type-paint type)
        string (pr-str x)]
    (text string 0 0 font paint)))

(def fn-args-spacing 10)

;; Compound types
(defn fn-call->drawable [{:keys [f args] :as e}]
  (let [f (tree->drawables f)
        ;; TODO @performance this should probably be clojure.core/+ instead
        ;; but leaving it as a sig for now to see how it performs
        args-x-offset (+ (->width f)
                         fn-args-spacing)
        args (-> (tree->drawables args)
                 (update :x + args-x-offset))]
    (container 0 0 f args)))

(declare ->drawables)

(defn tree->drawables [[type body]]
  (case type
    :atom (atom->drawable body)
    :fn-call (fn-call->drawable body)
    :vec (->drawables body)
    :quoted-list nil
    nil))

(defn ->drawables [e]
  (if (-> e first keyword?)
    (tree->drawables e)
    (mapv ->drawables e)))

(defn blocks []
  (let [b
        (-> '(foo 1 :bar) ->tree tree->drawables
            (assoc :x 500 :y 200))]
    (println b)
    (System/exit 0)
    b))

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
  (defn build-fn-call [f args]
    (let [fn-text (node->text)
          args-text]
      (container 0 0 fn-text args-text)))

  (defn expr->texts [parsed-e]
    (let [f (fn [acc [type v]]
              (do
                (println type)
                v))]
      (reduce f [] parsed-e))
    #_(loop [acc [] [h tail] parsed-e]
        (println (str (first h) ", " (second h)))
        (recur (conj acc (second h)) tail)))

  ;; (parse-str test-str)

  (node->text :f "hello")

  (def amp-expr '(+ 0.5 (* 0.2 (sine01 (* twoPi (slow 2 bar))))))
  (def melody-expr '(from-list [0 0 3 0 2 2 5 12] bar))

  (def amp-sig (eval amp-expr))
  (def melody-sig (eval melody-expr))
  (def freq-expr (list 'midinote->freq melody-expr))

  (def param-font (font "FiraCode Regular" 25))

  (def *highlighted (atom 0))

  (def block1 (apply-paint  (rect (+ 100 (* 100 (mod1 t))) 200 300 300)
                            (paint red)))

  (def block2 (apply-paint  (rect 500 200 300 300)
                            (paint blue)))

  (def blockv [block1 block2])

  (defn text-width [text]
    (let [bounds (->width text)
          left (._left bounds)
          right (._right bounds)]
      (- right left)))

  (defn blocks []
    (build-fn-call [:sym from-list]
                   [[:vec [1 2 3]] [:sym bar]])
    #_(let [f "mod1"
            arg "t"
            f-font (font 30)
            arg-font (font 28)
            x (half screen-width)
            y (half screen-height)
            f-text (text f x y f-font)
            spacing 20
            arg-left (+ x spacing (text-width f-text))
            arg-text (text arg arg-left y arg-font)]
        [f-text arg-text]))

  (comment

    ;; TODO
    (defn param-block [param code style]
      (let [param (text 0 0 (name param))
            code code
            vis (rect 10 10 10 10)]
        (g/container x y param vis code)))

    (defn block [name param-map]
      (let [n-params (count (keys param-map))
            y-offset (+ block-line-height 10)
            param-blocks (for [[param {:keys [code style]}] param-map]
                           (param-block param code style))]
        (apply g/container param-blocks)))

    (def test-block
      (let [node-x 100
            node-y 20
            node-width 590
            node-height 200
            node-background-paint (paint palette-blue-medium)
            node-stroke-paint (paint palette-red :stroke 2)
            param-size 25
            expr-size 18
            key-paint (paint palette-red)
            text-paint (paint palette-white)
            background (-> (rect node-x node-y node-width node-height 20)
                           (apply-paint node-background-paint))
            parameters
            [;; amp
             (let [x (+ node-x 10)
                   y (+ node-y 30)]
               [(text ":amp" x y param-size key-paint)
                #_(text #_(clojure.core/str (sample-at amp-expr (now))))
                (text (str amp-sig) (+ x 80) y expr-size text-paint)
                (text (clojure.core/str amp-expr) (+ x 10) (+ y param-size) expr-size text-paint)])

             ;; freq
             (let [x (+ node-x 10)
                   y (+ node-y 100)]
               [(text ":freq" x y param-size key-paint)
                (text (str melody-sig) (+ x 80) y expr-size text-paint)
                (text (clojure.core/str freq-expr) (+ x 10) (+ y param-size) expr-size text-paint)])]]

        [background parameters]))))
