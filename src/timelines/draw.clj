(ns timelines.draw
  (:import
   [org.jetbrains.skija
    BackendRenderTarget Canvas ColorSpace
    DirectContext FramebufferFormat Paint
    Rect Surface SurfaceColorFormat SurfaceOrigin]))

;; ;; GENERIC
;; (defn node-type [node]
;;   (:type node))

;; (defn render-node-dispatch-fn [n]
;;   (node-type n))

;; (defmulti render-node render-node-dispatch-fn)

;; (defn xywh [n]
;;   (map #(n %1) [:x :y :w :h]))

;; (defn expand-rect [n]
;;   {})

;; (defn contains-all? [m ks]
;;   (every? (partial contains? m) ks))

;; (defn valid-rect? [n]
;;   (and (contains-all? n [:x :y])
;;        (or (contains-all? n [:w :h])
;;            (contains-all? n [:x-end :y-end]))))

;; (defn rect-radius [n]
;;   (or (n :radius) 0))

;; (defn render-rect [n]
;;   (assert (valid-rect? n))
;;   (let [expanded-n (expand-rect n)
;;         [x y w h] (xywh n)
;;         r (rect-radius n)]
;;     (q/rect x y w h r)))

;; (defn draw [tree]
;;   ())
;;
