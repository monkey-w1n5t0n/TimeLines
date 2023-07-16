(ns timelines.core
  (:require
   [nrepl.server :as nrepl]
   [clojure.core.typed :as t]
   [timelines.protocols :refer [draw]]
   [timelines.globals :refer [*global-canvas]]
   [timelines.api-test :refer [draw-screen]]
   )

  (:import
   [org.jetbrains.skija BackendRenderTarget Canvas ColorSpace DirectContext FramebufferFormat Paint Rect Surface SurfaceColorFormat SurfaceOrigin]
   [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback GLFWKeyCallbackI]
   [org.lwjgl.opengl GL GL11]
   [org.lwjgl.system MemoryUtil])
  )


(set! *warn-on-reflection* true)

(defn color [^long l]
  (.intValue (Long/valueOf l)))

(def *rect-color (atom (color 0xFFCC3333)))


#_(defn draw-canvas [^Canvas canvas]
  ;; (let [paint (Paint.)
  ;;       path (Path.)]
  ;;   (.setColor paint (Color/GREEN))
  ;;   (.moveTo path 10 10)
  ;;   (.lineTo path 100 200)
  ;;   (.lineTo path 200 100)
  ;;   (.close path)
  ;;   (.drawPath canvas path paint)
  ;;   )

  ;; (let [paint (doto (Paint.) (.setColor @*rect-color))]
  ;;   (.translate canvas 320 240)
  ;;   (.rotate canvas (mod (/ (System/currentTimeMillis) 10) 360))
  ;;   (.drawRect canvas (Rect/makeXYWH -50 -50 100 100) paint)
  ;; (.drawText)
  ;; )
  (let [r (rect 50 50 20 50)])
  (draw (color r 0xFFCC3333))

  )

(defn create-window! [width height name]
  (GLFW/glfwCreateWindow width height name MemoryUtil/NULL MemoryUtil/NULL))

(defn create-main-window! [width height name]
  (let [window (GLFW/glfwCreateWindow width height name MemoryUtil/NULL MemoryUtil/NULL)]
    (GLFW/glfwMakeContextCurrent window)
    (GLFW/glfwSwapInterval 1)
    (GLFW/glfwShowWindow window)
    (GL/createCapabilities)
    window))


(defn init-GLFW! []
  (.set (GLFWErrorCallback/createPrint System/err))
  (GLFW/glfwInit)
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  )

;; TODO what's the type of window?
(defn display-scale [window]
  (let [x (make-array Float/TYPE 1)
        y (make-array Float/TYPE 1)]
    (GLFW/glfwGetWindowContentScale window x y)
    [(first x) (first y)]))

(defn -main [& args]
  (init-GLFW!)
  (let [width 640
        height 480
        window (create-main-window! width height "Skija LWJGL Demo FLOATING")]

    (doto (Thread. #(clojure.main/main))
      (.start))

    (nrepl/start-server :port 7888)
    (println "nREPL server started at locahost:7888")

    ;; TODO abstract these, look at stash
    (let [context (DirectContext/makeGL)
          fb-id   (GL11/glGetInteger 0x8CA6)
          [scale-x scale-y] (display-scale window)
          target  (BackendRenderTarget/makeGL (* scale-x width) (* scale-y height) 0 8 fb-id FramebufferFormat/GR_GL_RGBA8)
          surface (Surface/makeFromBackendRenderTarget context target SurfaceOrigin/BOTTOM_LEFT SurfaceColorFormat/RGBA_8888 (ColorSpace/getSRGB))
          canvas  (.getCanvas surface)]
      (.scale canvas scale-x scale-y)
      (reset! *global-canvas canvas)
      (loop []
        (when (not (GLFW/glfwWindowShouldClose window))
          (.clear canvas (color 0xFFFFFFFF))
          (let [layer (.save canvas)]
            (#'draw-screen)
            (.restoreToCount canvas layer))
          (.flush context)
          (GLFW/glfwSwapBuffers window)
          (GLFW/glfwPollEvents)
          (recur)))

      (Callbacks/glfwFreeCallbacks window)
      (GLFW/glfwHideWindow window)
      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwPollEvents)

      (.close surface)
      (.close target)
      (.close context)

      (GLFW/glfwTerminate)
      (.free (GLFW/glfwSetErrorCallback nil))
      (shutdown-agents)
      )))

(comment
  (reset! lwjgl.main/*rect-color (lwjgl.main/color 0xFF33CC33)))


;; (def *state (atom {:time 0}))

;; (defn now []
;;   (:time @*state))

;; ;; (defn draw-at-now [x]
;; ;;   (draw-at x (now)))

;; (defn update-sketch-state [state]
;;   ;; (swap! *state conj :time (clojure.core// (q/millis)))
;;   state)

;; (defn setup-sketch []
;;   (q/frame-rate 60)
;;   (q/color-mode :rgb)
;;   (q/text-align :left :center)
;;   (q/text-size 90)
;;   ;; (q/text-font
;;   ;;  (q/create-font "resources/fonts/FiraMono-Regular.ttf" 30 true))
;;   {})

;; (defn draw-sketch [state]
;;   ;; error color
;;   (q/background 55 0 0)
;;   (q/fill 255)

;;   ;; ()doseq [i (range 100)]
;;   ;; (draw-at














;;   ;;  (rect 100
;;   ;;        100
;;   ;;        (+ 100 (* 80 (fast 4 (sin t))))
;;   ;;        50)






;;   ;;  (clojure.core// (q/millis) 1000))



;;   (draw-at (text (str ".."
;;                       (from-list [",," "...." "..//.." "..//.."] (slow 3 (mod1 t)))
;;                       (from-list ["...." ".. . .. . . ."] (slow 3 (mod1 t)))
;;                       )
;;                  10
;;                  400)
;;         (clojure.core// (q/millis) 1000))
;;   )


;; (q/defsketch timelines
;;   :title "TimeLines"
;;   :size [1000 1000]
;;   :setup setup-sketch
;;   :update update-sketch-state
;;   :draw draw-sketch
;;   :features [:keep-on-top]
;;   :middleware [m/fun-mode])
