(ns timelines.core
  (:require
   [nrepl.server :as nrepl]
   [timelines.globals :refer [*main-canvas screen-width screen-height]]
   ;; [timelines.editor :as editor]
   [timelines.editor :as editor]
   [timelines.keyboard :as key]
   [timelines.utils :refer [color]])
  (:import
   [org.jetbrains.skija BackendRenderTarget ColorSpace DirectContext FramebufferFormat Surface SurfaceColorFormat SurfaceOrigin]
   [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback GLFWKeyCallbackI]
   [org.lwjgl.opengl GL GL11]
   [org.lwjgl.system MemoryUtil]))

(def target-fps 60)

;; Misc preparations
(set! *warn-on-reflection* true)

;; LWJGL functions
(do
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
    (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE))

  ;; TODO what's the type of window?
  (defn display-scale [window]
    (let [x (make-array Float/TYPE 1)
          y (make-array Float/TYPE 1)]
      (GLFW/glfwGetWindowContentScale window x y)
      [(first x) (first y)])))

(defn -main [& args]
  (init-GLFW!)
  (let [window (create-main-window! screen-width screen-height "Skija LWJGL Demo")]

    (doto (Thread. #(clojure.main/main))
      (.start))

    (GLFW/glfwSetKeyCallback window
                             (reify GLFWKeyCallbackI
                               (invoke [this _ key _ action mods]
                                 (#'key/handle-event key action))))

    (nrepl/start-server :port 7888)
    (println "nREPL server started at locahost:7888")

    ;; TODO abstract these, look at stash
    (let [context (DirectContext/makeGL)
          fb-id   (GL11/glGetInteger 0x8CA6)
          [scale-x scale-y] (display-scale window)
          target  (BackendRenderTarget/makeGL (* scale-x screen-width) (* scale-y screen-height) 0 8 fb-id FramebufferFormat/GR_GL_RGBA8)
          surface (Surface/makeFromBackendRenderTarget context target SurfaceOrigin/BOTTOM_LEFT SurfaceColorFormat/RGBA_8888 (ColorSpace/getSRGB))
          canvas  (.getCanvas surface)]
      ;; Flipping coordinate system
      ;; (.scale canvas 1 -1)
      ;; (.translate canvas 0 (- screen-height))
      ;; Continue...
      (.scale canvas scale-x scale-y)
      (reset! *main-canvas canvas)
      ;; Main loop
      (let [time-deltas (atom [])
            avg-fps (atom 0)
            target-ns-per-frame (/ 1e9 target-fps)]
        (loop []
          (when (not (GLFW/glfwWindowShouldClose window))
            (let [start-time (System/nanoTime)]

        ;; RENDER
              (.clear canvas (color 0xFFFFFFFF))
              (let [layer (.save canvas)]
                (editor/draw-screen)
                (editor/draw-fps @avg-fps)
                (.restoreToCount canvas layer))
              (.flush context)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)

        ;; FPS
              (swap! time-deltas conj (- (System/nanoTime) start-time))

        ;; Keep only the last 100 time measurements to calculate FPS
              (when (> (count @time-deltas) 100)
                (swap! time-deltas subvec 1))

        ;; Calculate FPS every 100 frames
              (when (zero? (mod (count @time-deltas) 100))
                (let [avg-time (/ (reduce + @time-deltas) (count @time-deltas))
                      fps (/ 1e9 avg-time)]
                  (reset! avg-fps fps)))

        ;; Calculate the time this frame took
              (let [end-time (System/nanoTime)
                    frame-duration (- end-time start-time)
                    sleep-time (- target-ns-per-frame frame-duration)]
                (when (> sleep-time 0)
            ;; Convert nanoseconds to milliseconds for Thread/sleep
                  (Thread/sleep (long (quot sleep-time 1e6)))))

              (recur)))))

      ;; Shutdown
      (Callbacks/glfwFreeCallbacks window)
      (GLFW/glfwHideWindow window)
      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwPollEvents)

      (.close surface)
      (.close target)
      (.close context)

      (GLFW/glfwTerminate)
      (.free (GLFW/glfwSetErrorCallback nil))
      (shutdown-agents))))

(comment
  (-main))
