(ns timelines.core
  (:require
   [nrepl.server :as nrepl]
   [clojure.core.typed :as t]
   [timelines.globals :refer [*main-canvas screen-width screen-height]]
   [timelines.editor :as editor]
   [timelines.utils :refer [color]])

  (:import
   [org.jetbrains.skija BackendRenderTarget Canvas ColorSpace DirectContext FramebufferFormat Paint Rect Surface SurfaceColorFormat SurfaceOrigin]
   [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback GLFWKeyCallbackI]
   [org.lwjgl.opengl GL GL11]
   [org.lwjgl.system MemoryUtil]))

(set! *warn-on-reflection* true)

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
    [(first x) (first y)]))

(defn -main [& args]
  (init-GLFW!)
  (let [window (create-main-window! screen-width screen-height "Skija LWJGL Demo FLOATING")]

    (doto (Thread. #(clojure.main/main))
      (.start))

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
      (reset! *canvas canvas)
      #_(let [draw-thread
              (future)])
      (loop []
        (when (not (GLFW/glfwWindowShouldClose window))
          (.clear canvas (color 0xFFFFFFFF))
          (let [layer (.save canvas)]
            (#'editor/draw)
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
      (shutdown-agents))))

(comment
  (reset! lwjgl.main/*rect-color (lwjgl.main/color 0xFF33CC33))

  (-main))

