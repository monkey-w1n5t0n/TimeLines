(require 'leiningen.core.eval)

(def dependencies
  '[[org.clojure/clojure "1.11.1"]
    [org.jetbrains.skija/skija-linux "0.93.1"]
    [nrepl "1.1.0-alpha1"]
    [org.typedclojure/typed.clj.runtime "1.0.32"]
    [org.clojure/test.check "1.1.1"]
    [org.clojure/core.specs.alpha "0.2.62"]
    [com.rpl/specter "1.1.4"]
    [net.sekao/odoyle-rules "1.1.0"]])

;; per-os jvm-opts code cribbed from Overtone
(def JVM-OPTS
  {:common   []
   ;; TODO add these to linux as well? at least first one
   :macosx   ["-XstartOnFirstThread" "-Djava.awt.headless=true"]
   :linux    []
   :windows  []})

(defn jvm-opts
  "Return a complete vector of jvm-opts for the current os."
  []
  (let [os (leiningen.core.eval/get-os)]
    (vec (set (concat (get JVM-OPTS :common)
                      (get JVM-OPTS os))))))

(def LWJGL_NS "org.lwjgl")

;; Edit this to change the version.
(def LWJGL_VERSION "3.3.2")

;; Edit this to add/remove packages.
(def LWJGL_MODULES ["lwjgl"
                    "lwjgl-glfw"
                    "lwjgl-opengl"
                    ;; "lwjgl-assimp"
                    ;; "lwjgl-bgfx"
                    ;; "lwjgl-egl"
                    ;; "lwjgl-jawt"
                    ;; "lwjgl-jemalloc"
                    ;; "lwjgl-lmdb"
                    ;; "lwjgl-lz4"
                    ;; "lwjgl-nanovg"
                    ;; "lwjgl-nfd"
                    ;; "lwjgl-nuklear"
                    ;; "lwjgl-odbc"
                    ;; "lwjgl-openal"
                    ;; "lwjgl-opencl"
                    ;; "lwjgl-opengles"
                    ;; "lwjgl-openvr"
                    ;; "lwjgl-par"
                    ;; "lwjgl-remotery"
                    ;; "lwjgl-rpmalloc"
                    ;; "lwjgl-sse"
                    ;; "lwjgl-stb"
                    ;; "lwjgl-tinyexr"
                    ;; "lwjgl-tinyfd"
                    ;; "lwjgl-tootle"
                    ;; "lwjgl-vulkan"
                    ;; "lwjgl-xxhash"
                    ;; "lwjgl-yoga"
                    ;; "lwjgl-zstd"
                    ])

;; It's safe to just include all native dependencies, but you might
;; save some space if you know you don't need some platform.
(def LWJGL_PLATFORMS ["linux"
                      ;; "macos" "windows"
                      ])

;; These packages don't have any associated native ones.
(def no-natives? #{"lwjgl-egl" "lwjgl-jawt" "lwjgl-odbc"
                   "lwjgl-opencl" "lwjgl-vulkan"})

(defn LWJGL-deps-with-natives []
  (apply concat
         (for [m LWJGL_MODULES]
           (let [prefix [(symbol LWJGL_NS m) LWJGL_VERSION]]
             (into [prefix]
                   (if (no-natives? m)
                     []
                     (for [p LWJGL_PLATFORMS]
                       (into prefix [:classifier (str "natives-" p)
                                     :native-prefix ""]))))))))

(def all-dependencies
  (into ;; Add your non-LWJGL dependencies here
   dependencies
   (LWJGL-deps-with-natives)))

(defproject timelines "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["space-maven" "https://packages.jetbrains.team/maven/p/skija/maven"]]
  :dependencies ~all-dependencies
  :jvm-opts ^:replace ~(jvm-opts)
  :main ^:skip-aot timelines.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
