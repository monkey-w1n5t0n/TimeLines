(ns timelines.keyboard
  (:import
   [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback GLFWKeyCallbackI]))

#_(defn key-callback [window key scancode action mods]
    (let [glfw (GLFW.)
          key-name (.glfwGetKeyName glfw key scancode)
          action-name (condp = action
                        (.glfw.GLFW_PRESS glfw) "pressed"
                        (.glfw.GLFW_RELEASE glfw) "released"
                        (.glfw.GLFW_REPEAT glfw) "repeated")]
      (println (str "Key " key-name " was " action-name))))

(def alphabet-codes (range 65 91))
(def qwerty-order (seq "abcdefghijklmnopqrstuvwxyz"))
(def num-codes (range 48 58))
(def dvorak-order (seq "axje.uidchtnmbrl'poygk,qf;"))

(def scancodes
  (concat
   ;; Numbers start at 48
   (repeat 48 nil)
   (seq "0123456789")
   (repeat 200 nil)))

(def keys (mapv #(if (or (symbol?  %) (number? %)) (str %) %)
                '[:esc :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :del
                  "`" 1 2 3 4 5 6 7 8 9 0 "[" "]" :backspace
                  :tab "'" "," "." p y f g c r l "/" = "\\"
                  :esc a o e u i d h t n s - :enter
                  :lshift < ";" q j k x b m w v z :rshift
                  :l-ctrl :super :l-alt :spc :r-alt :r-ctrl :left :up :down :right]))

(defn keycode->key [k]
  (let [relative (- k 65)]
    (if (<= 0 relative 25)
      (nth dvorak-order relative)
      k)))

(def keycodes [280  290  291  292  293  294  295  296  297  298  299  300  301  261  96  49  50  51  52  53  54  55  56  57  48  45  61  259  258  81  87  69  82  84  89  85  73  79  80  91  93  92  256  65  83  68  70  71  72  74  75  76  59  39  257  340  161  90  88  67  86  66  78  77  44  46  47  344  341  343  342  32  346  345  263  265  264  262])

(def keymap (apply hash-map (flatten (map vector keycodes keys))))

(defn handle-event [key action]
  (let [key key
        ;; action ([:up :down :else] action) TODO there's more than this
        ]
    (when (= action 0)
      (do (flush)
          (print "(" key " " (keymap key) ") ")
          (flush)))))
