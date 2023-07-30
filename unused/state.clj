(ns timelines.state
  (:require [timelines.utils :refer :all]
            [timelines.parameters :refer :all]))

(defonce *queue (agent []))

;; (defonce *signal-engine-state (atom {:signals}))


(defmacro send-queue [& body]
  `(send *queue conj (fn [] ~@body)))


(def lorem-ipsum "Earum a nostrum corporis. Nostrum pariatur molestias voluptatem voluptatem officia hic consectetur. Cum voluptatem saepe excepturi accusantium aut fugiat sint. Veritatis rem est quod temporibus totam voluptatem molestiae sed. Sed tempora veritatis animi architecto cum. Error temporibus sed consequatur omnis asperiores aspernatur et eius. Quasi placeat repellat nobis odit aut est alias. Soluta quos adipisci libero dolores autem placeat sit qui. Distinctio accusamus modi magni. Fugit saepe quia voluptate illo qui architecto laboriosam. Corrupti deserunt consectetur aut nulla necessitatibus. Qui placeat consectetur omnis autem enim. Mollitia quae iste placeat aliquid et beatae fuga. Autem soluta optio excepturi quae expedita ducimus qui sit. Natus dolore blanditiis et. Voluptatem cum quia porro. Ut repellendus a neque corporis nulla vel. Quia suscipit quo cum veritatis. Earum aut ratione sint aut. Nam vitae maxime praesentium nulla voluptatem asperiores dignissimos. Asperiores rerum quas sit. Omnis sed doloremque ut fugit temporibus omnis voluptatem voluptatem. Et tempora voluptas ipsam rerum unde occaecati. Magnam quia voluptatem tenetur ut velit. Et est ad alias earum dicta aspernatur.")


(defonce *state (agent {:text lorem-ipsum
                        :point {:x 0 :y 0}
                        }))

(def dummy-window-state {:windows [{:position {:x 0
                                               :y 0}
                                    :dimensions {:w (/ window-width 2)
                                                 :h window-height}}
                                   {:position {:x (/ window-width 2)
                                               :y 0}
                                    :dimensions {:w (/ window-width 2)
                                                 :h window-height}}
                                   ]})


(defn window-state []
  dummy-window-state)







(defn window-x [w]
  (get-in w [:position :x]))

(defn window-y [w]
  (get-in w [:position :y]))

(defn window-w [w]
  (get-in w [:dimensions :w]))

(defn window-h [w]
  (get-in w [:dimensions :h]))


(defn windows []
  (:windows (window-state)))


