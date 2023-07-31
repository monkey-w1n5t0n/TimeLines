(ns timelines.ecs
  (:require [timelines.specs :refer :all]
            [clojure.spec.alpha :as s]))

(defrecord Entity [id name attributes])

(defn make-entity
  ([id attributes] (make-entity id nil attributes))
  ([id name attributes]
   (let [e (->Entity id name attributes)]
     (if-not (s/valid? :timelines/entity e)
       (do
         (s/explain :timelines/entity e)
         (throw (AssertionError. "Invalid entity.")))
       e))))

