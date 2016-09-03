(ns universe.spatial
  (:require [universe.temporal])
  (:import [System]))



(def unit-data
  {:m       {:name "meter"}
   :km      {:name "kilometer"}
   :pc      {:name "parsec"}
   :y       {:name "year"}
   :s       {:name "second"}})

(def unit-convert {
  [:m :km] (/ 1 1000)
  [:km :m] 1000

  [:m :pc] (/ 1 (* 1e12 1000))
  [:pc :m] (* 1e12 1000)

  [:m :Mpc] (/ 1 (* 1e12 1000 1000000))
  [:Mpc :m] (* 1e12 1000 1000000)

  [:km :pc] (/ 1 1e12)
  [:pc :km] 1e12

  [:km :Mpc] (/ 1 (* 1e12 1000000))
  [:Mpc :km] (* 1e12 1000000)

  [:pc :Mpc] (/ 1 1000000)
  [:Mpc :pc] 1000000

  [:y :s] 3.154e+7
  [:s :y] (/ 1 3.154e+7)})

(defprotocol IUnit
  (operate [f a b])
  (convert [o u])
  (pretty  [o]))

(deftype Distance [
  ^:unsynchronized-mutable ^System.Double n 
  ^:unsynchronized-mutable ^clojure.lang.Keyword unit]
  IUnit
  (convert [o k]
    (if (= (.unit o) k) o
      (if-let [x (unit-convert [(.unit o) k])]
        (Distance. (* (.n o) x) k)
        (throw (Exception. (str "can't convert "  
          (apply str (interpose " to " (mapv (juxt identity #(-> % unit-data :name)) [(.unit o) k]))) ))))))
  (pretty [n] (str n (name unit)))
  (operate [o f b] 
    (cond 
      (number? b) 
      (Distance. (f (.n o) b) (.unit o))
      (= (.unit o) (.unit b))
      (Distance. (f (.n o) (.n b)) (.unit o))
      :else
      (Distance. (f (.n o) (.n (convert b (.unit o)))) (.unit o)))))

(def distance? #(instance? universe.spatial.Distance %))

(.addMethod print-method 
  universe.spatial.Distance
  (fn [o w] (.Write w (str "#distance (" (.n o) " " (.unit o) ")"))))

(defn read-distance [m] (Distance. (first m) (last m)))

(defmacro op-fn [sym f]
  `(defn ~sym 
    ([o#] o#)
    ([o# b#] 
      (cond 
        (distance? o#) 
        (operate o# ~f b#) 
        (number? o#)  
        (if (number? b#) 
          (~f o# b#)
          (operate (~'universe.spatial.Distance. o# :m) ~f b#))
        :else b#))
    ([o# b# & more#] (reduce ~sym (~sym o# b#) more#))))

(op-fn u+ +)
(op-fn u- -)
(op-fn u* *)
(op-fn udiv /)
(op-fn umax max)
(op-fn umin min)
(op-fn umod mod)

(defn unit 
  ([n] (Distance. n :m))
  ([n u] (Distance. n u)))




(umax (unit 2 :km) (unit 1.03e-7 :Mpc))
(apply (juxt umax umin) (map u* [(unit 10 :km) (unit 900 :m) (unit 1 :Mpc)] (repeat (unit 1.03e-7 :Mpc))))

(defn expansion-rate [d]
  (Distance. (* (.n H) (.n (convert d :Mpc))) :km))

(expansion-rate (Distance. 100 :pc))

;Hubble constant
(def H (Distance. 67.6 :km))

(def C (unit 3.00e8 :m))

(convert (unit 5 :s) :y)

'(def light-year (universe.temporal/second->year C))




(comment 'tests
(convert (unit 1000 :m) :km)
(convert (unit 1000 :m) :Mpc)
(convert (unit 1 :km) :Mpc))