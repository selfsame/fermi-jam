(ns universe.domain
  (:require 
    [universe.temporal]
    [combinatorics :as combo]
    [clojure.pprint :as pprint])
  (:import [System]))

(declare unit? unit-ratio? udiv u*)

(defmacro type-writer [sym args & code]
  `(~'.addMethod ~'print-method ~sym
    (fn [~(first args) w#] (~'.Write w# ~@code))))

(def SI-data
  (into {} (map 
    #(hash-map (keyword %1) {:name (str %1) :prefix (str %2) :factor %3}) 
    '[deca  hecto kilo  mega  giga  tera  peta  exa zetta yotta deci  centi milli micro nano  pico  femto atto  zepto yocto]
    '[da  h k M G T P E Z Y d c m μ n p f a z y]
    '[1e1 1e2 1e3 1e6 1e9 1e12  1e15  1e18  1e21  1e24 1e-1  1e-2  1e-3  1e-6  1e-9  1e-12 1e-15 1e-18 1e-21 1e-24])))

(defn gen-conversions 
  ([m] (gen-conversions ""))
  ([m sufx]
    (into {} (map 
      (fn [[a b]]
        (let [[af bf] (mapv #(:factor (get m %)) [a b])]
          {(mapv #(keyword (str % sufx)) [a b]) 
           (* (/ 1 bf) af)}))
      (map vec 
        (remove (partial apply =) (combo/selections  (keys m) 2)))))))

'(def SI-convert-map (gen-conversions SI-data))

(def domain-bases {:space :m :time :s})



(def unit-data
  {:m       {:name "meter"}
   :pc      {:name "parsec"}
   :Mpc     {:name "megaparsec"}

   :s       {:name "second"}
   :h       {:name "hour"}
   :d       {:name "day"}
   :y       {:name "year"}
   :eon     {:name "eon"}})

(def unit-domains
  {:space #{:m :km :pc :Mpc} 
   :time  #{:s :h :d :y :eon}})

(def unit->domain 
  (into {} 
    (mapcat (fn [[k v]] (zipmap v (repeat k))) 
      unit-domains)))



(def unit-convert {
  [:m :km] (/ 1 1000)
  [:km :m] 1000

  [:m :pc] (/ 1 3.086e16)
  [:pc :m] 3.086e16

  [:m :Mpc] (/ 1 (* 3.086e12 1000 1000000))
  [:Mpc :m] (* 3.086e12 1000 1000000)

  [:km :pc] (/ 1 3.086e13)
  [:pc :km] 3.086e13

  [:km :Mpc] (/ 1 3.086e19)
  [:Mpc :km] 3.086e19

  [:pc :Mpc] (/ 1 1e6)
  [:Mpc :pc] 1e6

  [:s :h] (/ 1 (* 60 60))
  [:h :s] (* 60 60)

  [:h :y] (/ 1 8760)
  [:y :h] 8760

  [:s :y] (/ 1 31536000)
  [:y :s] 31536000

  [:y :eon] (/ 1 1e9)
  [:eon :y] 1e9})

(defprotocol IUnit
  (operate [f a b])
  (convert [o u][o a b])
  (ration [a b])
  (edn [o])
  (pretty [o]))


(deftype ^:once Unit [^System.Double n ^clojure.lang.Keyword unit]
  IUnit
  (convert [o k]
    (if (= (.unit o) k) o
      (if-let [x (unit-convert [(.unit o) k])]
        (Unit. (* (.n o) x) k)
        (throw (Exception. (str "can't convert "  
          (apply str (interpose " to " (mapv (juxt identity #(-> % unit-data :name)) [(.unit o) k]))) ))))))
  (ration [a b]
    (when (unit? b) 
      (.n (udiv (convert b (.unit a)) a))))
  (pretty [n] (str n (name unit)))
  (edn [o] [n unit])
  (operate [o f b] 
    (cond 
      (number? b) (Unit. (f (.n o) b) (.unit o))
      (= (.unit o) (.unit b)) (Unit. (f (.n o) (.n b)) (.unit o))
      :else (Unit. (f (.n o) (.n (convert b (.unit o)))) (.unit o)))))

(type-writer universe.domain.Unit [o] (str "#unit (" (.n o) " " (.unit o) ")"))
(defn read-unit [m] (Unit. (first m) (last m)))
(def unit? #(instance? universe.domain.Unit %))


(defn commensurable? [a b]
  (and (unit? a) (unit? b)
    (= (unit->domain (.unit a))
       (unit->domain (.unit b)))))


(deftype ^:once UnitRatio [^universe.domain.Unit a ^universe.domain.Unit b]
  IUnit
  (convert [o k]
    (if-let [c-domain (unit->domain k)]
      (let [[na nb] (mapv #(if (= c-domain (unit->domain (.unit %))) (convert % k) %) (list a b))]
        (UnitRatio. na nb))))
  (convert [o a b] (convert (convert o a) b))
  (edn [o] (mapv edn [a b]))
  clojure.lang.IFn
  (invoke [o n]
    (when (unit? n)
      (let [c-domain (unit->domain (.unit n))
            [na nb] (mapv #(if (= c-domain (unit->domain (.unit %))) (ration % n) nil) (list a b))]
        (if (= [na nb] [nil nil])  
            (throw (Exception. (str "unusable domain " (.unit n) " for " (prn-str o)))))
        (if na (u* b na) 
               (u* a nb))))))

(type-writer universe.domain.UnitRatio [o] (str "#ratio " (prn-str (list (.a o) (.b o)))))
(defn read-ratio [m] (UnitRatio. (first m) (last m)))
(def unit-ratio? #(instance? universe.domain.UnitRatio %))


(defmacro op-fn [sym f]
  `(defn ~sym 
    ([o#] o#)
    ([o# b#] 
      (cond 
        (or (unit? o#) (unit-ratio? o#)) 
        (operate o# ~f b#) 
        (number? o#)  
        (if (number? b#) 
          (~f o# b#)
          (operate (~'universe.domain.Unit. o# :m) ~f b#))
        :else b#))
    ([o# b# & more#] (reduce ~sym (~sym o# b#) more#))))

(op-fn u+ +)
(op-fn u- -)
(op-fn u* *)
(op-fn udiv /)
(op-fn umax max)
(op-fn umin min)
(op-fn umod mod)
(op-fn u> >)


(defn unit [n u] (Unit. n u))

(defn per [a b] 
  (assert (unit? a) (unit? b)) 
  (UnitRatio. a b))










;unit value
(unit 1 :km)

;numeric op on unit types of the same domain
(u+ (unit 1 :s) (unit 1 :h) (unit 1 :y))

;convert km to meters and parsecs
(map (fn [k] (convert (unit 1e12 :km) k)) [:m :pc])

;speed of light, a ratio of two domain types
(def C (per (unit 3.00e8 :m) (unit 1 :s)))


;Hubble constant
(def H (per (unit 67.6 :km) (unit 1 :s)))


;age of universe
(def NOW (unit 1.38e10 :y))

;invoking lightspeed ratio on :space returns :time
(convert (C (unit 1 :pc)) :y)

;calc universe expansion speed for distance
(defn expansion-rate [d]
  (per (u* (udiv d (unit 1 :Mpc)) (.a H)) 
       (.b H)))

;distance where expansion is faster than lightspeed?
(map #(convert % :km) [C (expansion-rate (unit 4439 :Mpc))])


;megaparsec radius of the observable universe
(convert (C NOW) :Mpc)


;expansion speed from observational horizon
(convert (expansion-rate (C NOW)) :km)






#_(defn test-conversion []
  (pprint/pprint  
    {:round-trip 
      (remove empty?
        (map (fn [[a b]] 
        (conj 
          (if-not (= (.n (unit 1 a)) (.n (convert (convert (unit 1 a) b) a))) 
          {:round-trip [a b (list (.n (unit 1 a)) (.n (convert (convert (unit 1 a) b) a)))]} 
          {})
          (if-not (= (.n (unit 1 a)) (.n (convert (u* (unit 1 a) (ration  (unit 1 a) (unit 1 b))) b)))
            { :ratio [a b (.n (convert (u* (unit 1 a) (ration  (unit 1 a) (unit 1 b))) b))]}
            {} ) ))
        (keys unit-convert)))}))

'(test-conversion)

