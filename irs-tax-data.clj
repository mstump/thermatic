(ns income
  (:use clojure.contrib.generic.functor)
  (:require [incanter.stats :as stats])
  (:require [clojure.zip :as zip])
  (:require [clojure.xml :as xml])
  (:require [clojure.contrib.lazy-xml :as lazy-xml])
  (:require [clojure.contrib.io :as io])
  (:require [clojure.contrib.string :as string]))

(def map-file-name "maps/USA_Counties_with_FIPS_and_names.svg")
(def map-colors ["#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"])
(def input-file-name "data/countyincome07.dat")
(def output-file-name "income-map.svg")

(def path-style "font-size:12px;fill-rule:nonzero;stroke:#FFFFFF;stroke-opacity:1;stroke-width:0.1;stroke-miterlimit:4;stroke-dasharray:none;stroke-linecap:butt;marker-start:none;stroke-linejoin:bevel;fill:")

(defn parse-data [i output]
  (if-not (empty? i)
	(let [lv (vec (first i))
		  tv (map #(clojure.contrib.string/trim (apply str %)) 
				  [(subvec lv 0 2)(subvec lv 3 6)(subvec lv 7 9)(subvec lv 10 41)
				   (subvec lv 43 55)(subvec lv 55 67)(subvec lv 67 79)(subvec lv 79 91)
				   (subvec lv 91 103)(subvec lv 103 115)])]
	  (if-not (= (nth tv 1) "000")
		(recur (rest i) (assoc output (apply str (take 2 tv)) (concat (take 2 (drop 2 tv)) (map #(Integer. %) (drop 4 tv)))))
		(recur (rest i) output)))
	output))

(defn color [colors quantiles v]
  (get (zipmap quantiles colors) (first (filter #(> % v) quantiles))))

(defn color-map [loc cf]
  (if-not(zip/end? loc)
	(let [node (zip/node loc)]
	  (if (= :path (:tag node))
		(let [cv (cf (:id (:attrs node)))]
		  (if cv
			(recur (zip/next (zip/replace loc (assoc node :attrs (assoc (:attrs node) :style (str path-style cv))))) cf)
			(recur (zip/next loc) cf)))
		(recur (zip/next loc) cf)))
	(zip/root loc)))

(defn graph [parser data]
  (doall
   (let [d (parser data)]
	 (io/spit output-file-name 
			  (with-out-str
				(lazy-xml/emit
				 (color-map
				  (zip/xml-zip (lazy-xml/parse-trim (java.io.File. map-file-name)))
				  (partial get (fmap (partial color map-colors (stats/quantile (vals d))) d))))))
	
	 (println (stats/quantile (vals d)))
	 (map prn d))))

(defn get-interest-data [d]
  (fmap #(* 1000 (float (/ (last %) (nth % 2)))) d))

(defn get-exemptions-to-returns-ratio-data [d]
  (fmap #(float (/ (nth % 3) (nth % 2))) d))

(defn get-exemptions-to-returns-ratio-data [d]
  (fmap #(float (/ (nth % 3) (nth % 2))) d))

(defn get-dividends-data [d]
  (fmap #(* 1000 (float (/ (nth % 6) (nth % 2)))) d))

(defn get-wage-data [d]
  (fmap #(* 1000 (float (/ (nth % 5) (nth % 2)))) d))

(defn get-adinc-data [d]
  (fmap #(* 1000 (float (/ (nth % 4) (nth % 2)))) d))


(graph get-adinc-data (parse-data (io/read-lines input-file-name) (sorted-map)))

;; (doall
;;  (io/spit output-file-name 
;; 		  (with-out-str
;; 			(lazy-xml/emit
;; 			 (color-map
;; 			  (zip/xml-zip (lazy-xml/parse-trim (java.io.File. map-file-name)))
;; 			  (partial get (fmap (partial color map-colors (stats/quantile (vals exempt-returns-ratio-data))) exempt-returns-ratio-data)))))))

;; (println (stats/quantile (vals exempt-returns-ratio-data)))
;; (doall (map prn exempt-returns-ratio-data))


;; (def exempt-returns-ratio-data 
;; 	 (get-exemptions-to-returns-ratio-data 
;; 	  (parse-data ["56 045 WY Weston County                            3280        6678      182959      123767        2545        8834"] (sorted-map))))

;; (def line ["56 045 WY Weston County                            3280        6678      182959      123767        2545        8834"])

;; (println (nth (first (vals (parse-data line (sorted-map)))) 2))



;(def data (clojure.contrib.io/read-lines input-file-name))
;(doall (map println data))


;(def returnCount (reduce + (map  parsedData)))
;(def idata (map #(* 1000 (float (/ (last %) (nth % 3)))) parsedData))

;(def parsedData (parse-data data (sorted-map)))
;(xml-seq (parse (java.io.File. map-file-name)))





;(doall (map #(println (nth % 1) (str "'" (nth % 1) "'") (= (nth % 1) "000")) parsedData))

;(doall (map println parsedData))
;(view (histogram idata))




;; (println (float 
;; 		  (/ 
;; 		   (* 1000 (reduce + (map last parsedData)))
;; 		   returnCount)))
;;(doall (map println (map last parsedData)))



;; (let [lv    (vec l)
;; 		sfips (clojure.contrib.string/trim (apply str (subvec lv 0 2)))
;; 		cfips (clojure.contrib.string/trim (apply str (subvec lv 3 6)))
;; 		pcode (clojure.contrib.string/trim (apply str (subvec lv 7 9)))
;; 		cname (clojure.contrib.string/trim (apply str (subvec lv 10 41)))
;; 		rsize (Integer. (clojure.contrib.string/trim (apply str (subvec lv 43 55))))
;; 		esize (Integer. (clojure.contrib.string/trim (apply str (subvec lv 55 67))))
;; 		adinc (Integer. (clojure.contrib.string/trim (apply str (subvec lv 67 79))))
;; 		wginc (Integer. (clojure.contrib.string/trim (apply str (subvec lv 79 91))))
;; 		dvinc (Integer. (clojure.contrib.string/trim (apply str (subvec lv 91 103))))
;; 		iinc  (Integer. (clojure.contrib.string/trim (apply str (subvec lv 103 115))))]
	

;(println filename)

