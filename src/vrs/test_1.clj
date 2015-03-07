(ns vrs.test-1
  (:require [incanter.core :as ic]
            [incanter.charts :as icharts]
            [clojure.pprint :as pp]))

; Uses Vitter's reservoir sampling algorithm to take n items
; without replacement, from a iterable of items of unknown,
; arbitrary length. Inspired by
; https://www.omniref.com/ruby/gems/stream_sampler/0.0.1/symbols/StreamSampler

; Given some large pool of data, we'd like to confirm that over several runs the
; distribution of randomly selected samples is uniform.

(defn- replace-in-output [sample-size output [idx item]]
  (let [rnd (rand-int idx)]
    (if (< idx sample-size)
      ; Pick any item if we don't yet have enough samples
      (assoc output idx item)
      ; Otherwise pick current item if rnd is less than sample-size
      (if (< rnd sample-size)
        (assoc output rnd item)
        ; Finally, if rnd >= sample-size: Keep output as-is
        output))))

(defn get-random-samples [sample-size data]
  (reduce (partial replace-in-output sample-size) [] (map #(vec [%1 %2]) (range) data)))

; The below data can be of any kind, but we'll stick to a simple
; sequence of numbers for the example:
(def data (range 1 1000))

; How many samples to randomly fetch
(def sample-size 25)

;(println (get-random-samples sample-size data))

; Ok, so above we have the get-random-samples function we can now use..
; Next we want to get the histogram data we need to look at the distribution.

(def sample-count 10000)

(defn- update-map-count [output item]
  (assoc output item (inc (get output item 0))))

(def sample-seq (map (fn [& _] (get-random-samples sample-size data)) (range sample-count)))
;(pp/pprint sample-seq)

(def distribution-map (reduce update-map-count (sorted-map) (mapcat identity sample-seq)))
;(pp/pprint distribution-map)

(ic/view (icharts/histogram (mapcat identity sample-seq) :nbins 1000))

; Formally testing that the distribution is uniform is left to the reader..
