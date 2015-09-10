(ns tenki.core
  (:require [net.cgrand.enlive-html :as html]
            [skyscraper :as s]))

(def ^:private tenki-forecast-url "http://www.jma.go.jp/jp/yoho")

(defn map-title [node]
  (first (html/attr-values node :title)))

(defn map-href [node]
  (str tenki-forecast-url "/" (first (html/attr-values node :href))))

(defn seed []
  [{:url tenki-forecast-url
    :processor ::map-page}])

(s/defprocessor map-page
  :cache-template "/tenki/map"
  :process-fn (fn [res ctx]
                (let [areas (html/select res [:map :area])]
                  (for [area areas
                        :let [area-name (map-title area)
                              area-url (map-href area)]
                        :when (re-find #"2\d{2}\.html" area-url)]
                    {:area (map-title area)
                     :url area-url
                     :processor ::area-page}))))

(s/defprocessor area-page
  :cache-template "/tenki/map/:area"
  :process-fn (fn [res ctx]
                (let [prefectures (html/select res [:map :area])]
                  (for [prefecture prefectures
                        :let [prefecture-name (map-title prefecture)
                              prefecture-url (map-href prefecture)]
                        :when (re-find #"3\d{2}\.html" prefecture-url)]
                    {:prefecture prefecture-name
                     :url prefecture-url
                     :processor ::prefecture-page}))))

(defn- forecast-parser [node]
  (let [weather (first (html/attr-values (first (html/select node [:.weather :img])) :title))
        info (html/text (first (html/select node [:.info])))
        rain (apply hash-map (map html/text (html/select node [:.rain :table.rain :tr :td])))
        temp (->> (partition 3 (map html/text (html/select node [:.temp :table.temp :tr :td])))
                  (map #(interleave [:city :min :max] %))
                  (map #(apply hash-map %)))]
    {:weather weather
     :info info
     :rain rain
     :temp temp}))

(s/defprocessor prefecture-page
  :cache-template "/tenki/map/:area/:prefecture"
  :process-fn (fn [res ctx]
                (let [forecast-table (html/select res [:table.forecast])
                      caption (html/text (first (html/select forecast-table [:caption])))
                      forecasts (partition 4 (html/select forecast-table [html/root :> :tr]))]
                  (for [[header today tomorrow _] forecasts
                        :let [small-area (html/text (first (html/select header [[:th.th-area] :> html/first-child])))]]
                    {:small-area small-area
                     :today (forecast-parser today)
                     :tomorrow (forecast-parser tomorrow)}))))

(defn scrape []
  (s/scrape (seed)))
