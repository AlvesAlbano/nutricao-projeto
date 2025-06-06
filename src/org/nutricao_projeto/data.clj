(ns org.nutricao_projeto.data
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))


(def data-formato (DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(defn intervalo? [data data-inicio data-fim]
  (and (or (.isEqual data data-inicio) (.isAfter data data-inicio))
       (or (.isEqual data data-fim) (.isBefore data data-fim))))

(defn filtrar-data [item data-inicio data-fim]
  (let [data-str (:data item)]
    (if (and data-str (not (str/blank? data-str)))
      (try
        (let [data (LocalDate/parse data-str data-formato)]
          (intervalo? data data-inicio data-fim))
        (catch Exception _ false))
      false)))

(defn entre-datas [lista data-inicio data-fim]
  (filter #(filtrar-data % data-inicio data-fim) lista))
