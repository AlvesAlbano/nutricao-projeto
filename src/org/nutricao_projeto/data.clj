(ns org.nutricao_projeto.data
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)))

(def data-formato (DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(defn intervalo? [data data-inicio data-fim]
  (and (or (.isEqual data data-inicio) (.isAfter data data-inicio))
       (or (.isEqual data data-fim) (.isBefore data data-fim))))

(defn filtrar-data [item data-inicio data-fim]
  (let [data (LocalDate/parse (:data item) data-formato)]
    (intervalo? data data-inicio data-fim)
    )
  )

(defn entre-datas [lista data-inicio data-fim]
  (filter #(filtrar-data % data-inicio data-fim) lista)
  )