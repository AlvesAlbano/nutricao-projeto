(ns org.nutricao-projeto.alimento.alimento-controller
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as str]))

(defn buscar-alimento [nome]
      (let [nome-enc (java.net.URLEncoder/encode nome "UTF-8")
            url (str "http://localhost:3000/alimento/" nome-enc)
            response (client/get url {:headers {"Accept" "application/json"}
                                      :as :json})]
           (:body response)))


(defn enviar-refeicao [refeicao]
  (client/post "http://localhost:3000/refeicoes"
               {:headers {"Content-Type" "application/json"
                          "Accept" "application/json"}
                :body (json/generate-string refeicao)
                :as      :json}))

(defn obter-nutriente [nutrientes nome]
  (if (empty? nutrientes)
    0
    (let [n (first nutrientes)]
      (if (= nome (:nutrientName n))
        (:value n)
        (recur (rest nutrientes) nome)))))

(defn imprimir-refeicoes [refeicoes]
  (println "---------------------------------------------------------")
  (println (format "%-10s %8s %10s %10s %10s %14s"
                   "Alimento" "Qtd(g)" "Calorias" "Proteína" "Gordura" "Carboidrato"))
  (println "---------------------------------------------------------")
  (letfn [(imprimir-rec [lst]
            (if (empty? lst)
              (println "---------------------------------------------------------")
              (do
                (let [r (first lst)]
                  (println (format "%-10s %8.1f %10.2f %10.2f %10.2f %14.2f"
                                   (:alimento r)
                                   (:quantidade r)
                                   (:calorias r)
                                   (:proteina r)
                                   (:gordura r)
                                   (:carboidrato r))))
                (recur (rest lst)))))]
    (imprimir-rec refeicoes)))

(defn inteiro? [s]
  (boolean (re-matches #"\d+" s)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn adicionar-refeicao [refeicoes traduzir-pt-en traduzir-en-pt]
  (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
  (let [linha (read-line)
        nomes-pt (clojure.string/split linha #",\s*")
        nomes-en (map traduzir-pt-en nomes-pt)]
    (letfn [(processar [alimentos nomes-pt acumulado]
              (if (empty? alimentos)
                acumulado
                (let [nome-en (first alimentos)
                      nome-pt (first nomes-pt)
                      resultado (buscar-alimento nome-en)
                      alimento (first resultado)
                      descricao-en (:description alimento)
                      descricao-pt (traduzir-en-pt descricao-en)
                      nutrientes (:foodNutrients alimento)
                      cal-100g (obter-nutriente nutrientes "Energy")
                      prot-100g (obter-nutriente nutrientes "Protein")
                      gord-100g (obter-nutriente nutrientes "Total lipid (fat)")
                      carb-100g (obter-nutriente nutrientes "Carbohydrate, by difference")]
                  (println (str "Digite a quantidade consumida em gramas de " descricao-pt ":"))
                  (let [qtd-str (read-line)]
                    (if (double-str? qtd-str)
                      (let [qtd (Double/parseDouble qtd-str)
                            fator (/ qtd 100.0)
                            nova-refeicao {:alimento descricao-pt
                                           :quantidade qtd
                                           :calorias (* cal-100g fator)
                                           :proteina (* prot-100g fator)
                                           :gordura  (* gord-100g fator)
                                           :carboidrato (* carb-100g fator)}]
                        (println (str "Adicionado: " descricao-pt " (" qtd "g)"))
                        (enviar-refeicao nova-refeicao)
                        (recur (rest alimentos) (rest nomes-pt) (conj acumulado nova-refeicao)))
                      (do
                        (println "Quantidade inválida. Pulando alimento.")
                        (recur (rest alimentos) (rest nomes-pt) acumulado)))))))]
      (processar nomes-en nomes-pt refeicoes))))


(defn mostrar-alimentos-rec [alimentos count]
  (if (or (empty? alimentos) (>= count 5))
    nil
    (let [alimento (first alimentos)
          descricao (:description alimento)
          categoria (:foodCategory alimento)
          nutrientes (:foodNutrients alimento)
          calorias (obter-nutriente nutrientes "Energy")
          proteina (obter-nutriente nutrientes "Protein")
          gordura (obter-nutriente nutrientes "Total lipid (fat)")]
      (println "===========================")
      (println "Nome: " descricao)
      (println "Categoria: " categoria)
      (println "Calorias: " calorias)
      (println "Proteína: " proteina)
      (println "Gordura: " gordura)
      (println "===========================")
      (recur (rest alimentos) (inc count)))))
