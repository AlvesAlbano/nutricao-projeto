(ns org.nutricao-projeto.core
  (:require [org.nutricao-projeto.alimento.alimento-controller :as alimento]
            [org.nutricao-projeto.exercicio.exercicio-controller :as exercicio])
  (:import [java.io InputStreamReader BufferedReader OutputStreamWriter PrintStream]
           [java.nio.charset Charset]
           [java.lang System]))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Consultar alimento")
  (println "2. Adicionar refeição")
  (println "3. Mostrar refeições do dia")
  (println "4. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (alimento/inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite o nome do alimento:")
        (let [nome (read-line)
              resultados (alimento/buscar-alimento nome)]
          (if (not (empty? resultados))
            (alimento/mostrar-alimentos-rec resultados 0)
            (println "Nenhum alimento encontrado.")))
        (recur refeicoes))

      (= opcao 2)
      (let [novas (alimento/adicionar-refeicao refeicoes)]
        (recur novas))

      (= opcao 3)
      (do
        (alimento/imprimir-refeicoes refeicoes)
        (recur refeicoes))

      (= opcao 4)
      (println "Saindo...")

      :else
      (do
        (println "Opção inválida.")
        (recur refeicoes)))))

(defn -main []
  (executar []))
