(ns org.nutricao-projeto.core
  (:require [org.nutricao-projeto.alimento.alimento-controller :as alimento]
            [org.nutricao-projeto.exercicio.exercicio-controller :as exercicio])
  (:import [java.io InputStreamReader BufferedReader OutputStreamWriter PrintStream]
           [java.nio.charset Charset]
           [java.lang System]))
            [org.nutricao-projeto.exercicio.exercicio-operacoes :as exercicio]
            [org.nutricao-projeto.usuario.usuario-operacoes :as usuario])
  )

(defn menu-usuario[]
  (let [_ (println "Digite sua altura: ")
        altura (read)
        _ (println "Digite seu peso: ")
        peso (read)
        _ (println "Digite sua idade: ")
        idade (read)
        _ (println "Digite seu sexo: ")
        sexo (read)
        usuario {:altura altura
                 :peso peso
                 :idade idade
                 :sexo sexo}]
    (usuario/cadastrar-usuario usuario)
    (println "usuario cadastrado!" usuario)
  ))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Consultar alimento")
  (println "2. Adicionar refeição")
  (println "3. Mostrar refeições do dia")
  (println "9. Registar perda de caloria")
  (println "10. Obter extrato de perda calorica")
  (println "4. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (alimento/inteiro? entrada)
      (Integer/parseInt entrada)
      -1)
    )
  )

(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite o nome do alimento:")
        (let [nome-p (read-line)
              nome-e (trad/portugues-ingles nome-p)
              resultados (alimento/buscar-alimento nome-e)]
          (if (not (empty? resultados))
            (alimento/mostrar-alimentos-rec resultados 0)
            (println "Nenhum alimento encontrado.")))
        (recur refeicoes))

      (= opcao 2)
      (let [novas (alimento/adicionar-refeicao refeicoes trad/portugues-ingles trad/ingles-portugues)]
        (recur novas))

      (= opcao 3)
      (do
        (alimento/imprimir-refeicoes refeicoes)
        (recur refeicoes))

      (= opcao 4)
      (println "Saindo...")

      (= opcao 9)
      (do
        (println "Diga o nome do exercicio: ")

        (let [nome-exercicio (read-line)
              peso-usuario (usuario/get-peso)
              _ (println "Digite a duração do exercicio em minutos: ")
              duracao (read)
              _ (println (format "perda calorica baseada no seu peso atual %.2f Kg e tempo gasto %d min" peso-usuario duracao))
              lista-exercicios (exercicio/listar-exercicios nome-exercicio peso-usuario duracao)
              ;_ (println lista-exercicios)
              _ (println (exercicio/enumerar-exercicios lista-exercicios))
              _ (println "Escolha o exercicio realizado: ")
              indice (read)
              exercicio-selecionado (exercicio/selecionar-exercicio lista-exercicios indice)
              _ (println "Exercicio escolhido: "exercicio-selecionado)
              _ (read-line)
              _ (println "Informe a data em qual o exercicio foi relizado: (Ex: dia/mês/ano)")
              data (read-line)
              exercicio-com-data (exercicio/adicionar-data exercicio-selecionado data)
              _ (exercicio/registrar-perda exercicio-com-data)
              _ (println (format "Exercicio (%s) adicionado com sucesso!" exercicio-com-data))]
          )
        (recur refeicoes)
        )

      (= opcao 10)
      (do
        (println "Perdas caloricas registradas")
        (println (exercicio/calorias-perdidas))
        ;(exercicio/calorias-perdidas)
        (recur refeicoes)
        )


      :else
      (do
        (println "Opção inválida.")
        (recur refeicoes)))))

(defn -main []
  ;(menu-usuario)
  (executar [])
  )
