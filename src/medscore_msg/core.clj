(ns medscore-msg.core
  (:require [clojure.math.combinatorics :as combo])
  (:require [random-seed.core :refer :all]) ;For pseudo-random seed
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:gen-class)
  (:import (simplenlg.lexicon.italian ITXMLLexicon)
           (simplenlg.lexicon.english XMLLexicon)
           (simplenlg.lexicon MultipleLexicon)
           (simplenlg.framework NLGFactory)
           (simplenlg.realiser Realiser)
           (simplenlg.features Feature Tense)))



;;;call simplenlg-it
;; lexicons
(def lexIta (new ITXMLLexicon))
(def lexEng (new XMLLexicon))
(def lexFoodIta (new simplenlg.lexicon.XMLLexicon "D:\\Universita\\Magistrale\\TesiDoc\\medscore-msg\\resources\\foodLexicon-ITA-01.xml"))
(def multiLexIta (new MultipleLexicon "it"))
(. multiLexIta addInitialLexicon lexFoodIta)
(. multiLexIta addFinalLexicon lexIta)
(def nlgFactory-ita (new NLGFactory multiLexIta))
(def nlgFactory-eng (new NLGFactory lexEng))

(def realiser  (new Realiser))

(set-random-seed! 42)


(defn nlgFactory-lang
  "select the simpleNLG factory based on the language"
  [language]
  (cond (= language :ita) nlgFactory-ita
        (= language :eng) nlgFactory-eng)
  )


(defn determiner
  ([kind language]
   (cond
     (= kind :this)  (if (= language :ita) "questo" "this")
     (= kind :def)   (if (= language :ita) "il" "the")
     (= kind :undef) (if (= language :ita) "un"  "a"))))



(defn lexicalize-concept [concept rand lang]
  (let [italian-standard-dictionary
        {
         :to-be ["essere" "risultare" "apparire"]
         :this ["questo""quello" "tale"]
         :dish ["menu" "piatto" "portata"]
         :choice ["scelta" "decisione" "idea"]
         :good ["buono" "giusto" "accettabile"]
         :very ["molto" "tanto" "super"]
         :very-good ["ottimo" "eccellente" "fantastico"]
         :too-much ["troppo" "assai" "estremamente"]
         :perfect ["perfetto" "ideale" "eccezionale"]
         :to-go ["andare" "suonare" "combinare"]
         :rich ["ricco" "consistente" "ampio"]
         :toorich ["abbondante" "eccessivo" "esagerato"]
         :poor ["povero" "misero" "scarso"]
         :okay ["bene" "correttamente" "giustamente"]
         :lightly ["leggermente" "lievemente" "appena"]
         :car ["carboidrati"]
         :lip ["lipidi"]
         :pro ["proteine"]
         :mscore ["Med Score"]
         :week ["settimana "]
         :of ["di"]
         :to-eat ["mangiare"]
         :portions ["porzioni"]
         :to-increase ["aumentare"]
         :to-decrease ["diminuire"]
         :cer ["Cereali", "Pasta"]
         :pot ["Patate"]
         :fru ["Frutta"]
         :veg ["Verdura"]
         :leg ["Legumi"]
         :fish ["Pesce"]
         :rmeat ["Carne rossa", "Carne"]
         :poul ["Carne bianca", "Pollame"]
         :ffdp ["Latte e derivati", "Latte e formaggi", "Latticini"]
         :number ["numero"]
         :you ["tu"]
         :must ["dovere"]
         :right ["giusto","corretto"]

         }
        english-standard-dictionary
        {
         :to-be ["be"]
         :this ["this"]
         :dish ["menu" "meal"]
         :choice ["choose" "decision" "idea"]
         :good ["good" ]
         :very ["very" ]
         :very-good ["great" "excellent" "fantastic"]
         :too-much ["really"]
         :perfect ["perfect" "ideal"]
         :to-go ["go" "sound"]
         :rich ["rich"]
         :toorich ["rich"]
         :poor ["poor" ]
         :okay ["well" ]
         :lightly ["lightly"]
         :car ["Carbohydrates"]
         :lip ["Lipids"]
         :pro ["Proteins"]
         :mscore ["Med Score"]
         :week ["week"]
         :of ["of"]
         :to-eat ["eat"]
         :portions ["portions"]
         :to-increase ["increase"]
         :to-decrease ["decrease"]
         :cer ["Cereals"]
         :pot ["Potatoes"]
         :fru ["Fruits"]
         :veg ["Vegetables"]
         :leg ["Legumes"]
         :fish ["Fish"]
         :rmeat ["Read meat"]
         :poul ["Poultry"]
         :ffdp ["Full fat diary products"]
         :number ["number"]
         :you ["you"]
         :must ["must"]
         :right ["right"]


         }]
    (cond (= lang :ita)
          (if (= rand :random)
            (rand-nth (concept italian-standard-dictionary))
            (first (concept italian-standard-dictionary)))
          (= lang :eng)
          (if (= rand :random)
            (rand-nth (concept english-standard-dictionary))
            (first (concept english-standard-dictionary)))
          ))
  )

(defn compute-main-answer
  "Define the type of the main answer based on medscore"
  [mscore]
  (cond (and (> mscore 0)(< mscore 12))  :very-bad
        (and (> mscore 11) (< mscore 23)) :not-good
        (and (> mscore 23) (< mscore 34)) :good
        (and (> mscore 34) (< mscore 45)) :very-good
        :else :great)
  )

(defn ms-text-planner
  "return the medscore and the main answer type"
  [mscore]
  (let [main-answer (compute-main-answer mscore)
        ms (str mscore)]
    {:mscore mscore
     :main-answer main-answer}
    )
  )

(defn compute-cat-order
  "If not present 5 or 4 or 3 -> order desc"
  [cat-dict]
  (cond (and (some #{:5} cat-dict)
             (some #{:4} cat-dict)
             (some #{:3} cat-dict)) {:ord :desc :cat-ordered (sort-by :score cat-dict)}
        :else {:ord :sandw :cat-ordered (sort-by :score cat-dict)}
        )
  )

(defn cat-text-planner
  "define the order of the message for the food category
  ordered desc or sandwitch"
  [cat-dict]
  (let [cat-ordered (compute-cat-order cat-dict)]
    cat-ordered
    )
  )

(defn ms-sentence-planner
  "Define the med score sentence plan
   based on text plan value
   ms-text-plan -> :mscore :main-answer"
  [ms-text-plan]
  (cond
    (= (:main-answer ms-text-plan) :very-bad)    {:type :declarative :content :very-bad :ms (:mscore ms-text-plan) }
    (= (:main-answer ms-text-plan) :not-good)    {:type :declarative :content :not-good :ms (:mscore ms-text-plan)}
    (= (:main-answer ms-text-plan) :good)        {:type :declarative :content :good :ms (:mscore ms-text-plan)}
    (= (:main-answer ms-text-plan) :very-good)      {:type :declarative :content :very-good :ms (:mscore ms-text-plan)}
    (= (:main-answer ms-text-plan) :great)          {:type :declarative :content :great :ms (:mscore ms-text-plan)}

    )
  )

(defn cat-sentence-planner
  "create 9 sentence plan
   one for each category cat-text-plan based on categ score
   cat-text-plan-> :order :cat-ordered
   if :score is 5 or 1 or 0 verb is :to-eat,
   else if category is more-is-better (:+) :increase
   else if category is less-is-better (:-) :decrease

   if :score is 4 :mod :lightly
   if :score is 2 :mod :greatly
   if :score is 1 :mod :much

   use the :cat name to build the pp"

  [cat-text-plan]
  (vec
    (map (fn [x] (hash-map :type :declarative
                           :verb (cond
                                   (or (= (:score (second x)) 5)
                                       (= (:score (second x)) 1)
                                       (= (:score (second x)) 0)) :to-eat
                                   (= (:dev (second x)) :+) :to-increase
                                   (= (:dev (second x)) :-) :to-decrease
                                   )
                           :mod (cond
                                  (= (:score (second x)) 2) :lightly
                                  (= (:score (second x)) 2) :greatly
                                  (= (:score (second x)) 1) :much
                                  )
                           :prep-of (:cat (second x))
                           :score (:score (second x))
                           :dev (:dev (second x))

                           )
           )
         (:cat-ordered cat-text-plan))
    )
  )






(defn lexicalize [input randomness language]
  (let [lexicon (lexicalize-concept input randomness language)]
    lexicon)
  )

(defn ms-quasi-tree-generator
  "Define the quasi-tree from the medscore sentence-plan
  ms-sentence-plan -> {:type :declarative :content ? :ms ? }

  We build the sentence:
  The Medscore (of this week) is :ms and is :content
  "
  [ms-sentence-plan rand lang]
  (let [nlgFactory (nlgFactory-lang lang)]
    (do
      ;; The Medscore (of this week) is :ms
      (def medscore-score (. nlgFactory createClause))
      (. medscore-score setVerb (lexicalize :to-be rand lang))
      (def subj (. nlgFactory createNounPhrase
                                      (determiner :def lang) (lexicalize :mscore rand lang)))

      (. subj addPostModifier
         (. nlgFactory createPrepositionPhrase  (lexicalize :of rand lang)
            (. nlgFactory createNounPhrase
               (determiner :this lang) (lexicalize :week rand lang)) ))
      (. medscore-score setSubject subj)

      (. medscore-score setObject (str (:ms ms-sentence-plan)))

      ;; (The Medscore) is not good
      (def medscore-eval (. nlgFactory createClause))
      (. medscore-eval setVerb (cond
                                 (= (:content ms-sentence-plan) :not-good)(lexicalize :to-go rand lang)
                                 :else (lexicalize :to-be rand lang)
                                 ))
      (def cobj (. nlgFactory createAdjectivePhrase
                   (cond
                     (or (= (:content ms-sentence-plan) :very-bad)
                         (= (:content ms-sentence-plan) :good)
                         (= (:content ms-sentence-plan) :very-good)) (lexicalize :good rand lang)
                     (= (:content ms-sentence-plan) :not-good) (lexicalize :okay rand lang)
                     (= (:content ms-sentence-plan) :great) (lexicalize :very-good rand lang)
                     )
                   ))
      (. medscore-eval setObject cobj)
      (if (= (:content ms-sentence-plan) :very-good)
        (. cobj addPreModifier (lexicalize :very rand lang))
        )
      (if (or (= (:content ms-sentence-plan) :very-bad)
              (= (:content ms-sentence-plan) :not-good))
        (. medscore-eval setNegated true)

        )
      ;;coordination for AND
      (def clause (. nlgFactory createCoordinatedPhrase))
      (. clause addCoordinate medscore-score)
      (. clause addCoordinate medscore-eval)
      clause
      )
    )
  )

(defn cat-quasi-tree-generator
  "Build the quasi-tree for a single category sentence plan
  Every sentence-plan contains
  :verb (:mod)  :prep-of


  Generate the sentences (You) :verb (:mod) the number of portions :prep-of "
  [cat-sentence-plan rand lang]
  (let [nlgFactory (nlgFactory-lang lang)]
    (do

      (def clause (. nlgFactory createClause))
      (. clause setVerb (lexicalize (:verb cat-sentence-plan) rand lang))


      (def subj (. nlgFactory createNounPhrase (lexicalize :you rand lang)))
      (. clause setSubject subj)
      (def cobj (. nlgFactory createNounPhrase
                   (determiner :def lang) (lexicalize :number rand lang)))
      (if (or (= (:score cat-sentence-plan) 5)
              (= (:score cat-sentence-plan) 1))
        (. cobj addPreModifier (. nlgFactory createAdjectivePhrase
                                  ;;createNounPhrase
                                  (lexicalize :right rand lang)))
        )

      (def cobj-post-modifier (. nlgFactory createPrepositionPhrase  (lexicalize :of rand lang)
                                 (. nlgFactory createNounPhrase
                                    (lexicalize :portions rand lang)) ))
      (def cat-name-post-modifier (. nlgFactory createPrepositionPhrase  (lexicalize :of rand lang)
                                     (. nlgFactory createNounPhrase
                                        (lexicalize (:prep-of cat-sentence-plan) rand lang)) ))
      (. cobj-post-modifier addPostModifier
         cat-name-post-modifier )

      (. cobj addPostModifier
         cobj-post-modifier )

      (if (:mod cat-sentence-plan) (. cobj addPreModifier (lexicalize (:mod cat-sentence-plan) rand lang)))
      (. clause setObject cobj)

      (if (or (= (:verb cat-sentence-plan) :to-increase)
              (= (:verb cat-sentence-plan) :to-decrease))
        (. clause setFeature
           (. Feature MODAL)
           (lexicalize :must rand lang))
        )

      (if (= (:verb cat-sentence-plan) :to-eat)
        (do
          (. clause setFeature (Feature/TENSE) (Tense/PAST))
          (. clause setFeature (Feature/PERFECT) true)
          (. clause setFeature (Feature/PROGRESSIVE) false)
          )
        )

      (. subj setFeature (Feature/ELIDED) true)



      {:score (:score cat-sentence-plan)
       :dev (:dev cat-sentence-plan)
       :qt clause}
      )
    )
  )

(defn add-coord
  "Add a coordinate sentence to principal coordinate sentences"
  [quasi-tree coord-principal]
  (let [
        obj (. quasi-tree getObject)
        post-mod (. obj getPostModifiers)]
    (do
      (. coord-principal addCoordinate (nth post-mod 0))
      coord-principal
      )

    )
  )

(defn quasi-tree-pp-agg
  "Returns a single qt obtained by coordinating the qts over the PP"
  [quasi-trees language]

  (let  [nlgFactory (nlgFactory-lang language)
         q-trees (vec (map #(:qt %) quasi-trees))
         ]
    (do
      (def clause (nth q-trees 0))
      (def post-mod )


      (def posts (. nlgFactory  createCoordinatedPhrase))
      (doseq [item (rest q-trees)]
        (def obj (. item getObject) )
        (def post-mod (. obj getPostModifiers))
        (def post-post-mod (. (nth post-mod 0) getPostModifiers))
        (. posts addCoordinate (nth post-post-mod 0)))
      (. (nth (. (. clause getObject) getPostModifiers)0) addPostModifier posts)

      clause  ) )
  )

(defn cat-quasi-trees-generator [cat-sentence-plans rand lang]
  (let [cat-quasi-trees (map cat-quasi-tree-generator cat-sentence-plans (repeat (count  cat-sentence-plans) rand) (repeat (count  cat-sentence-plans) lang) )
        sen (quasi-tree-pp-agg (vec (map (fn [x] (hash-map :qt (:qt x)
                                                           ))
                                         (filter (fn [x]
                                                   (and (= (:score x) 5)
                                                        (= (:dev x) :+)))
                                                 cat-quasi-trees) )) lang)]
    ;;aggregate qt for score 5
    sen
    )

  )


(defn createFeedbackMedMessages
  "This is the function called by the server in order to return the text
  message to the user. It assumes as input the score of the 9 food categories
  and the value for randomness and language
  Workaround: 11 integers "
  [cer pot fru veg leg fish rmeat poul ffdp mscore rand-comp ita-eng]
  ;;create a dict of categories {score name type}
  (let [cat-dict {:cer {:score cer :cat :cer :dev :+}
                  :pot {:score pot :cat :pot :dev :+}
                  :fru {:score fru :cat :fru :dev :+}
                  :veg {:score veg :cat :veg :dev :+}
                  :leg {:score leg :cat :leg :dev :+}
                  :fish {:score fish :cat :fish :dev :+}
                  :rmeat {:score rmeat :cat :rmeat :dev :-}
                  :poul {:score poul :cat :poul :dev :-}
                  :ffdp {:score ffdp :cat :ffdp :dev :-}
                  }
        ;;select randomness from input
        rand (cond (= rand-comp 0) :no-random
                   (= rand-comp 1) :random)
        ;;select language from input
        lang (cond (= ita-eng 0) :ita
                   (= ita-eng 1) :eng)
        ]

    (str (. realiser realiseSentence (ms-quasi-tree-generator (ms-sentence-planner (ms-text-planner mscore)) rand lang))
         " "
         (. realiser realiseSentence (cat-quasi-trees-generator (cat-sentence-planner (cat-text-planner cat-dict)) rand lang)))

    )
  )

(defn -main
  "I don't do a whole lot ... yet."
      ;;(println "Hello, World!")
  [& args]
  (println "Hello, World!"))
