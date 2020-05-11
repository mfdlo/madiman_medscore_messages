(ns medscore-msg.core-test
  (:require [clojure.test :refer :all]
            [medscore-msg.core :refer :all]
            ))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

;(deftest msTest
;  (testing "ms sentence"
;    (let [sent (createFeedbackMScoreMessage 13 0 0)]
;      (println sent)))
;  )

(deftest catTest
  (testing "cat sentences"
    (let [sent (createFeedbackMedMessages 4 2 4 5 2 3 5 5 1 44 0 0)]
      (println (str sent))))
  )


