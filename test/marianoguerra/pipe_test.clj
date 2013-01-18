(ns marianoguerra.pipe-test
  (:use marianoguerra.pipe clojure.test))

(deftest pipe-test
  (testing "pipe of no funs returs result"
           (is (= (pipe 42) (ok 42))))
  
  (testing "piping one fun works"
           (is (= (pipe 42 #(ok %)) (ok 42))))

  (testing "mutation in fun works"
           (is (= (pipe 42 #(ok (+ % 1))) (ok 43)))
           (is (= (pipe 42 #(ok (+ % 1)) #(ok (* % 2))) (ok 86))))

  (testing "first non :ok fun returns"
           (is (= (pipe 42 #(ok (+ % 1)) #(error %) #(ok (* % 2)))
                  (error 43))))

  (testing "any non :ok result returns"
           (is (= (pipe 42 #(ok (+ % 1)) #(result :foo %) #(ok (* % 2)))
                  (result :foo 43))))

  (testing "first-ok-pipe"
           (is (= (first-ok-pipe 42 #(ok %) #(error %)) (ok 42)))
           (is (= (first-ok-pipe 42 #(error %) #(ok %)) (ok 42)))
           (is (= (first-ok-pipe 42 #(error (+ 1 %)) #(ok %)) (ok 42)))
           (is (= (first-ok-pipe 42 #(error %) #(ok %) #(error %)) (ok 42)))
           (is (= (first-ok-pipe 42 #(error %) #(error (+ 1 %))) (error 43)))))

