(ns determine-order-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [determine-order :as subject]))

(def example-tasks
  {"make a sandwich" #{"buy groceries"}
   "buy groceries" #{"go to the store"}
   "go to the store" #{}   })

(def complex-example-tasks
  {"make a pie" #{"buy groceries" "buy pie pan" "preheat oven"}
   "buy groceries" #{"go to the store"}
   "buy pie pan" #{"go to the store"}
   "go to the store" #{}
   "preheat oven" #{}})

(deftest determine-order
  ;; supplied example tests
  (is (= ["go to the store" "buy groceries" "make a sandwich"]
         (subject/determine-order example-tasks ["make a sandwich"])))
  (is (= ["go to the store" "buy groceries" "make a sandwich"]
         (subject/determine-order example-tasks ["buy groceries" "make a sandwich"])))

  ;; When there is more than one valid path through the dependency
  ;; tree, the precise order is not deterministic, i.e. there are
  ;; multiple valid topological orderings of the DAG, so the test
  ;; needs to be a little more complicated.

  ;; The approach here is to assert that, for each task in the task
  ;; list, each of its immediate dependencies precedes it.
  (let [ordered-tasks (subject/determine-order complex-example-tasks ["make a pie"])]
    (dotimes [i (count ordered-tasks)]
      (is (set/superset? (set (take i ordered-tasks))
                         (set (get complex-example-tasks (nth ordered-tasks i)))))))

  (let [ordered-tasks (subject/determine-order complex-example-tasks ["make a pie" "go to the store"])]
    (dotimes [i (count ordered-tasks)]
      (is (set/superset? (set (take i ordered-tasks))
                         (set (get complex-example-tasks (nth ordered-tasks i))))))))
