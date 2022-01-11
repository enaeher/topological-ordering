(ns determine-order)

(defn- remove-node-from-children [node-to-remove [node children]]
  [node (remove #(= node-to-remove %) children)])

(defn- is-leaf-node? [[node children]]
  (empty? children))

(defn- topological-ordering
  "Given a directed acyclic graph represented by a map of the form:

  {node [child-nodes ...]
   ...}

  Returns a topological ordering of all the nodes in the graph such
  that each node precedes its parent. This ordering is represented as
  a map whose keys are nodes and whose values are indexes into the
  ordering:

  {child-node 0
   another-child-node 1
   parent-node 2
   ...}

  Uses an approximation of Kahn's algorithm."
  [graph]
  (loop [topological-ordering-index 0
         topological-ordering {}
         graph graph]
    ;; grab any leaf node from the current graph
    (if-let [[leaf-node _children] (first (filter is-leaf-node? graph))]
      (recur
       ;; add that leaf node to the accumulated ordering
       (inc topological-ordering-index)
       (assoc topological-ordering leaf-node topological-ordering-index)
       ;; recur with a version of the graph with that node removed
       (->> (dissoc graph leaf-node)
            (map (partial remove-node-from-children leaf-node))
            (into {})))
      topological-ordering)))

(defn- get-all-dependencies [task-graph goal-tasks]
  ;; simple breadth-first traversal of the dependency tree to
  ;; accumulate all dependencies, ignoring order
  (loop [tasks-acc (set goal-tasks)
         selected-tasks goal-tasks]
    (if-let [dependent-tasks (seq (mapcat (partial get task-graph) selected-tasks))]
      (recur (apply conj tasks-acc dependent-tasks)
             dependent-tasks)
      tasks-acc)))

(defn determine-order [task-graph goal-tasks]
  ;; first, get all the dependencies (unordered)
  (->> (get-all-dependencies task-graph goal-tasks)
       ;; then sort by topological ordering
       (sort-by (topological-ordering task-graph))))
