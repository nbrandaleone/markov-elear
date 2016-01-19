(ns markov-elear.generator)

(def example "And the Golden Grouse And the Pobble who")

(def words (clojure.string/split example #" "))

(def word-transitions (partition-all 3 1 words))
;; -> (("And" "the" "Golden")
;;     ("the" "Golden" "Grouse")
;;     ("Golden" "Grouse" "And")
;;     ("Grouse" "And" "the")
;;     ("And" "the" "Pobble")
;;     ("the" "Pobble" "who")
;;     ("Pobble" "who")
;;     ("who"))

(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with clojure.set/union r
                      (let [[a b c] t]
                        {[a b] (if c #{c} #{})})))
          {} word-transitions))
; {["who" nil] #{},
;;  ["Pobble" "who"] #{},
;;  ["the" "Pobble"] #{"who"},
;;  ["Grouse" "And"] #{"the"},
;;  ["Golden" "Grouse"] #{"And"},
;;  ["the" "Golden"] #{"Grouse"},
;;  ["And" "the"] #{"Pobble" "Golden"}}

(defn text->word-chain [s]
  (let [words (clojure.string/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn chain->text [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn generate-text
  [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file [fname]
  (text->word-chain
   (slurp (clojure.java.io/resource fname))))

(generate-text "And the" (process-file "quangle-wangle.txt"))
;; -> "And the Attery Squash, and the Bumble-Bee,
;;     The Frog and the Bisky Bat,-- All came and built on the
;;     Crumpetty Tree
;;     The plainer than ever it"
