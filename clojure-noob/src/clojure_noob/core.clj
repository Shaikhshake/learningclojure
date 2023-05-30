(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Wassupppppppp"))

(println "Now i've got this running")

(defn train
  []
  (println "choo choo"))

(str "hi " "how are you?");; valid but useless

(if false
  (+ 1 1)
  (+ 2 3))
;; note that the empty list isn't false
(if ()
  (str "Nothing other than nil and false are falsy")
  (str "This part is useless"))


;;predicate is function that either returns true or false
;; common practice to name predicates with trailing Q marks
;;example
;; true?-exactly true false?-exactlyfalse nil? zero? every?-is func true for all arg
(every? true? [nil]);; if you want to delve deeper, which you should,
;; use (find-doc #"\?$"), just know that there's a lot of them, loooooootttss
(symbol? 'hello)
(keyword? :a)





;; use the do operator when you 
;; want to do multiple things in an if 
;; statement block
(if true
  (do (println "This is still not very intuitive") "By the Gods!!")
  (do (println "perhaps more practice is needed") "What else is there to do?"))

;;below code wont compile
;; (if true
;;   "one"
;;   "two"
;;   "three")

;; --------------------------------------------------------
;; can use when, it is a combination of if and do without 
;; an else
(when true
  (println "this")
  (println "that")
  "andthat");;  returns "andthat"
;; checking what happens if I give three
;; values in an if statement

;; this wont do anything since nil is falsy
(when nil
  (println "does this work?"))


;; --------------------------------------------------------
;; nil values represent no value
(nil? 1);; will give false
(nil? nil);;will give true
;;note that nil is falsy, so is false

(if ""
  "Empty string is truthy"
  "Empty string is falsy");; "" evaluates to true

(if 0
  "0 is truthy"
  "0 is falsy");; 0 evaluates to true

;; --------------------------------------------------------
;; note that = is clojure's equality operator
(= "i" 1); gives false
(= 1 nil);
(= nil nil);gives true

;; --------------------------------------------------------
;; clojure has and , or as its logical operators
;; or returns the first truthy value, if all are falsy, it
;; returns the last value
;; and returns the first falsy value , if all are truthy,
;; it returns the last value



;; --------------------------------------------------------
;; binding the name to value in clojure

(def wow
  ["one", "two", "three"]);
wow;;
;; note calling (wow) doesn't work, i think cuz () are for
;; functions only


;; --------------------------------------------------------
;; all data structures are immutable

;; lets look at numbers, namely ints, floats, and ratios
1;;
13.2222;
1/5; ratio, clojure can work with it directly


;; take a look at strings, they're allowed to span multiple lines
"Im a string";
"I'm a string too";note the ' in between the "" 
"I\"m a string as well"; \ is the escape character, common
;; only double quotes are allowed for delineating strings 
"I'm a \n multiline string"
"This is 
 also a multiline string"
(println "Another
    multiline string 
      but printed 
          this time")
;;most common string function
(str "e" 1 1 nil 33 nil "boogie");; ignores any nils
;;Clojure characters are java characters, literal syntax- \{letter}
;; or for specialcharacters \backspace, \formfeed, \newline ,\return \space \tab
(str \n \o \t \space \f \u \n);
(str \newline); gives \n

;;example
(def elephantName "Jumbo")
(str "what is your name? -" elephantName)


;; Keywords, these are like a symbol except they begin with a :
:Keyword ;All keywords resolve to themselves

;; --------------------------------------------------------
;; vectors, indexing starts at zero, conj puts in at the
;; end, can use get
[1 2 3]
(count [1 1 2 3])
(count (set [1 1 2 3]))
(get [1 2 3] 1);
(conj [1 3 4] 44.4); will join at the end
(get [1 2 33] 4 "nothing"); I've provided a default value
(get [1 :c "pubg"] :D "not found")
;;can make a vector like this
(vector "hi" {:a "see"});;
(vector '(1 2 3));; converts into a vector of single element being [(1 2 3)]
;; ?? what is all this?
(into [] (set {:a :b}))
(into [1] (list [1 2]))


;; lists, indexing starts at 0, cant use get, use nth
;; also conj adds in the beginning, nth is slower than
;; get in vectors
(quote (1 2)); also works and the next line is just shortcut
'(1 2); list literals must start with '
(list 1 2 3 4);
(get (nth '(1 {:a "string" :b {:c "madness"}}) 1) :b);;
(nth '(1 3 4) 3 "3rd index doesn't exist")
(conj '(2 44 "string") 888);; will add at the start
(list [1 2 3 4]);; convert it into a list of single element benig ([1 2 3 4])


;; NOTE: Maps and sets have a reliable traversal order, but that order depends
;;       on the implementation details, and cannot be relied upon, or predicted
;; TO AVOID THIS, use sorted maps and sorted sets



;; sets - a. Hash-sets, b.Sorted-Sets
;; hashsets are shown here
;; literal notation for hashset
#{"velma blight" 22 33.33}
(hash-set 22 22 2 "bingobong");; repeated elements are treaed as one

(conj #{1 2 3 4} 5);; conj adds at the end
(conj #{1 2 3} 2);;wont conjoin anything

(contains? #{2 33 3 4 "hi" "what"} "hii");
(contains? #{nil} 2)
(contains? #{:a {:b nil}} nil)

(get #{:a {:c "wanderer"} :b} :a)
(get #{:a nil} nil)
(get #{:a {:b "test"}} :a)
(set [1 2 2]);; converts it into a set
(set '(1 3 3 3 4));; converts it into a set

;; sorted sets - will sort values by their natural order
(sorted-set "one", "two" "three" "four"); #{"four" "one" "three" "two"}


;; Maps, sorted or hash

{1 2};; map literal
{"string" "this"};; just {"string"} gives an error
;; but here "string" is mapped to "this"
(get {"string" "this"} "string");; will give "this"
;; can also nest

(def me {:first-name "shaikh" :last-name "shake" :address {:locality "House p near xyz" :district "smol" :state "big"}})
(get-in me [:address :locality])
(count me)

({:hi "whats up?"} :hi)

;; sorted maps- will come back sorted according to key
(sorted-map :1 "1" :2 "2" :3 "3");{:1 "1", :2 "2", :3 "3"}
(sorted-map :now "is" :not "the" :time "?");{:not "the", :now "is", :time "?"}






;;---------------------------------------------------------
;; functions 

(+ 1 2 3 4)
(first '(1 2 3))
(last [1 2 3 3])
(or + -)
;;(1 2);; will not compile since we're trying call the function 1 using argument 2


(inc 2.2)
(map inc [1 2])


(defn no-params
  "This function takes no parameters"
  []
  (str "Hah, I take no params"));; 0 arity
(no-params)

(defn one-param
  "Just one parameter"
  [one]
  (str "Give me the single param " one " and I shall use it"));; one arity
(one-param "two")


(defn two-param
  "Two parameters, how unique"
  [x y]
  (str "two parameters nice and free, two params squashed together " x y)) ;; two arity;;;
(two-param "testing" "Vesting")

;; behold arity overloading

(defn multi-arity-party
  ([first second third]
   (str first " " second " " third))
  ([first second]
   (str first " " second))
  ([first]
   (str first)))

(multi-arity-party "hi")
(multi-arity-party "hi " " how")
(multi-arity-party "hi " "how " "are")
;; below code wont compile cuz of arity violation
;;(multi-arity-party "hi " "how " "are" " you?")

(defn giveName
  ([name burgerType]
   (str name " likes " burgerType))
  ([name]
   (giveName name "cheeesburger")))

(giveName "rahul" "hamburger")
(giveName "pinki")


;; variable arity functions
(defn variable-arity
  "This is so cool"
  [& numbers]
  (map inc numbers))
(variable-arity 1 2)
(variable-arity 1 2 3 4 5 6 7)

(defn one-tenth
  "This function divides by 10"
  [x]
  (/ x 10))
(defn jaado
  "will use one-tenth to modify a vector"
  [& numberVector]
  (map one-tenth numberVector))
(jaado 1 2 3)
(jaado "1");; wont work, cant divide string by a number

;;mixing variable and fixed arity
(defn mix-match
  [first second & remaining]
  (println (str "heres the first thing " first))
  (println (str "here's the second thing " second))
  (println (str "heres the remaining stuff: " (clojure.string/join remaining))))

(mix-match "hi" "how " " are you?" "Okay")










;; ----------------------------------------------------------
;;anonymous functions
;; heres the syntax
;; (fn [param-list]
;;   function body)

(map (fn [value] (println (str value " bing!"))) ["Rahul" "Kshitiz" "Bing" "Bong"])
;; can also bind it to a name
(def what-a-lang (fn [name] (println (str "hope its been a nice day Mr/Miss " name))))
(what-a-lang "Shaikh")

;; take the following function
(fn [value] (* value 2));; multiplies by two
;;more compact way of writing this is
#(* % 2) ;; here % is the placeholder for the variable 
;; interchanging the locations of % and 2 works as well 


(defn custom-adder
  "this function adds some specied value to a given number"
  [value]
  #(+ value %));; changed the location of %

(def incby10 (custom-adder 10))

(map incby10 [10 20 22 2.22 2222])

;; you can also pass multiple values to an anonymous function
(#(println (str "hi " %1 " bye " %2)) "Shojo" "Shoyo")

(defn make-greeter
  "This function creates greeter function based on your liking"
  [greeting]
  #(str greeting " " %))

(def hello-greeter (make-greeter "hello"))
(hello-greeter "shakalaka")
(def cursing-grandma (make-greeter "wut in tarnation"))
(cursing-grandma "grandson")


;; ----------------------------------------------------------
;; let variables are immutable and local
(let [x 2] x) ;; value of let for is the last thing they evaluate
(let [x 4] (println x) 5);; this will demonstrate that x in this scope is 4 but the value of let form is 5
(let [x 2 y 4] (str "x : " x ", y: " y))


;; ----------------------------------------------------------
;;loops
(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration 4)
    (println "goodday")
    (recur (inc iteration))))

(defn my-loop
  "This function loops through four times, printing Value of iteration each time
   and Good Day on it's last iteration"
  ([]
   (my-loop 0))
  ([iteration]
   (println (str "Iteration " iteration))
   (if (> iteration 4)
     (println "Good Day!")
     (my-loop (inc iteration)))))

(my-loop)
(var my-loop)

(defn increasing-stars
  [num-of-lines]
  (loop [star-line "" n num-of-lines]
    (if (<= n 0)
      "Thanks for Playing"
      (do (println star-line)
          (recur (str star-line "*") (dec n))))))
(increasing-stars 3)

;; prints all primes till the given number
(defn list-all-primes-till
  [num]
  (loop [i 2]
    (loop [j 2]
      (if (= i j) (println j))
      (if (and (< j i) (not= (rem i j) 0))
        (recur (inc j))))
    (if (< i num)
      (recur (inc i)))))

(list-all-primes-till 1);; Took me 2 hours to figure this out Dammnnnn

(defn prime-checker-using-recur
  [num]
  (loop [i 2]
    (if (= i num)
      (println "Prime"))
    (if (and (> num i) (not (zero? (rem num i))))
      (recur (inc i)))))

(prime-checker-using-recur "")




;; different way of checking for primes using sequences
(defn new-prime-checker
  [teee]
  (every? false? (map #(= (rem teee %) 0) (range 2 teee))))
(new-prime-checker 10)

(defn refactored-prime-checker
  [num]

  (->> (range 2 num)
       (map #(= (rem num %)))
       (every? false?)))

(refactored-prime-checker 31)



;; ----------------------------------------------------------
;; For loops
;; in short, Clojure has no for loops and no direct mutable variables
(defn give-back-indexed
  [coll]
  (map-indexed vector coll))
(give-back-indexed {:a 1 :b 2}); ([0 [:a 1]] [1 [:b 2]])
(give-back-indexed '(1 2 3 3 4));([0 1] [1 2] [2 3] [3 3] [4 4])
(give-back-indexed "now is not a good time")

;; MUST COME BACK AND REVISIT FOR LOOPS in CHAPTER 2

;; ----------------------------------------------------------
;; regex

;; CANT UNDERSTAND ANYTHING, WILL COMEBACK TOMORROW
(re-find #"left" "popeye lefteye")

;;-----------------------------------------------------------
; two control precision
(+ 1 0.0000000000001M); add a M at the end
(* 1000N 10000 10000 1000 10000000000)

;;-----------------------------------------------------------
;; Vars - Are Bound To Names
;; object defined using defn or def are stored in a Clojure var, 
;; can be accessed using (var a-symbol) or more likely #'a-symbol
(def ten 10);
(var ten); #'clojure-noob.core/ten
#'ten ;; same as above #'clojure-noob.core/ten
;;note that the value of associated with ten (10) is different from its var


;; -----------------------------------------------------------
;; Bindings, note that vars are bound to names, but there
;; are other kinds of bindings another example is let binding
;; that you're already aware of.

;; -----------------------------------------------------------

;; Destructuring 

;; destructuring maps
(defn map-destructure
  "Lines below say bind one to the value of key :first and bind two to the value of the key :second"
  [{one :first two :second}]
  (println (str "first value is " one " and second value is " two)))

(map-destructure {:a 1 :second 2 :first "popeye"})


(defn map-destructure-with-modifications
  "Note that the words associated with the keys must exactly match the keys of map being passed"
  [{:keys [first second third]}]
  (println (str "first thing " first " second thing " second " third thing " third)))

(map-destructure-with-modifications {:third 1 :second 2 :first "popeye"})


;; Destructuring vectors
(defn vec-destructure
  [[first-thang second-thang & remainder-thangs]]
  (println (str "here the first and second thing of the vector: " first-thang " " second-thang))
  (println (str "and here the rest of em " (clojure.string/join remainder-thangs))))

(vec-destructure [1 2 3 4])
(vec-destructure [1])


;; it is also possible to bind both a collection and elements within it 
;; simultaneously
(let [[x y :as test] [1 2 3 4 5 6]] (str "x " x " y: " y " test: " (str  test)))

;; example showing how to skip elements form the start
;; suppose you only want to bind the third element
;; note that the underscore is assigned multiple times
(let [[_ _ _ k] [1 2 3 4]] (str "k: " k ", _ : " _))

(def testsymbol 10)
(resolve 'testsymbol)
#'testsymbol

;; -------------------------------------------------------------------
;; MUST STUDY NAMESPACES
;; https://clojure.org/reference/namespaces 
;; LOOK INTO THIS WHEN YOU GOT THE TIME OR THE ENERGY

;; -------------------------------------------------------------------
;; METADATA - data that is orthogonal to the logical value of an object
(meta #'str)

(meta #'+)
(meta #'+')
(meta #'str)

(var +)
#'+
(meta (var +'))
;; don't need to know how to add metadata yet, will come back if needed

;; -------------------------------------------------------------------
;; can Call java from clojure. Not really interested in using this.
;; just remember that you can call javadocs in clojure to 
;; find what something does in java, Example

;;(javadoc java.util.Scanner);; will take you to the doc page for Scanner

;; -------------------------------------------------------------------
;; Comments- anything after ; is ignored
;; can also have multiline comments using
(comment
  (def ignore-me 10))
;; the above code is ignored but it is read by the Clojure reader and
;; therefore it must be correct

;; -------------------------------------------------------------------
;;              Everything is a Sequence(Logical list)(NOT CONCRETE LISTS)
;; 3 core capabilities of sequence
;; a->get the first item, b-> get the rest of the items, 
;; c-> create a new sequence by adding an item to the front
;; Note- lazy sequence means a sequence not calculated until it is used

(seq? '(1 2 3));; true
(seq? [1 2 3]);; false

;; a
(first (vector 1 2 3))
(first {});; gives nil
(first #{:a "1" :b :c :d})
#{:a :b :c :d}

;; b
(rest #{1 2 3 22 11}); gives (2 3 22 11)
(rest '(1));; gives the empty seq i.e ()
(rest {});; gives the empty seq i.e ()
(rest #{:a "1" :b :c :d})

;; c - cons returns a new sequence where x is the first elem and
;;  seq is the rest, CONS stands for construct
(cons 2 #{1 2 3 4});; 
(cons '("hi-key" "bye-value") {:a "1" :b "2"}); 
;;(("hi-key" "bye-value") [:a "1"] [:b "2"])
(cons {"hi-key" "bye-value"} {:a "1" :b "2"})
;;({"hi-key" "bye-value"} [:a "1"] [:b "2"]);
(cons 1 '(1 2 3));; (1 1 2 3)

;; convert things to sequence, using (seq coll), get rest of the sequence
;; after the first element using (next aseq)< = (seq(rest aseq))>

(seq [1 2 3]);; gives (1 2 3)
(seq '(1 2 3));;same
(seq {:a "1" :b "2"});; ([:a "1"] [:b "2"]) -- seq of vectors of key-value pair
(seq #{:a :b :c :d :e})
(next {:a "1" :b "2"});; ([:b "2"])


;; conj(oin) -- used to add one or more elem to collection
;; (conj coll element & remainder)
;; note that conj will do the efficient thing, add in front of the list, 
;; add to the back of sets and vectors

(conj {})
(conj #{2} 1 2 3)
(conj {} [:a ""] [:b "sim"]);; {:a "", :b "sim"}


;; into adds all items from one collection into another
;; (into to-coll from-coll) (basic, see docs, dunno what a transducer is)

(into [1 2] '(1 2 3));[1 2 1 2 3] 
(into '(1 2 3) [1 2]); (2 1 1 2 3)
(into #{1 2 3} [1 2 3 4]); #{1 4 3 2}
(into #{1 2 3 4 5} '(22 33 22 44 22));; #{1 4 33 22 44 3 2 5}
(into '(1 2 3 4) #{22 1 3 4}); (3 22 4 1 1 2 3 4)


;; ----------------------------------------------------------------------
;;                    Clojure Sequence Library


;; range(start, end, step) , (range end), (range start end) - end is always exclusive
;; default to end is infinity, default for start is 0
(range 3);(0 1 2)
(range 2 10);(2 3 4 5 6 7 8 9)
(range 2 4 0.5);; (2 2.5 3.0 3.5)
(range 2 -1);() empty list
(range 2 -1 2); ()
(range 2 -1 -2);; (2 0) , step is negative, works
(range 2 4 -0.5); ()
;;if step is zero, then range will just give start an infinite number of times
(range 2 4 0);; (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...)


;; (repeat n x)- repeats element x n times
(repeat 10 :x); (:x :x :x :x :x :x :x :x :x :x)
(repeat 1);; will give an infinite sequence of 1's;


;; (iterate f x) --returns a seq x, (f x), (f(f x)).. f must be free of side-effects
(iterate inc 1); (1 2 3 4 ...) -infinite sequence
(iterate + 2); gives (2 2 2 2... ) cuz  (+ 2); =>2
(+ 2); 2
(iterate zero? (range -2 2))
(iterate zero? [1 2]);; will not work, x must be a single value, number
;;using this you can define the LIST OF WHOLE NUMBERS
(def whole-numbers (iterate inc 0))


;;(take n sequence) -- return n elems of sequence, all elems in n >count(seq)
(take 4 (iterate - 2));; =>(2 -2 2 -2)  ,cuz (- 2)=> -2, (- -2)=> 2
(take 10 (map zero? (range -2 2)));; (false false true false)

;; (cycle coll), takes a collection and cycles it infinitely

(cycle '(1)); (1 1 1 1 ...)
(take 11 (cycle (range 6))); (0 1 2 3 4 5 0 1 2 3 4)

(->> (range 6)
     (cycle)
     (take 11));; ; (0 1 2 3 4 5 0 1 2 3 4)


;; interleave - (interleave seq1 seq2)
;; interleaves two sequences(finite and infinite, doesn't matter)
;; first element of first seq, first elem of second, second elem of first
;; second elem of second seq, so on until one of the seq exhausts

(interleave [1 2 3] '("1" "2" "3" "4"));; (1 "1" 2 "2" 3 "3")
(interleave whole-numbers (range 0.0 10 0)); (0, 0.0, 1, 0.0, 2, 0.0, 3, 0.0, 4...)
(str "hi")

;; interpose - (interpose separator coll)
;; returns seq where each elem is segregated by separator
(interpose 1 '(1.3 2 3 4 5)); (1.3 1 2 1 3 1 4 1 5)
(interpose "," (take 10 whole-numbers))
; (0 "," 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9)

;; (apply fn args/list)
(apply str (interpose ", " (take 10 whole-numbers)))
;;"0, 1, 2, 3, 4, 5, 6, 7, 8, 9"
(->> (take 10 whole-numbers)
     (interpose ", ")
     (apply str));; same as above

;; generating all leaps years till the year 10_000
(defn leapYear?
  [num]
  (if (zero? (mod num 400))
    true
    (if (zero? (mod num 100))
      false
      (if (zero? (mod num 4))
        true
        false))))

(leapYear? 1)
(leapYear? 100)

;; DOESNT WORK, MUST COME BACK HERE
(defn list-leapYears-till
  [num]
  (let [coll (take num (iterate inc 0))]
    (println coll)
    (loop [years coll leapyears []]
      (if (nil? (first years))
        (println (str leapyears) " entered first if"))
      (if (and (not (nil? (first years))) (leapYear? (first years)))
        (do (println "enterd second if")
            (conj leapyears (first years))))
      (if (not (nil? (first years)))
        (do (println "third if")
            (recur (rest coll) (leapyears))))))) ;; this is giving me ArityException
(rest ())

(list-leapYears-till 5)

;; using filter
;; (filter pred coll) examples
(defn list-leapYears-using-filter
  [num]
  (->> (take num whole-numbers)
       (filter leapYear?)))
(list-leapYears-using-filter 100)

;; (take-while pred coll) keeps taking the elems of coll until pred returns false
(take-while leapYear? [4 104 204 400 100 2000 2008 2020 1000]);; (4 104 204 400)
;; execution stops since (leapYear? 100) is false


;; (drop-while pred coll) keeps dropping(not returning) from coll until pred returns
;; false and then returns the rest
(drop-while leapYear? [4 104 204 400 100 2000 2008 2020 1000])
; (100 2000 2008 2020 1000)
;; (leapYear? 100) returns false and drop-while returns the rest
(take 10 (drop 500 (filter leapYear? whole-numbers)))
;;(2060 2064 2068 2072 2076 2080 2084 2088 2092 2096)


;; split-at splits at an index, split-with splits using a predicate
(split-at 5 [4 104 204 400 100 2000 2008 2020 1000]);
;[4 104 204 400 100 2000 2008 2020 1000]
(split-with leapYear? [4  100 104 204 400 100 2000 2008 2020 1000])
;; [(4 104 204 400) (100 2000 2008 2020 1000)] 
;; split-with splits the list the moment the predicate returns a false
(split-with leapYear? []); => [()()] returns two empty seqs, keep that in mind



;; (every? pred coll) returns true if pred is true for every elem of coll
;; (some pred coll) returns true if pred is true for atleast one elem of coll

;; NOTE: be careful with these, they might go on forever.
;; like this (take-last 1 whole-numbers)
(every? even? whole-numbers);; false
(some leapYear? [1 4 100 200]); true;; note some is not a predicate and returns
;; first 
;; common way to use some to check set membership
(some #{20} [nil 0 -1 20 1 false]);; 20
(some identity [nil false 0 -1 20 1 false])

;; (not-every? pred coll)
;; (not-any? pred coll)
(not-every? even? [1 2 3 4 5]); true
(not-any? odd? [1 2 3 4 5 6]); false



;; TRANSFORMING SEQUENCES

;; maps - (map f coll & colls) -- will stop when the smallest coll is exhausted
(map leapYear? [1 200 1 4000 2 5]);; (false false false true false false)
(map #(format "<p>%s</p>" %1) ["This" "is" "a" "line"])
;; ("<p>This</p>" "<p>is</p>" "<p>a</p>" "<p>line</p>")

(map #(format "<%s>%s<%s>" %1 %2 %1) ["h1" "h2" "h3" "h4" "h5"] ["this" "is" "a" "line"]);; ("<h1>this<h1>" "<h2>is<h2>" "<h3>a<h3>" "<h4>line<h4>")

(map #(format "<%s>%s %s<%s>" %1 %2 %3 %1) ["h1" "h2" "h3" "h4" "h5"] ["this" "is" "a" "line"] (seq (take 20 (iterate str ":Bing!"))))
;;("<h1>this :Bing!<h1>" "<h2>is :Bing!<h2>" "<h3>a :Bing!<h3>" "<h4>line :Bing!<h4>")


;; reduce -- (reduce f coll) or (reduce f val coll)
;; f must take two args, reduce gives (f(f...(f x y) y<secondlast>) y<last>)

(defn sum-of-n-numbers
  [num]
  (->> (range 1 (inc num))
       (reduce +)))



  ;; (reduce + (range 1 (inc num))))
(sum-of-n-numbers 100)

(defn factorial
  [num]
  (->> (range 1 (inc num))
       (reduce *)))
(factorial 6)
(factorial -1)
(factorial 0)
(factorial 10)


;; sorting collection using (sort pred coll), or (sort coll) <natural order>
(sort > [1213 134141 1414 121 1 33 421]); (134141 1414 1213 421 121 33 1)
(sort [1213 134141 1414 121 1 33 421]); (1 33 121 421 1213 1414 134141)

;; (sort-by keyfn comparisonfn coll) (sort-by comparisonfn coll)
(sort-by :grade [{:grade 99} {:grade 100} {:grade 28} {:grade 99.5}])
;; ({:grade 28} {:grade 99} {:grade 99.5} {:grade 100})
(sort-by :grade > [{:grade 99} {:grade 100} {:grade 28} {:grade 99.5}])
;; ({:grade 100} {:grade 99.5} {:grade 99} {:grade 28})



;; IMPORTANT
;; List Comprehension
;; (for [binding-form coll-expr filter-expr? ..] expr)

;; for [each] word in [seq of words], format [according to format instructions]
(for [word ["Whatcha" "Upto" "to" "?"]] (format "<p>%s</p>" word))
;; ("<p>Whatcha</p>" "<p>Upto</p>" "<p>to</p>" "<p>?</p>")

(last (take 200 (for [n whole-numbers :when (leapYear? n)] n))); 820
;; returns the first 200 leap Years

;; :when selects while predicate is true
;; :while keeps evaluating until expression(think predicate) is true/ returns true


(for [n whole-numbers :while (even? n)] n);; (0) is return
(for [years [2000 2004 1100 4 8] :while (leapYear? years)] years);; (2000 2004)

;; can also use two collection binding
;; note that clojure iterates over the rightmost binding expression in a seq 
;; comprehension first then goes left
(for [letters "ABC" rank (range 1 4)] (format "%s%s" letters rank));
;;("A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3")
(for [rank (range 1 4) letters "ABC"] (format "%s%s" letters rank));
;;("A1" "B1" "C1" "A2" "B2" "C2" "A3" "B3" "C3")
(for [rank (range 1 3) letters "AB" symbols ",."] (format "%s%s%s" letters rank symbols));

;; :let works as a local binding, similar to clojure’s let .
;; :when works as a filter, skipping cases where loop variables don’t satisfy a condition(like continue in C or Java)
;; :when works as a test, ending the current loop early if loop variables don’t satisfy a condition(like break in C or Java). Notice that it doesn’t end the entire for loop, only the inner-most loop.

;; find right triangle where sum of sides is 24

(for [c (range 1 24)
      a (range 1 (inc c))
      b (range 1 (inc a))
      :when (= (+ (* a a) (* b b))
               (* c c))
      :when (== (+ a b c) 24)]
  [a b c])

;; WTFFFFFFF
;; (def primes
;;   (concat
;;    [2 3 5 7]
;;    (lazy-seq
;;     (let [primes-from
;;           (fn primes-from [n [f & r]]
;;             (if (some #(zero? (rem n %))
;;                       (take-while #(<= (* % %) n) primes))
;;               (recur (+ n f) r)
;;               (lazy-seq (cons n (primes-from (+ n f) r))))) wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
;;       (primes-from 11 wheel)))))


;; doall and dorun functions exists to deal with sideeffects, 
;; learn then later


;; dunno how to handle files, will need to learn using
;; some other resource, the text is driving me nuts on this topic
(import 'java.io.File)
(map #(.getName %) (.listFiles (File. "./src/clojure_noob")))
;; (".clj-kondo" ".lsp" "core.clj")

(seq (.listFiles (File. "./src/clojure_noob")))
;; (#object[java.io.File 0x68614184 "./src/clojure_noob/.clj-kondo"]
;;  #object[java.io.File 0x9324027 "./src/clojure_noob/.lsp"]
;;  #object[java.io.File 0x2c44ae5e "./src/clojure_noob/core.clj"])
(require '[clojure.java.io :refer [reader]])
(with-open [rdr (reader "./src/clojure_noob/core.clj")]
  (count (line-seq rdr)))



;; --------------------------------------------------------------------------
;;                   REVIEW ALL OF THIS
(defn trying-loop
  [coll]
  (loop [mylist coll sum 0]
    (if (empty? mylist)
      sum
      (recur (rest mylist) (+ (first mylist) sum)))))
(trying-loop '())
(conj '(1) 2)

;; REFACTOR THIS SHIT USING ->> magic
(defn stupid-fibb
  [num]
  (if (or (zero? num) (= num 1))
    [0])
  ;; (if (= num 1)
  ;;   "nothing")
  (last (take (- num 1) (take 10 (iterate #(conj % (+ (last (butlast %)) (last %))) [0 1])))))

(stupid-fibb 15) ;; WORKS

(defn sensible-fibb
  [num]
  (loop [fibseries [0 1] cnt 0]
    (if (< num 2)
      [0 1])
    (if (< cnt (- num 2))
      (recur (conj fibseries (+ (last fibseries) (last (butlast fibseries))))
             (inc cnt))
      fibseries)))
(sensible-fibb 5)
(fn [coll]
  (loop [workcoll coll mylist []]
    (if (empty? coll)
      (loop [fir (first workcoll) rem (rest workcoll)]
        (if (coll? fir)
          (recur () ()))))))

((fn [coll]
   (loop [workcoll coll mylist []]
     (if (empty? coll) workcoll)
     (if (empty? workcoll) mylist)
     (if (and (not (empty? workcoll)) (not (empty? coll)))
       (loop [fir (first workcoll) rem (rest workcoll)]
         (if (and (coll? fir) (not (nil? fir)))
           (recur (first fir) (next fir))
           (conj mylist fir)))
       (recur (rest workcoll) (mylist))))) [1 2 3 4])

(defn flatten-this
  [[h & rem]]
  (if h
    (if (coll? h)
      (concat (flatten-this h) (flatten-this rem))
      (cons h (flatten-this rem)))))
(flatten-this [1 [2]])

(Character/isUpperCase ["abbb"])
(.toUpperCase "a")
(apply str (filter #(if (= (.toUpperCase %) %) true false) (seq "aB")))


((defn ret-up [[fir & res]]
   (if fir
     (if (= (.toUpperCase fir) fir)
       (apply str fir (ret-up (())))
       false))) "aB")

(filter (fn [x] (Character/isUpperCase x)) "abCD &S")

(#(apply str (filter (fn [x] (Character/isUpperCase x)) %))
 "aBa(C)")
#(clojure.string/replace % #"[^A-Z]" "")
(apply str (set "aaaabbb"))


(defn compress [lst]
  (let [mylist [] one (first lst) remaining (rest lst)]
    (if (and one)
      (if (= one (first remaining))
        (do (cons mylist one)
            (println "one: " one ",mylist: " mylist ", 1sI,")
            (compress (rest remaining)))
        (do (conj mylist one)
            (println "one: " one ",mylist: " mylist ", 2sI,")
            (compress remaining)))
      (println mylist))))


(compress "ab")
(compress [1 2 2 2 3 4 4])
(first "ab")
(rest "ab")

;; problem is to write a function to remove consecutive duplicates from a 
;; sequence

;; 1. approach - for each elem add it to list and walk the list until you find a 
;;               diff elem and repeat till the end is reached
;;
;; DOESNT WORK               
;; (defn what
;;   [givenseq]
;;   (let [mylist []]
;;     (for [fir (first givenseq) :while (not (nil? fir))
;;           sec (first (rest givenseq)) :while (not (nil? sec))]
;;       (if (= (fir sec))
;;         (do (conj mylist fir)
;;             (what (rest givenseq)))
;;         (do (conj (conj mylist fir) second)
;;             (what (rest (rest givenseq))))))))

;; 2. use loop

;; DOESNT WORK
;; (defn what
;;   [givenseq]
;;   (let [mylist []]
;;     (loop [fir (first givenseq) mycoll givenseq]
;;       (if (and (not= fir (second mycoll)) (not (empty? mycoll)))
;;         (do (conj mylist fir)
;;             (recur (first (rest mycoll)) (rest mycoll)))
;;         (do (conj mylist fir)
;;             (loop [getridcoll mycoll]
;;               (if (= (first getridcoll) (second getridcoll))
;;                 (recur (next getridcoll)))))))
;;                 mylist))

;; need to come up with a func that takes elem and runs through the list
;; until it finds a mismatch


;; (reduce (fn [[fir & rem]]
;;           (if (and (= (fir) (first rem)) (seq (first rem)))
;;             (rest (rest rem))))  '(1 2 3 4 5))

;; the last expression returned by this fn will be the value of (reduce ...)
;; so we need to ensure that the value returned is the fillerlist, filled with 
;; elements
(defn wut
  [coll]
  (reduce (fn [fillerlist coll]
            (if (= (last fillerlist) coll)
              fillerlist
              (do (println "fl: " fillerlist " last fl: " (last fillerlist) ",coll: " coll)
                  (conj fillerlist coll))))  [] coll))



(defn re:wut
  [givenseq]
  (reduce (fn [fillerlist givenseq]
            (if (= (last fillerlist) givenseq)
              fillerlist
               (conj fillerlist givenseq))) [] givenseq))

(re:wut '(1 3 2 3 3 3 4 4 4 4 5))
(re:wut [[1 2] [1 2] [3 4] [1 2]]) ;

;; a function to pack consecutive duplicates into sublists
;; approach
;; 1 - write a sub function that compares the first element of the remaining coll
;; and the last element of the last sublist added

;; this is so close to being correct
;; DOESNT WORK!!
(defn packem
  [givenseq]
  (reduce (fn [fillerlist givenseq]
            (if (= (last (last fillerlist)) givenseq)
              (do 
                  (println "1IF->fl:" fillerlist ",lastfi:" (last fillerlist) ",givseq:" givenseq)
                  (conj (last fillerlist) givenseq)
                  )
              (do 
                  (println "2IF->fl:" fillerlist ",lastfi:" (last fillerlist) ",givseq:" givenseq)
                  (conj fillerlist (seq (list givenseq)))))) [] givenseq))
                
                

(packem '(1 2 3 4 4 4))

(=(last (last [])) '(1))
(conj [] (list 1))

;; (partition-by f coll) applies f to each element of coll and groups consecutiv
;; elems that give the same result

(partition-by #(> % 3) '(1 4 2 5 3 2 2 1)); ((1) (4) (2) (5) (3 2 2 1))
;; identity- (identity x) - returns x equivalent to (fn [x] x)
(identity 10); 10

;; answer to our Question
(defn short-packem
  [givenseq]
  (partition-by identity givenseq))

(short-packem '(1 3 3 1 1 2 2 3 4)) ;;((1) (3 3) (1 1) (2 2) (3) (4))

;; Write a function to Duplicate each element of the sequence

(reduce (fn [fillerlist givenseq]
          (apply conj fillerlist (repeat 2 givenseq)))
         [] '(1 2 3));



;;[1 1 2 2 3 3]

;; Write a function that replicates each element of a sequence of a variable number
;; of times

 
  (defn dup-what
    [givenseq num]
    (reduce (fn [fillerlist givenseq]
              (apply conj fillerlist (repeat num givenseq)))
            [] givenseq))
((fn [givseq num] (reduce (fn [fillerlist givenseq]
                            (apply conj fillerlist (repeat num givenseq)))
                          [] givseq)) [1 2 3] 3)


(dup-what [1 2] 3)





