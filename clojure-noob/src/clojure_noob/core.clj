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


;; take a look at strings
"Im a string";
"I'm a string too";note the ' in between the "" 
"I\"m a string as well"; \ is the escape character, common
;; only double quotes are allowed for delineating strings 

;;example
(def elephantName "Jumbo")
(str "what is your name? -" elephantName)


;; --------------------------------------------------------
;; vectors, indexing starts at zero, conj puts in at the
;; end, can use get
[1 2 3]
(get [1 2 3] 1);
(conj [1 3 4] 44.4);
(get [1 2 33] 4 "nothing"); I've provided a default value
(get [1 :c "pubg"] :D "not found")
;;can make a vector like this
(vector "hi" {:a "see"});;
(vector '(1 2 3));; converts into a vector of single element being [(1 2 3)]
;; ?? what is all this?
(into [] (set {:a :b}) )
(into [1] (list [1 2]))


;; lists, indexing starts at 0, cant use get, use nth
;; also conj adds in the beginning, nth is slower than
;; get in vectors

'(1 2); list literals must start with '
(list 1 2 3 4);
(get (nth '(1 {:a "string" :b {:c "madness"}}) 1) :b);;
(nth '(1 3 4) 3 "3rd index doesn't exist")
(conj '(2 44 "string") 888)
(list [1 2 3 4]);; convert it into a list of single element benig ([1 2 3 4])



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


;; Maps, sorted or hash

{1 2};; map literal
{"string" "this"};; just {"string"} gives an error
;; but here "string" is mapped to "this"
(get {"string" "this"} "string");; will give "this"
;; can also nest

(def me {:first-name "shaikh" :last-name "shake" :address {:locality "House p near xyz" :district "smol" :state "big"}})
(get-in me [:address :locality])

({:hi "whats up?"} :hi)

;;---------------------------------------------------------
;; functions 

(+ 1 2 3 4)
(first '(1 2 3))
(last [1 2 3 3])
(or + -)
(1 2);; will not compile since we're trying call the function 1 using argument 2


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
(multi-arity-party "hi " "how " "are" " you?")

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

;; Destructuring vectors
(defn vec-destructure
  [[first-thang second-thang & remainder-thangs]]
  (println (str "here the first and second thing of the vector: " first-thang " " second-thang))
  (println (str "and here the rest of em " (clojure.string/join remainder-thangs))))

(vec-destructure [1 2 3 4])


;; Destructuring maps
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


;; ----------------------------------------------------------
;;anonymous functions
;; heres the syntax
;; (fn [param-list]
;;   function body)

(map (fn [value] (println (str value " bing!"))) ["Rahul" "Kshitiz" "Bing" "Bong"])
;; can also bind it to a name
(def what-a-lang (fn [name] (println (str "hope its been a nice day Mr/Miss " name)) ))
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
(#(println(str "hi " %1 " bye " %2)) "Shojo" "Shoyo")


;; ----------------------------------------------------------
(let [x 2] x) ;; value of let for is the last thing they evaluate
(let [x 4] (println x) 5);; this will demonstrate that x in this scope is 4 but the value of let form is 5


;; ----------------------------------------------------------
;;loops
(loop [iteration 0]
  (println (str "Iteration " iteration ))
  (if (> iteration 4 )
   (println "goodday")
   (recur (inc iteration))))

(defn my-loop
  ([]
   (my-loop 0))
  ([iteration]
   (println (str "Iteration " iteration))
   (if (> iteration 4)
     (println "Good Day!")
     (my-loop (inc iteration)))))

(my-loop)


;; ----------------------------------------------------------
;; regex

;; CANT UNDERSTAND ANYTHING, WILL COMEBACK TOMORROW
(re-find #"left" "popeye lefteye")

;;-----------------------------------------------------------


