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
(count me)

({:hi "whats up?"} :hi)




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

(list-all-primes-till 14);; Took me 2 hours to figure this out Dammnnnn


;; different way of checking for primes using sequences
(defn new-prime-checker
  [teee]
  (every? false? (map #(= (rem teee %) 0) (range 2 teee))))

(defn refactored-prime-checker
  [num]
  (if (= num 1)
    false)
  (->> (range 2 num)
       (map #(= (rem num %)))
       (every? false?)))

(refactored-prime-checker 1)



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

(javadoc java.util.Scanner);; will take you to the doc page for Scanner

;; -------------------------------------------------------------------
;; Comments- anything after ; is ignored
;; can also have multiline comments using
(comment
  (def ignore-me 10))
;; the above code is ignored but it is read by the Clojure reader and
;; therefore it must be correct

;; -------------------------------------------------------------------
