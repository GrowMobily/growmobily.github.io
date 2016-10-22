# Miscellaneous
* passwords
* mouse-avoidance
  * chrome
  * emacs
  * ubuntu
* emacs - don't cover old territory
  * my setup
* Touch Typing
  * gtypist
  * zty.pe
* Vimeo speed up - javascript: var v = document.querySelector('video'); var t = prompt('Set the playback rate'); v.playbackRate = parseFloat(t)

## Rationality
* well designed experiments, teasing apart difficult-to-test things

## Mystery
* mysteries are what inspires
* Peter Thiel divides things into easy goals, hard goals, impossible goals
* people frequently think hard things are solved, so all that's left is boring easy, and impossible
* de-motivating. 

## Success
* Why I will Succeed
  * I'm driven by a philosophy, (information/spirits/consciousness)
  * ex:
    *  paypal-new world currency
    *  elon musk - energy, existential crises

## Productivity
* The Journey Is The Reward
  * I've found it's hard for me to do things where 
     * I want the outcome
     * but I don't enjoy the journey
* Failure mode:
  * If there's something I "need" to do
  * And I don't want to do it
  * I torch time doing menial/unimportant things
  * AKA avoidance
* One productive mode:
  * If there's something I "want" to do
  * Even if I don't need to
  * I'll do it

* Brett Victor - Thinking Unthinkable Thoughts
  * Design Tool - http://idl.cs.washington.edu/projects/lyra/app/
    * https://github.com/vega/lyra
    * similar - https://github.com/laszlokorte/reform-swift
  
# Philosophy

* We're morphisms operating in contexts
  * drugs - highly depend on context
  * love - there's a line between adorable fawning and creepy fawning, and it's just the context of the pursuee's mind
  * 

# Meditation/Cognition

* focus/flow/attention is a high autotelic value, so train it (at least I think that works)
* we think in slots, roughly ~7
* math window - (2+3) > (3+5) > (5+8) > (8+3) > (3+1) > (1+4)
*               (7+8+5) > (8+5+0) > (5+0+3) > (0+3+8) > (3+8+1)
* build a simple app for them to play along from the post
  * actual window game (but point out it's hard since extra mental load
  * simple counter
* play for ten minutes with thing counter app, impossible to not have mental drift
* what's the max windows you can maintain

# Flow State

# DSLs
  * Free Monad + Interpreter
    * http://degoes.net/articles/modern-fp
    * http://programmers.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern
  * Denotational Semantics ???
  * DSLs and Towers of abstraction: https://vimeo.com/72870861
    * don't start with theories (syntax)
    * start with semantic domains (combinators)
    * write theories that match your domains
    * layer theories on theories, with each model disallowing more sentences, and providing more rules
    * include an AST - leave yourself open to multiple interpretations
  * Higher Order Abstract Syntax + Circular Programming
  * Freer Monad, Operational Monad
  

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

-- Data Type
data Blerg cont = A String String cont
                | B String        cont
                | C String        cont
                | End
                deriving (Show, Functor)
-- Monad
type BlergM = Free Blerg

-- Constructor Functions
a :: String -> String -> BlergM ()
a x y = liftF $ A x y ()

b :: String -> BlergM ()
b i = liftF $ B i ()

c :: String -> BlergM ()
c j = liftF $ C j ()

end :: BlergM ()
end = liftF $ End

-- Interpreter
interp :: Show n => BlergM n -> String
interp (Free (A x y c)) = "A " ++  x ++  y ++ interp c
interp (Free (B i c)) = "B " ++  i ++ interp c
interp (Free (C j c)) = "C " ++  j ++ interp c
interp (Free End) = ""
interp Pure {} = error "Program not ended!"

-- Example Code
test :: BlergM ()
test = do x <- a "apple" "angry"
          y <- b "basket"
          z <- c "chalice"
          end

## Context-Free Grammars
G = (N, E, R, S)
* N is a set of non-terminal symbols
* E is a set of terminal symbols
* R is a set of rules (like GEB's theorems?)
* S (is in N) is a distinguished start symbol



# Category Theory

## Comonad
* image analogy, and a blur function
  * `blur :: pixel -> pixel` in a context
  * not `image -> image`
  
* example from `Tel` on `functional programming` slack
```
;; doesn't exactly work though
(defn extract [[image pos]] (image pos))
(defn extend [k]
  (fn [[im pos0]]
    (let [new-im (fn [pos1] (k [im pos1]))]
      [new-im pos0])))
(defn blur-pixel [[im [x y]]]
  (/ (+ (im [x       y      ])
        (im [(inc x) y      ])
        (im [(dec x) y      ])
        (im [x       (inc y)])
        (im [x       (dec y)]))
   5))
(defn blur-image [im] (first ((extend blur-pixel) [im [0 0]])))

(defn one-image [[x y]] (-> [[1 2 3]
                             [4 5 6]
                             [7 8 9]]
                            (get x)
                            (get y)))
       ```
       
## misc

* Flow Chart : http://www.johndcook.com/blog/category_theory/
* Adjoints
  * 4.5 -> 5 = lost info
  * 4.5 -> 5 -> 5.0 -> 5 -> 5.0 = you stop losing info
  * code -> pretty print = lost spacing info
  * c -> p -> c -> p = you stop losing info
* .*jections : https://jeremykun.com/2011/07/09/set-theory-a-primer/
  * range - {f(x) | x E S}, or all of the f(x)s
  * codomain - bigger than, and contains, range
  * my own ex: for R1, consider (*2). 
    * Type is Int -> Int. 
    * Range is {evens}
    * Codomain is Ints
  * surjection - 
    * range = codomain
    * for every `y` in codomain, there is some x where f(x)=y (ie the range)
  * injection  - 
    * no 2 x's map to the same y
    * ie: if f(a) = f(b), then a = b.
    * floor(1.2) = floor(1.9), a != b, not an injection
  * bijection
    * "relabeling"
    * surjection + injection
  * infinity
    * 2 infinite sets have equal cardinality when a bijection exists between them

## Products
* Universal Properties and Products
  * https://jeremykun.com/2013/05/24/universal-properties/
* Product
  * http://www.johndcook.com/blog/2016/03/22/categorical-products/
* Set's initial = ({}? or Null?) , (in haskell, it's `Void`, and `absurd` is what makes it that way (?))
* Set's final = {x} for x E <some set>, ie all singleton sets are final
* Poset's initial = smallest number (note some posets don't have a least element)
* initial = unique 0 -> C, for every object in C. EG 0 
* terminal = unique C -> 1
* GREAT TUTORIALS:
  * http://eed3si9n.com/learning-scalaz/Examples+of+categories.html
  
  ## Algebras
  ## Yoneda Lemma

## Lens/Traversable

* Traversable

```
λ> let deleteIfNegative x = if x < 0 then Nothing else Just x
λ> let rejectWithNegatives = sequenceA . fmap deleteIfNegative
λ> rejectWithNegatives [1,2,3]
   Just [1,2,3]
λ> rejectWithNegatives [-1,1,2,3]
   Nothing
```
```
λ> sequenceA [(Just 1), (Just 2)]
    Just [1,2]
λ> sequenceA [(Just 1), (Just 2), Nothing]
    Nothing
```

* Contravariant Functor


a -> b` has `a` in the ​_negative_​ position

# Clojure
## many nested loops simplified
```(let [rr (comp reverse range)]
     (loop [a   (rr 3)
            b   (rr 3)
            c   (rr 3)
            acc []]
       (cond
         (empty? a) acc
         (empty? b) (recur (rest a)
                           (rr (first a))
                           (rr (first a)) acc)
         (empty? c) (recur a (rest b)
                           (rr (first b)) acc)
         :else      (recur a b (rest c)
                           (conj acc (apply str (map first [a b c])))))))```
                           
## Function composition composition (.).(.)
    http://adit.io/imgs/lenses/inception.jpg
    http://i.imgur.com/5kaxrvW.jpg

    let c = (.) . (.)
    (c (+) (*)) 9 8 7 == 79 == 9*8 + 7

    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    (.) :: (d -> e) -> (c -> d) -> (c -> e)
    ... :: (de-cd-ce)-(bc-ab-ac)
    ... :: 

    user=> (def a (comp inc (partial * 2)))
    user=> (a 3)
    7
    user=> (comp comp comp)
    #<Fn@55152e61 clojure.core/comp[fn]>

    user=> ((comp comp comp) a a)
    #<Fn@36630b56 clojure.core/comp[fn]>

    user=> (((comp comp comp) a a) 3) ; 3 -> 6 -> 7 -> 14 -> 15
    15

    user=> ((comp a a) 3)

    ((.) . (.)) (*10) (+) 3 5 --> 80
    (((comp comp comp) (partial * 10) +) 3 5) ;; --> 80


    ((.).(.)) show (+) 11 22
    "33"

# IO Monad
```
(def io-monad
  (let [pure (fn [v] (fn [] v))
        bind (fn [io f] (fn [] (f (io))))]
    {:pure    pure
     :bind    bind
     :echo    (fn [])
     :readln  (fn [])
     :println (fn [])}))
((:pure io-monad) "hi")
(((:pure io-monad) "hi"))

(((:bind io-monad)
  ((:pure io-monad) "hi")
  io-prn))

(defn io-prn
"String -> IO ()"
  [& args]
  (apply prn args))
(io-prn "hi")

(defn io-read
  "IO String"
  []
  (read-line))
(io-read)

((let [>>=  (:bind io-monad)
       pure (:pure io-monad)
       wow  (fn [x] (str x "!!!"))
       a    (>>= io-read wow)
       b    (>>= a io-prn)]
   b))


(defn io-echo []
  (let [>>=  (:bind io-monad)
        pure (:pure io-monad)]
    (>>= io-read io-prn)))

((io-echo))
```


