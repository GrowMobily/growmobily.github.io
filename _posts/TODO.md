# Blog Purpose + Strategy
* Purpose: networking? passive income? esteem? human-connection?
* Strategy:
    * post types: code. assessment of startups. philosophy. AI. experiments. health. nootropics. rationality. behaviorism. sales.

# Emacs Org-mode tips
* write this cuz I want to learn it!

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
* Process:
  * 0. How to make it safe to talk about conversation X
  * 1. If you were wrong about X, would you want to know?
  * 2. If you were wrong, how would you know?
  * 3. If you found you were wrong, what would it cost you?
* language = we can communicate our ideas to each other
* science = we can communicate objective ideas to each other

## Behaviorism

* high-level reference: http://danielbayn.com/deep-magic-of-behaviorism/
* V.R. +Reward for desired +behavior
* Continuous punishment for bad +behavior
* For learning new behavior, VR of 1:1 (ie continuous), and gradually increase 1:X

  
## Things that Surprise Me + Beliefs I've Updated + Expectation Violations
* If you told me about the lottery, I'd've said no one would do it
* If you told me the human race was intelligent enough to put computers in orbit around the planet, I'd've guessed that religion would be all but disappeared
* Bad attention is about inability to *direct* your attention, 
* Money isn't the most important
* Happiness isn't the most important
* Nutrition - WFPB is better than the western diet I was raised on
* Beard is ok
* Family isn't the most important
* Donating organs is a bit sketch. They'll harvest you while you're still alive (you needa be, lest your organs necrose)

## we're not the end product of a simulated universe
* which is more likely: 1. the universe is simulated, 2. the universe is simulated *and* we're the things the "creators" were interested in making

## Mystery + Curiosity
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
*  Happiness isn't acquired externally

# Passive Income Success Stories

* submithub
* park.io
* indiehackers.com
* 
    

## Tea
* Fruit Loops - hibiscus, dried orange peel, crushed up rose hips

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
  
# SciFi
* Para/Dia-gravity (analagous to magnetism)

# Science
* language of:
  * blind
  * crossover
  * longitudinal
  * case report
  * empiricism
  * randomized
  * observational study vs. experiment
  * control
  
# Philosophy

* We're morphisms operating in contexts
  * drugs highly depend on context
* Amoralism
* The Word "Should"
* Emotivism - right, wrong, should, ought, responsibility, obligation
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
* Peak brain state - water, sleep, caffeine, 

# Clojure Async Examples
```
(def chan-a (chan))
(def chan-b (a/map clojure.string/reverse [chan-a]))
(def chan-c (a/map clojure.string/upper-case [chan-b]))

(go (while true (println (<! chan-c))))
(go (>! chan-a "yppah"))

; => "HAPPY"
```

# Clojure efficiency

* macros for code gen anywhere
* key bindings for use in chrome: eg, console.log the state atom
* code quality tools, eg generate a pretty log of TODOs, FIXMEs, OPTIMIZEs, HACK, REVIEW, etc.
* visualize a web of the code

# Velocity vs. Position

    Change in Program = More Composable

* metaphor - velocity is to position, as
             composable is to imperative
* transducers
  * imperative - (map inc [1 2 3]) - represents data
  * composable - (map inc)         - represents a change in data
* effects/coeffects-
  * ???

# Re-Frame new features: CoEffects v Effects

```
;; Simple Side-effect ----------
;;   logging to `console`

(reg-event-fx
 :log
 (fn [cofx [dispatch-id v]]
   {:log v}))

(reg-fx
 :log
 (fn [x]
   (js/console.log x)))

(dispatch [:log {:test "yo" "hi" "there"}])


;; Simple Co-effect ----------
;;   grabbing a key from `localStorage`

(reg-event-fx
 :from-ls
 [(inject-cofx :local-store "cofx-message")] ; interceptor, like middleware that loads localStorage into coeffects (which is a map)
 (fn [cofx [dispatch-id]]
   (let [val (get-in cofx [:local-store])
         db  (:db cofx)]
     {:db (assoc db "cofx-message" val)})))

(reg-cofx
 :local-store
 (fn [coeffects k]
   (assoc coeffects
          :local-store
          (js->clj (.getItem js/localStorage k)))
   ))

(dispatch [:from-ls])

;; subscribe to the db entry which recieves the data from localStorage
#_(let [cofx-message (re-frame/subscribe ["cofx-message"])]
    (fn []
      [:p "localStorage's \"cofx-message\": " @cofx-message]))


;; Interceptor ----------
(defn update-value [fn-a fn-b]
  (->interceptor
   :id     "pre-append"
   :before (fn [ctx] (update-in ctx [:coeffects :event 1] fn-a))
   :after  (fn [ctx] (update-in ctx [:coeffects :event 1] fn-b))))

(let [pre-append  (fn [x] (fn [s] (str x s)))
      post-append (fn [x] (fn [s] (str s x)))]
  (reg-event-fx
   :gogogo
   [(update-value (pre-append "A") (post-append "Z"))
    (update-value (pre-append "X") (post-append "Y"))]
   (fn [cofx [_ v]]
     {:log v})))

(dispatch [:gogogo " hello "])
;; logs =>  "XA hello "
;;   notice, it does not include the `post-appends`/`:after` fns
;;   `:before` is generally for coeffects
;;   `:after`  is generally for effects
;; 1. `pre-append "A"`
;; 2. `pre-append "X"`
;; 3. `console.log(v)`
;; 4. `post-append "Z"`
;; 5. `post-append "Y"`
```

# Super Monads
https://www.youtube.com/watch?v=HRofw58sySw

Standard Monad
bind : m a -> (a -> m b) -> m b
return : a -> m a

Effect Monad
bind : m i a -> (a -> m j b) -> m (i <> j) b
return : a -> m e a

Hoare/Indexed/Parameterized Monads
bind : m i j a -> (a -> m j k b) -> m i k b
return : a -> m i i a

Constrained Monads
bind : (CtB a b) => m a -> (a -> m b) -> m b
return : (CtR a) => a -> m a

Super Monad
bind : (CtB m n p a b) => m a -> (a -> n b) -> p b
return : 




# Transducers

`(((map inc) conj) [1 2] 3) ; => [1 2 4]`

# Flow State
* number windows meditation

# Monads
* Indexed Monads
* Relationship to Arrows
* Relative Monads - http://arxiv.org/abs/1412.7148

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
  * Freer Monad, Operational Monad (Is Operational iso to Free?)
  
```
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
          ```
          
# Free / Cofree DSL

* ref : http://dlaing.org/cofun/posts/free_and_cofree.html

```
-- VERSION #1 --------------------------------------------------------------------------------
-- Notice : `k` is being used as "state", not as "next action" like it should be used as

data EF k = A k
          | B k deriving (Show, Functor)
type E k = Free EF k

a :: E ()
a = liftF $ A ()

b :: E ()
b = liftF $ B ()    

----------
    
data CoEF k = CoEF { aH :: k
                   , bH :: k } deriving (Show, Functor)
type CoE k = Cofree CoEF k

coA :: String -> String
coA x = x ++ "a"

coB :: String -> String
coB x = x ++ "b"

mkCoE :: CoE String
mkCoE = coiter next start
        where
          next w = CoEF (coA w) (coB w)
          start = "S: "

----------


            
instance Pairing CoEF EF where
    pair f (CoEF j _) (A k) = f j k
    pair f (CoEF _ j) (B k) = f j k
                              
abba = a >> b >> b >> a
abtest = pair (\a b -> (a, b)) mkCoE abba

```


```
-- VERSION #2 -----------------------------------------------------------------------------------------
-- Notice : `k` is being used as "state", not as "next action" like it should be used as

data EF k = A k
          | B k
          | C String k
             deriving (Show, Functor)
type E k = Free EF k

a :: E StateString
a = liftF $ A "change state to this, A was the last expression"

b :: E ()
b = liftF $ B ()    

c :: String -> E ()
c x = liftF $ C x ()    

----------
    
data CoEF k = CoEF { aH :: k
                   , bH :: k
                   , cH :: String -> k}
              deriving (Functor)
type CoE k = Cofree CoEF k

type StateString = String
    
coA :: StateString -> StateString
coA s = s ++ "a"

coB :: StateString -> StateString
coB s = s ++ "b"

coC :: StateString -> String -> StateString
coC s x = s ++ "(c:" ++ x ++ ")" 
        
mkCoE :: CoE StateString
mkCoE = coiter next start
        where
          next w = CoEF (coA w) (coB w) (coC w)
          start = "S: "

----------

instance Pairing CoEF EF where
    pair f (CoEF j _ _) (A k) = f j k
    pair f (CoEF _ j _) (B k) = f j k
    pair f (CoEF _ _ j) (C x k) = f (j x) k

----------

prog = a >> b >> b >> a >> c "Cat" >> a >> c "cow" >> b >> a
test = pair (\x y -> (x, y)) mkCoE prog
```

## Context-Free Grammars
G = (N, E, R, S)
* N is a set of non-terminal symbols
* E is a set of terminal symbols
* R is a set of rules (like GEB's theorems?)
* S (is in N) is a distinguished start symbol



# Category Theory

* An object in Hask is a type, like Int, [Bool], or [a]->Int. Types are nothing more than just sets of values - Bartosz
* monomorphism - if f.g = f.h, then g = h
  * f::B->C; g, h::A->B, monomorphism if *injective* on points of B
* epimorphism  - if g.f = h.f, then g = h
  * Every point of C is the image, under f, of some point of B
* [C,D] = functor category whose objects are functors C->D, and morphs are nat transformations
* Types of Morphisms:
  * Functional
  * Relational (and not functional)
  * the category where objects are natural numbers and the arrows `n -> m` are matrices of size `n x m`. matrix multiplication is composition
  * the category of free paths on a directed graph. a directed graph is obviously _sort of_ a category, but composition isn’t defined
* MultiCategories = Operads with typed inputs (bartosz)

## Adjunctions
* Of list monad
* `there exists` -| * -| `for all` -- `*` is functor between power sets https://www.wikiwand.com/en/Universal_quantification#/As_adjoint
* `there exists` -| `const` -| `for all`
* `sum` -| `const` -| `product`
* Of State monad (reader and writer. note that nlab calls writer a *comonad*)
* free monoid -| forgetful -- (list monad?) (`F: Set -> Mon`)
* left, *and* right, adjoints of `U: Grp -> Mon`
* colimit -| diagonal functor -| limit
* one adjoint triple gives rise to 2 monad-comonad pairs - http://comonad.com/reader/
* pull-back -| push-forward
* quotients -| truth/equality -| comprehension - http://www.euclideanspace.com/maths/discrete/category/comparing/adjunction/index.htm
* corresponding diagonal functor -| any limit functor
* Every adjunction 〈F, G, ε, η〉 gives rise to an associated monad 〈T, η, μ〉 in the category D.
  * monad   : 〈GF, η, GεF〉
  * comonad : 〈FG, ε, FηG〉
* Concept <---> Example, generalization:E->C, instantiation:C->E, generalization -| instantiation, -- https://ncatlab.org/nlab/show/generalisation+as+an+adjunction

## List Monad
* from adjoints: free monoid functor `Set->Mon`, and forgetful functor `Mon->Set`
* 

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
* Products in Categories
  * Cat. of Posets - product = greatest lowerbound, or greatest object lower than A and B in AxB
  * Cat. of Logical Predicates - product = conjuction, (morphism =? "implies")

        If a and b is true then a is true. Also, if a and b is true then b is true.
        These are the two projections.
      
        If you have another predicate c, and you can show that if c is true then a is true,
        and you can show that if c is true that b is true, then it 
        follows that if c is true that a and b is true. That’s universality.
        
  * Cat of Set of Natural Numbers - product = GCD (i think)
  
* Universal Objects
  * Cat of Monoids - Free Monoid
  * Cat. of F-Algebras - Initial Algebras (???)
  * Cat. of Sets - Initial= {}, Terminal={x}, terminal's unique morphism = `constant`

## Functors
* covariant, contra, bi, pro, representable, distributive (or co-traversable), diagonal, 
* in Haskell, hom-functor = reader functor
* `Op` = contravariant version of `Reader`
* `λ> Data.Set.map (const 99) (fromList [1,2,3])`
* `λ> fmap (const 99) [1,2,3]`
* C(a, _) = covariant functor (notice the positive position is the variant one, not the `a`)
* C(_, a) = contravariant functor
* C(_, _) = profunctor
  * C(_, _) :: Cop × C -> Set, contravariance is the same as mapping from the opposite category
* Representable - 
  * http://covariant.me/notes/rep-functors.html 
  * http://jozefg.bitbucket.org/posts/2013-10-21-representable-functors.html
  * ekmett - what functors are Representable? Anything that can be seen as a fixed shape with some index. Pairs, fixed-size vectors, fixed-size matrices, any nesting of fixed vectors and matricies. But also infinite structures of regular shape! However, not things whose shape can vary -- not lists, not sums. Trees of fixed depth or infinite binary trees therefore, but not trees of arbitrary depth or with ragged structure, etc.
  * all representables are distributive
  * For posets, Yoneda's lemma says that an object is determined up to isomorphism by the set of objects less than or equal to it (equivalently, the set of objects greater than or equal to it). - Quiaochu Yuan on SO
  * Identity is representable by `()`
  * `data Pair a = Pair a a` is representable by `Bool`
  * `data Triple a = Triple a a a` is representable by `data Three = One | Two | Three`
  * Streams can be represented by natural numbers
  * every representable is a monad, and is isomorphic to `Reader Monad`
  * `Maybe` and `Either` are non-representable
  * Example code:
  
        λ> let a x = x ++ "!"
        λ> let b x = x ++ "?"
        λ> let f x y = x ++ y
        λ> liftR2 f a b "representables ftw"
           "representables ftw!representables ftw?"
        λ> a "hi"
           "hi!"
        λ> index a "hi"
           "hi!"
        λ> (tabulate . index) a "hi"
           "hi!"
* given any object A in C^op,
  H_A : C^op -> Set
  X |-> C(X, A) -- (`|->` means "send it")
  f |-> _ . f -- (precomposition)

  H^A : C -> Set
  X |-> C(A, X)
  f |-> f . _
      
* H_. : C -> [C^op, Set]
  A |-> H_A
  f |-> (H_A -H_f-> H_B)
      
* Yoneda Embedding sends A into HomSet
  Given any A in C, we have H_A in [C^op, Set]
*


## Yoneda's Lemma
  * According to Yoneda's lemma, natural transformations from Hom(A,–) to F are in one-to-one correspondence with the elements of F(A).

```
(forall b. (a -> b) -> f b) ~ f a  -- these two types are isomorphic

fw :: (Functor f) => (forall b . (a -> b) -> f b) -> f a
fw f = f id

bw :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
bw x f = fmap f x

fw . bw = id
bw . fw = id

```


  

## Limits / CoLimits

* universal cone is terminal obj in Cat. of Cones
* ## Catster's Notes on Limits
   * morphism for `terminal` in `Set` is `map . (const x)`
   * `initial :: forall a. InitialObject -> a`
     * `newtype Initial = Initial { getInitial :: Initial }`
     * `absurd :: Initial -> a
        absurd = absurd . getInitial`
     * or `data Void`, with `absurd x = case x of {}`
   * limits examples - pullback (it's diagram="span"), product, terminal, equalizer
   * a limit for a diagram is a universal cone over it
   * terminal = limit, initial = colimit
   * equalizer - A set of values on which two functions are equal is called the equilizer
   * cone = vertex of cone is some object "u", base is every object in the diagram
     * every triangle in te projection map commutes
     * u ---> x
       u ---> y
       x -f-> y
     * it commutes
     * any other cone factors through the limit one
     * "factors through" means that theres a unique morphism going from the non-universal
       cone through the universal one, where every triangle commutes
     * a terminal object is a limit over the empty diagram
     * Equalizer = limit over parallel pair of arrows
     * Diagram in C of shape I is a functor D I -> C, I is a category (small or finite)
       I just gives you the shape of a diagram
     * pullbacks have applications for unification, and for class inheritance
     * pullback is type unification in type inference
     * pullback = SELECT ... FROM A, B WHERE A.x = B.y
     * coproducts = pushout from the initial object
     * coequalizer of f and g is pushout from [f, g] and [idf, idg] (?)
     
     ### CoLimits
     * pushout, sum, initial, 
     * 
     
     ### Hom-functor
     * (in haskell) mapping of any two types to a function type
     * or a parameterized function type
     
     * In Cat. of Sets, coproduct = disjoint union, (union, and indexed as to which parent set it came from) (disjoint means no elements in common, so disjoint union makes sense since, because they're indexed, they can't have anything in common)

## Algebras

## Kan Extensions

## Yoneda Lemma

## Reader-Env, Writer-Traced

## Bayesian Network using Haskell and Vector Monad
https://vimeo.com/6590617
https://dl.dropboxusercontent.com/u/828035/Computing/ICFP2009.pdf

## Logic Monad
https://vimeo.com/6590617
https://dl.dropboxusercontent.com/u/828035/Computing/ICFP2009.pdf

## Quantum Logic Monad
https://vimeo.com/6590617
https://dl.dropboxusercontent.com/u/828035/Computing/ICFP2009.pdf

## Category of IIT?

## IIT : haskell calculator

```
-- Prelude Control.Comonad.Trans.Traced Data.Char

--------------------------------------------------
-- Reader Monad
instance Monad ((->) e) where
   return = const
   bind f r = \c -> f (r c) c
   
--------------------------------------------------
-- Env Comonad (ie CoReader)
--    http://hackage.haskell.org/package/lens-3.10/docs/Control-Comonad-Env.html
instance Comonad ((,) e) where
    extract = snd
    extend f w = (fst w, f w)
    
let e = env 1 "hi"
runEnv e -- (1, "hi")
asks id e -- 1
runEnv (local (+1) e) -- (2,"hi")

let f = \wx -> (extract wx) ++ "!"
runEnv (extend f e) -- (1,"hi!")


    
--------------------------------------------------
-- Writer Monad
instance Monoid e => Monad ((,) e) where
    return = ((,) mempty)
    bind f (c, a) = (c <> c', a') -- <> is mappend?
        where (c', a') = f a

--------------------------------------------------
-- Traced Comonad (ie CoWriter)
--   https://hackage.haskell.org/package/comonad-transformers-3.0/docs/Control-Comonad-Trans-Traced.html
instance Monoid e => Comonad ((->) e) where
    extract m = m mempty
    extend f m = \c -> f (\c' -> m (c <> c'))

runTraced (traced length) "hi" -- 2
runTraced (traced (+2)) 4      -- 6
(runTraced $ listen (traced length)) "hi" -- (2,"hi")
runTraced (listens (++"!") (traced length)) "hi"  -- (2,"hi!")
runTraced (listens (length) (traced length)) "hi" -- (2,2)
runTraced (censor (++"!") (traced length)) "hi" -- 3
(trace "FlappyBird" (traced length)) -- 10

let f = \wx -> (extract wx) ++ "!"
runTraced (extend f (traced (++"?"))) "hi" -- "hi?!"


```


## State vs Store

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
```
λ> getPredicate (contramap (*2) (Predicate (<10))) 4
True
λ> getPredicate (contramap (*2) (Predicate (<10))) 5
False
```
```
λ> getOp (contramap (*2) (Op (<10))) 5
False
λ> getOp (contramap (*2) (Op (<10))) 4
True
```

a -> b` has `a` in the ​_negative_​ position

# Clojure

## inputs in re-frame
http://stackoverflow.com/questions/40034152/how-do-i-handle-input-elements-in-clojure-re-frame

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

## Pixel Tracker

* White Pixel, base 64 : `"R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"`
* more stuff you can grab

```
$ip = $_SERVER['REMOTE_ADDR'];
$referer = $_SERVER['HTTP_REFERER'];
$useragent = $_SERVER['HTTP_USER_AGENT'];
```


## General Programming

### Code Annotations : https://github.com/bbatsov/clojure-style-guide#comment-annotations
* Use TODO to note missing features or functionality that should be added at a later date. [link]
* Use FIXME to note broken code that needs to be fixed. [link]
* Use OPTIMIZE to note slow or inefficient code that may cause performance problems. [link]
* Use HACK to note "code smells" where questionable coding practices were used and should be refactored away. [link]
* Use REVIEW to note anything that should be looked at to confirm it is working as intended. For example: REVIEW: Are we sure this is how the client does X currently? [link]

### My Emacs
* ace-jump-mode - C-C SPC
