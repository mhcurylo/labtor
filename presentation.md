
---
title: Functional Programming 
author: Mateusz Curylo, Gamesys
patat:
  slideLevel: 2
  incrementalLists: true
...

# An FP quote first

## The FP quote

No matter what language you work in, programming in a functional style provides
benefits. You should do it whenever it is convenient, and you should think hard
about the decision when it isn't convenient. 

**In-depth: Functional programming in C++**
*John Carmack*

# Learning *functional programming*

# Functional programing is a *theory* and *practice* of programming based in **mathematics**

# It might be used to write *composable programs*

## Composable programs?

Like:

`find ./src -type f | grep "\.java" | xargs cat | grep " class "`

# It might be used to exercise *intellectual violence*

## Intellectual violence?

- *Intellectual Violence* is a Project Management *AntiPattern*

- It occurs when *someone uses theory, technology or buzzword to intimidate
  others*

- It leads to **incapability to learn**, **defensive culture** and **breakdown of communication**

## Enablers of Intellectual Violence AntiPattern

*Professional culture*

- We are all 'knowledge industry professionals'

- Professional means engaged in an activity for profit

- Knowledge professionals know stuff to get paid

- If we do not know stuff -> entropy and thermal death??

- If we know stuff -> how to learn?

## Becoming Immune

*Amateur culture*

- Amateur means lover of stuff or someone not paid to do stuff

- Amateur does not expect himself to know

- Therefore he can learn

## Mentoring culture

- As professional we can feel vulnerable when we do not know or we expose we do not
  know

- It takes courage to admit lack of knowledge

- Courageous == vulnerable (think trench warefare or cavalery charge)

Mentoring culture is a culture of vulnerable professionals

*Affective work* enables the *knowledge economy* (think childcare)

## Back to the point

- I would like to talk about simple concepts: pure functions, function composition, currying

- This simple concepts took me a lot of time to get 

- I more or less understand them now which may make me sound as an ass

- Please ask questions if you need clarification

# *Learning FP*

## FP Overview, Mapped:

>
> **Composition of functions**
> the land of purity
> -------- a strange gap -----------
> **Composition of effectful computations**
> the land of fantasy, where you pretend your side-effects happen
>

--- chasm between syntax / semantics, char / meaning ---

> your programm runs here
> the land of **the true side-effects** - IO, randmoness etc.

## Languages and tools:

| Use                     | Feature           | Haskell | Clojure   | Java           | JS             |
| ---                     | ---               | ---     | ---       | ---            | ---            |
| Keeping pure            | purity            | YES     | NO        | NO             | NO             |
|                         | immutable DS      | YES     | YES       | vavr           | immutable.js   |
|                         | mutability        | LOW     | MANAGED   | HIGH           | RATHER CRAZY   |
| Function composition    | HO functions      | YES     | YES       | lambdas        | YES            |
|                         | composition       | YES     | YES       | lambdas        | one liner      |
|                         | currying          | DEFAULT | kinda     | lambdas        | YES, ramda     |
|                         | types             | YES     | OPTIONAL  | YES            | TS, flow       |
| Computation composition | containers        | YES     | library?  | vavr, optional | ramda-fantasy  |
| Other nice to haves     | recursion TCO     | kinda   | via recur | NO             | not now        | 
|                         | pattern-match     | YES     | YES       | vavr           | NO             |
|                         | add-hoc polymorphy| YES     | YES       | NO, AFAIK      | monkey-patching|

# *Learning FP in Java and JavaScript*

# Purity

## The land of the impure

Adding an element to an array. Getting lost in inputs and outputs?

JavaScript
        
    const addNum = {
      num: 3,
      to: function (a) {a.unshift(this.num); return a}
      setNum: function (n) { this.num = n }
    }

Java
    
    public class AddNum {
      Integer num;
      AddNum(Integer n) { num = n; }
      public List<Integer> to(List<Integer> ls) {
        ls.add(0, this.num);
        return ls;
      }
      public void setNum(Integer n) {num = n;}
    }

## The land of the pure

Creating a method to add an element to an array. The pure way, step one. 

Haskell
   
    addNum :: Num a => a -> [a] -> [a]
    addNum n ls = n : ls

JavaScript 
    
    const addNum = (x, ls) => [x, ...ls];

Java with vavr
    
    public class AddNumImm {
      public static Function2<Integer, List<Integer>, List<Integer>>
      addNum = (num, ls) -> ls.prepend(num);
    }

## The land of the pure. Curried.

Creating a method to add an element to an array. The pure way, step two. 

Haskell
    addNum :: Num a => a -> [a] -> [a]
    addNum n ls = n : ls
    addNum5 :: Num a => [a] -> [a]
    addNum5 = addNum 5

JavaScript 
    const addNum = x => ls => [x, ...ls];
    const addNum5 = addNum(5);

Java with vavr
    
    public class AddNumImm {
      Function1<Integer, Function1<List<Integer>, List<Integer>>>
      addNum = (num -> ls -> ls.prepend(num));
      Function1<List<Integer>, List<Integer>> addNum5 = addNum.apply(5); 
    }

# Function composition

## Function composition. Theory.

    A --f--> B --g--> C
    A ---- g ◯ f ---> C
 
  composition of function g and function f is a function h which works as if h(v) = g(f(v))

## TC;DR

Function composition is a function like that:

Haskell
        
    compose2  g f v = g (f v)
    compose2' g f = g . f
    compose2'' = (.)
    
JavaScript   
    
    compose2 = (g, f) => value => g(f(value));
     
Java with vavr    
    
    compose2 = ((g, f) -> value -> g.apply(f.apply(value)))
    compose2alt = (g, f) -> g.compose(f); 
    
Function which takes two (or more) functions and returns a new function.

## Thumbs up for purity and avoiding side-effects

- Use immutable data structures - no hidden outputs!
- Do not assign - evaluate - no opportunity for mutations!
- Do not use statements - evaluate - less side effects!
- Do not use loops - loops use assignments and mutations!
- Overuse final, const - no reassignment!

## What can it give:

- Make function sigantures great again!
- Easier function composition
- Creating customized methods by currying lambdas and composing functions
- Shorter, more reusable and easier to test code

## A lot of freebees!

'Free' as in 'free theorems'.

For example:

GIVEN a function 
     
    f: [a] -> a
AND a list of a 
     
    ls: [a] 
WHEN a function g is 
     
    g: a -> b

THEN for any a and b 
    
    f (map g ls) == g (f ls) 
# Comparing Haskell to java. How to calculate the count of all words in a string?

## Step one. Make a list of all words. Haskell way:
     
    listTokens :: String -> [String]
    listTokens = words . filter isAlphaOrSpace . map toLower
      where
        isAlphaOrSpace x =  isAlpha x || isSpace x

## Step one. Make a list of all words. Java way:
    
    Function1<String, String[]> toArrayOfTokens = txt -> txt
               .toLowerCase()
               .replaceAll("[^a-zA-Z\\d\\s:]", "")
               .split("\\s+");

    Function1<String[], Stream<String>> listTokens = toArrayOfTokens
               .andThen(Stream::of);

## Step Two. Merge a list of words into a map. Haskell way:

    countTokens :: [String] -> M.Map String Int
    countTokens = M.fromListWith (+) . (`zip` repeat 1)

## Step Two. Merge a list of words into a map. Java.

    Function1<Stream<String>, Map<String, Integer>> countTokens = stream -> stream
                    .foldLeft(TreeMap.empty(), (p, c) -> p.put(c, p.getOrElse(c, 0) + 1));

## Step Three. Compose. Haskell way:

    countWords :: String -> M.Map String Int
    countWords = countTokens . listTokens

## Step Three. Compose. Java way:
    
    Function1<String, Map<String, Integer>> countWords = countTokens.compose(listTokens);

# Testing it! Quickcheck FTW.
# Making it work.
# THANKS! Questions?
