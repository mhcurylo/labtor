
---
title: Functional Programming 
author: Mateusz Curylo, Gamesys
patat:
  slideLevel: 2
  incrementalLists: true
...

# What is *functional programming*?

# It is a *theory* and *practice* of programming based in **math**

# It might be use to write *composable programms*

## Composable programms?

Like:

`find ./src -type f | grep "\.java" | xargs cat | grep " class "`

# It might be used to exercise *intelectual violence*

## The elephant in the room

- *Intellectual Violence* is a Project Managment *AntiPattern*

- It occurs when *someone uses theory, technology or buzzword to intimidate
  others*

- It leads to **incapability to learn**, **defensive culture** and **breakdown of communication**

## Enablers of Intellectual Violenvce AntiPattern

*Professional culture*

- We are all 'knowledge industry professionals'

- Professional means engaged in an activity for profit

- Knowledge professionals know stuff to get paid

- If we do not know stuff -> entropy and thermal death??

- If we know stuff -> how to learn?

## Becoming Immune

*Amateure culture*

- Amateure means lover of stuff or someone not paid to do stuff

- Amateure does not expect himself to know

- Therefore he can learn

## Mentoring culture

- As professional we can feel vulnerable when we do not know or we expose we do not
  know

- It takes courage to admit lack of knowledge

- Courageous == vulnerable (think trench warefare or cavalery charge)

Mentoring culture is a culture of vulnerable professionals

*Affective work* enables the *knowledge economy* (think childcare)

## Back to the point

- I would like to talk about simple concepts: pure functions, function composition, currying,
  partial application

- This simple concepts may take a lot time to learn and to understand right

- I more or less understand them which may make me sound as an ass

- Please tell me if you need more clarification

# *FP in Java and JavaScript*

## FP, Mapped:

>
> **Composition of functions**
> the land of purity
>
> **Composition of effectful computations.**
> the land of fantasy
>

your programm runs here
the land of **side-effects** - IO, randmoness etc.

## The land of the (im)pure
Adding an element to an array. Getting lost in inputs and outputs?

JavaScript
    
    const addNum = {
      num: 3,
      to: function (a) {a.unshift(this.num); return a}
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
Adding an element to an array. The pure way. 

Haskell
   
    addNum:: Int -> [Int] -> [Int]
    addNum n ls = (:) n ls

JavaScript with Immutable.js
    
    const addNum = x => ls => ls.unshift(x);

Java with vavr
    
    public class AddNumImm {
      public static Function1<Integer, Function1<List<Integer>, List<Integer>>>
      addNum = (num -> ls -> ls.prepend(num));
    }

## Thumbs up for purity and avoiding side-effects

- Use immutable data structures
- Do not assign - evaluate
- Do not use statements - evaluate
- In java - overuse final

## What can it give:

- function composition
- creating customized methods by currying lambdas
- shorter, more reusable and easier to test code

# Function composition

How to calculate the count of all words in a string?

Haskell way:
    
    
    
    








    
    






