addNum = n => ls => [n, ...ls];

compose = (...args) => val => args.reverse().reduce((p, c) => c(p), val);
pipe = (...args) => val => args.reduce((p, c) => c(p), val);

compose2 = (g, f) => v => g(f(v));

addNum1 = addNum(1);
addNum2 = addNum(2);
addNum3 = addNum(3);

list = [];

console.log('compose2(addNum1,addNum2)(list)', compose2(addNum1, addNum2)(list));
console.log('addNum1(addNum2(list))', addNum1(addNum2(list)));

addNum123 = compose(addNum1, addNum2, addNum3);


console.log('list is', list);
console.log('addNum1(addNum2(addNum3(list)))', addNum1(addNum2(addNum3(list))));
console.log('compose(addNum1, addNum2, addNum3)(list)', compose(addNum1, addNum2, addNum3)(list));
console.log('pipe(addNum1, addNum2, addNum3)(list)', pipe(addNum1, addNum2, addNum3)(list));
console.log('list is', list);


