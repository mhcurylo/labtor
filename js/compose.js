addNum = n => ls => [n, ...ls];

compose = (...args) => val => args.reverse().reduce((p, c) => c(p), val);

addNum1 = addNum(1);
addNum2 = addNum(2);
addNum3 = addNum(3);

addNum123 = compose(addNum1, addNum2, addNum3);

list = [];

console.log('list is', list);
console.log('addNum1(addNum2(addNum3(list)))', addNum1(addNum2(addNum3(list))));
console.log('addNum123(list)', addNum123(list));
console.log('list is', list);


