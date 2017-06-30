const List = require('immutable').List;

const addNum = n => ls => ls.unshift(n);

const addNum3= addNum(3);

const a = List.of(2, 1);
console.log('a is ', a);
console.log('to output', addNum1(a));
console.log('a is ', a);
