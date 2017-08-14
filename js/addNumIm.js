const addNum = n => ls => [n, ...ls];

const addNum3 = addNum(3);

const a = [2, 1];

console.log('a is ', a);
console.log('AddNum to output', addNum3(a));
console.log('a is ', a);
