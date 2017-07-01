const addNum = {
  num: 3,
  to: function (a) {a.unshift(this.num); return a}
}

const a = [1, 2];
console.log('a is ', a);
console.log('to output', addNum.to(a));
console.log('a is ', a);
