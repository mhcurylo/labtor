const addNum = {
  num: 3,
  to: function (a) {a.unshift(this.num); return a},
  setNum: function (n) {this.num = n}
}

const a = [1, 2];

console.log('a is ', a);
console.log('addNum output', addNum.to(a));
console.log('a is ', a);
