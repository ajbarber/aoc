const fs = require('fs');
const path = require('path');

const handler = () => {
  var res = [[]];
  var j =0;
  var array = fs.readFileSync('Day13.txt').toString().split("\n");
  for(i in array) {
    if (array[i] == "") {
      res.push([]);
      j++;
    } else {
      res[j].push(array[i]);
    }
  }
  res.pop();
  const vertResults = res.map(r => singleRun(r, true));
  const horiResults = res.map(r=> singleRun(transpose(r), false));
  const horis = vertResults.flatMap(x=>x).reduce((acc,x) => x + acc,0);
  const verts = horiResults.flatMap(x=>x).reduce((acc,x) => x + acc,0);
  console.log(`Part 1: ${horis + verts}`);
}

const substrs = (str) => {
  const n = str.length;
  return perms(0, n-1).map(([i,j]) => [str.substring(i,j+1),[i,j]]);
}

const perms = (start, end) => {
  if (start > end) return [];

  res = [];
  for (var i = start; i <= end; i++) {
    if ((i-start) % 2 == 1) res.push([start,i]);
    if (i> start && (end-i) % 2 == 1) res.push([i,end]);
  }
  return res;
}

const process = (str, res=true) => {
  if (str === "") return res;

  const end = str.slice(-1);
  const middle = str.slice(1,-1);
  const start = str.slice(0,1);

  return process(middle, res && start === end);
}

const reflection = (cache, str) => {
  const res = substrs(str);
  res.forEach(([s,[k,kn]]) => {
    const refl = process(s);
    if (cache[k] === undefined) cache[k]={};
    const existing = cache[k][kn];
    if (existing === undefined) {
      cache[k][kn] = refl;
    } else {
      cache[k][kn] = cache[k][kn] && refl;
    }
  });
  return cache;
}

const reflections = (cache, strs) => {
  return strs.reduce((c,x) => reflection(c,x), cache);
}

function transpose(matrix) {
  let result = []
  for (var j = 0; j < matrix[0].length; j++) {
    for (var i = 0; i < matrix.length; i++) {
      result[j] = (result[j] || "") + matrix[i].substr(j,1)
    }
  }
  return result;
}

function midPoint(i,j) {
  if ((j-i) % 2 === 0) {
    return (j-i)/2 + i;
  } else {
    return j-(j-i + 1)/2 + 1;
  }
}

const singleRun = (matrix, vert) => {
  const test = reflections({}, matrix);
  var res = [];
  for (var i of Object.keys(test)) {
    for (var j of Object.keys(test[i])) {
      var i_ = +i, j_ = +j;
      if (test[i][j] && i_ < j_) {
        if (vert) {
          res = [...res,  midPoint(i_,j_)]
        } else {
          res = [...res,  100*midPoint(i_,j_)]
        }
      }
    }
  }
  //Take highest in case of duplicate
  return res.length>0 ? [Math.max(...res)]:res;
}
