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

  const verts = summarise(vertResults);
  const horis = summarise(horiResults);

  const verts_ = flatten(vertResults);
  const horis_ = flatten(horiResults);

  // returns [ newVert, newHori]
  const pt2 = [...res].map((r,i) => simulateOne(r, verts_[i], horis_[i])).reduce((acc, [v,h]) => Number(v) + Number(h) + acc, 0);

  console.log(`Part 1: ${horis + verts}`);
  console.log(`Part 2: ${pt2}`)
}

function uniq(value, index, self) {
  return self.indexOf(value) === index;
}

const simulateOne = (res, origVert, origHori) => {
  const smu = smudges(res);
  const newVert = smu.flatMap(x => singleRun(x, true, origVert));
  const newHori = smu.flatMap(x => singleRun(transpose(x), false, origHori));
  const result = flatten([newVert, newHori]);
  return result;
}

const flatten = x => x.map(x => x.length == 0 ? 0 : x[0]);

const summarise = (z) => z.flatMap(x=>x).reduce((acc,x) => x + acc,0);

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
  let result = [];
  const [iMax,jMax] = dimensions(matrix)
  for (var j = 0; j < jMax; j++) {
    for (var i = 0; i < iMax; i++) {
      result[j] = (result[j] || "") + matrix[i].substr(j,1)
    }
  }
  return result;
}

function dimensions(matrix) {
  return [matrix.length, matrix[0].length];
}

function midPoint(i,j) {
  if ((j-i) % 2 === 0) {
    return (j-i)/2 + i;
  } else {
    return j-(j-i + 1)/2 + 1;
  }
}

const singleRun = (matrix, vert, exclude = null) => {
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
  let newRes = res;
  if (exclude !== null) {
      const arrMaybe = res.filter(uniq).filter(x => x !== exclude);
      newRes = arrMaybe.length > 0 ? [arrMaybe[0]] : [];
  }
  return newRes;
}

const flip = (chr)  => {
  if (chr == "#") return ".";
  else if (chr == ".") return "#";
}

const copy = (matrix) => matrix.slice().map(arr => arr.slice());

const smudges = (matrix) => {
  const [iMax, jMax] = dimensions(matrix);
  var res = []
  for (let i = 0; i < iMax; i++) {
    for (let j = 0; j < jMax; j++) {
      let newCopy = copy(matrix);
      const newVal = flip(newCopy[i].charAt(j));
      newCopy[i] = setCharAt(newCopy[i],j, newVal);
      res.push(newCopy);
    }
  }
  return res;
}

function setCharAt(str,index,chr) {
  if(index > str.length-1) return str;
  return str.substring(0,index) + chr + str.substring(index+1);
}
