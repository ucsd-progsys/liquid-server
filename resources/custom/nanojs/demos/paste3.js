/////////////////////////////////////////////////////////////////
// Paste Demo 3//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/*@ range :: (number, number) => list[number] */
function range(lo, hi) {
  if (lo < hi) {
    res = range(lo + 1, hi);
    res = push(lo, res);
    return res;
  }
  return empty();
}

/*@ reduce :: forall A B. (list[B], A, (A, B) => A) => A */
function reduce(xs, acc, f){
  if (!isEmpty(xs)) {
    acc = f(acc, top(xs));
    acc = reduce(pop(xs), acc, f);
  }
  return acc;
}

/* minIndex :: ({a:list [number] | true}) => number */            // BAD SPEC 
/*@ minIndex :: ({a:list [number] | 0 < (len a)}) => number */    // GOOD SPEC

function minIndex(a){
  /*@ step :: (number, number) => number */
  function step(min, i){
    if (a[i] < a[min]) 
      min = i;
    return min;
  }
  var is = range(0, length(a));
  return reduce(is,0,step);
}

