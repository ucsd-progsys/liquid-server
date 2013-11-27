

/////////////////////////////////////////////////////////////////
// Paste Demo 2//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/*@ loop :: forall A. (number, number, A, (number, A) => A) => A */
function loop(lo, hi, acc, body){
  if (lo < hi) {
    acc = body(lo, acc);
    return loop(lo + 1, hi, acc, body);
  }
  return acc;
}

/* minIndex :: ({a:list [number] | true}) => number */            // BAD SPEC 
/*@ minIndex :: ({a:list [number] | 0 < (len a)}) => number */    // GOOD SPEC
function minIndex(a){
  /*@ step :: (number, number) => number */
  function step(i, min){
    if (a[i] < a[min]) 
      min = i;
    return min;
  }
  return loop(0, length(a), 0, step);
}


