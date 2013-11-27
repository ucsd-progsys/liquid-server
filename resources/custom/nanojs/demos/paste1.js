/////////////////////////////////////////////////////////////////
// Paste Demo 1//////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


/*@ loop :: (list[number], number, number) => number */
function loop(b, min, i){
  if (i < length(b)){
    if (b[i] < b[0])
      min = i;
    return loop(b, min, i+1)
  }
  return min;
}

/*@ minIndex :: (list[number]) => {v:number|true} */ 
function minIndex(a){
  return loop(a,0,0);
}

