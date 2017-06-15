var Vector = (function() {
  var empty = function() {
    return { ary: [] };
  };

  var append = function(vec, v) {
    vec.ary = vec.ary.concat(v);
    return vec.ary.length - 1;
  };

  var get = function(vec, idx) {
    return vec.ary[idx];
  }

  var find = function(vec, idx) {
    if(vec.length <= idx)
      return { tag: "Error", error: "Index " + idx + "out of bounds" };

    var v = get(vec, idx);
    if(v == undefined)
      return { tag: "Error", error: "Value is undefined" };
    else
      return { tag: "Result", result: v };
  }

  var set = function(vec, idx, v) {
    vec.ary[idx] = v;
  }

  return {
    empty: empty,
    append: append,
    get: get,
    find: find,
    set: set};
})();
