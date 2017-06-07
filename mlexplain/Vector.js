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

  return {
    empty: empty,
    append: append,
    get: get};
})();
