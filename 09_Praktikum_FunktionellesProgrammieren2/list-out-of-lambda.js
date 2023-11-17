/* 
 *  Quelle:
 *  https://gist.github.com/sjl/5277681
 */

var nil = function(selector) {
    return selector(undefined, undefined, true);
};

var cons = function(el, list) {
    return function(selector) {
        return selector(el, list, false);
    };
};
var car = function(list) {
    return list(function(h, t, e) {
        return h;
    });
};
var cdr = function(list) {
    return list(function(h, t, e) {
        return t;
    });
};

var is_nil = function(list) {
    return list(function(h, t, e) {
        return e;
    });
};

var not = function(x) {
    if (x) {
        return false;
    } else {
        return true;
    }
};
var and = function(a, b) {
    if (a) {
        if (b) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
};
var or = function(a, b) {
    if (a) {
        return true;
    } else {
        if (b) {
            return true;
        } else {
            return false;
        }
    }
};

var map = function(fn, l) {
    if (is_nil(l)) {
        return nil;
    } else {
        return cons(fn(car(l)), map(fn, cdr(l)));
    }
};
var filter = function(fn, l) {
    if (is_nil(l)) {
        return nil;
    } else if (fn(car(l))) {
        return cons(car(l), filter(fn, cdr(l)));
    } else {
        return filter(fn, cdr(l));
    }
};

var zero = nil;

var inc = function(n) {
    return cons(nil, n);
};
var dec = function(n) {
    return cdr(n);
};

var one = inc(zero);

var is_zero = function(n) {
    return is_nil(n);
};

var add = function(a, b) {
    if (is_zero(b)) {
        return a;
    } else {
        return add(inc(a), dec(b));
    }
};
var sub = function(a, b) {
    if (is_zero(b)) {
        return a;
    } else {
        return add(dec(a), dec(b));
    }
};
var mul = function(a, b) {
    if (is_zero(b)) {
        return zero;
    } else {
        return add(a, mul(a, dec(b)));
    }
};
var pow = function(a, b) {
    if (is_zero(b)) {
        return one;
    } else {
        return mul(a, pow(a, dec(b)));
    }
};

var is_equal = function(n, m) {
    if (and(is_zero(n), is_zero(m))) {
        return true;
    } else if (or(is_zero(n), is_zero(m))) {
        return false;
    } else {
        return is_equal(dec(n), dec(m));
    }
};
var less_than = function(a, b) {
    if (and(is_zero(a), is_zero(b))) {
        return false;
    } else if (is_zero(a)) {
        return true;
    } else if (is_zero(b)) {
        return false;
    } else {
        return less_than(dec(a), dec(b));
    }
};
var greater_than = function(a, b) {
    return less_than(b, a);
};

var div = function(a, b) {
    if (less_than(a, b)) {
        return zero;
    } else {
        return inc(div(sub(a, b), b));
    }
};
var rem = function(a, b) {
    if (less_than(a, b)) {
        return a;
    } else {
        return rem(sub(a, b), b);
    }
};

var nth = function(l, n) {
    if (is_zero(n)) {
        return car(l);
    } else {
        return nth(cdr(l), dec(n));
    }
};
var drop = function(l, n) {
    if (is_zero(n)) {
        return l;
    } else {
        return drop(cdr(l), dec(n));
    }
};
var take = function(l, n) {
    if (is_zero(n)) {
        return nil;
    } else {
        return cons(car(l), take(cdr(l), dec(n)));
    }
};
var slice = function(l, start, end) {
    return take(drop(l, start), sub(end, start));
};
var length = function(l) {
    if (is_nil(l)) {
        return zero;
    } else {
        return inc(length(cdr(l)));
    }
};