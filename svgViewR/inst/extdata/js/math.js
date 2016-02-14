function add(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] + b[i];
	return s;
}

function dist(a, b){
	var i;
	var s = 0;
	for(i = 0;i < a.length;i++) s += Math.pow(b[i] - a[i], 2)
	return Math.sqrt(s);
}

function addC(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] + b;
	return s;
}

function inv(a){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = 1 / a[i];
	return s;
}

function max() {
    var args = Array.prototype.slice.call(arguments);
    return Math.max.apply(Math, args.filter(function(val) {
       return !isNaN(val);
    }));
}

function min() {
    var args = Array.prototype.slice.call(arguments);
    return Math.min.apply(Math, args.filter(function(val) {
       return !isNaN(val);
    }));
}

function multC(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] * b;
	return s;
}

function rotateTrig(r){return {sin : -Math.sin(r), cos : Math.cos(r)};}

function rotateVector(v, deg){
	var rad = degToRad(deg);
	var vec = new Array(v[0]*Math.cos(rad) - v[1]*Math.sin(rad), v[0]*Math.sin(rad) + v[1]*Math.cos(rad));
	return vec;
}

function rotateX1(a, t) {
	var tmp = a.y;
	a.y = (t.cos * a.y) - (t.sin * a.z);
	a.z = (t.sin * tmp) + (t.cos * a.z);
	return a;
}

function rotateY1(a, t) {
	var tmp = a.x;
	a.x = (t.cos * a.x) + (t.sin * a.z);
	a.z = - (t.sin * tmp) + (t.cos * a.z);
	return a;
}

function rotateZ1(a, t) {
	var tmp = a.x;
	a.x = (t.cos * a.x) - (t.sin * a.y);
	a.y = (t.sin * tmp) + (t.cos * a.y);
	return a;
}

function rotateX2(a, t) {
	var tmp = a.y;
	a.y = sub(multC(a.y, t.cos), multC(a.z, t.sin));
	a.z = add(multC(tmp, t.sin), multC(a.z, t.cos));
	return a;
}

function rotateY2(a, t) {
	var tmp = a.x;
	a.x = add(multC(a.x, t.cos), multC(a.z, t.sin));
	a.z = sub(multC(a.z, t.cos), multC(tmp, t.sin));
	return a;
}

function rotateZ2(a, t) {
	var tmp = a.x;
	a.x = sub(multC(a.x, t.cos), multC(a.y, t.sin));
	a.y = add(multC(tmp, t.sin), multC(a.y, t.cos));
	return a;
}

function sub(a, b){
	s = new Array(a.length);
	for (var i = 0, len = a.length; i < len; i++) s[i] = a[i] - b[i];
	return s;
}

function uvector(v){
	var i, r;
	var s = 0;
	for(i = 0;i < v.length;i++) s += Math.pow(v[i], 2)
	var d = Math.sqrt(s);
	if(d == 0) return v;

	r = new Array();
	for(i = 0;i < v.length;i++) r[i] = v[i]/d
	return r;
}
