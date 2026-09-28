{ function a() {} function a() {} }
switch (0) { case 1: function b() {} default: function b() {} }
{ l: function c() {} function c() {} }
try {} catch (e) { function d() {} function d() {} }
{ function f() { "use strict"; } function f() {} }

{ async function g() {} function g() {} }
{ function h() {} function* h() {} }
{ function i() {} let i; }
{ var j; function j() {} }
{ function k() {} class k {} }
try {} catch (m) { function m() {} }
function n() { "use strict"; { function o() {} function o() {} } }
