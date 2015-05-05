var Elm = Elm || { Native: {} };
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,empty: empty
                       ,repeat: repeat
                       ,initialize: initialize
                       ,fromList: fromList
                       ,length: length
                       ,push: push
                       ,append: append
                       ,get: get
                       ,set: set
                       ,slice: slice
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,filter: filter
                       ,foldl: foldl
                       ,foldr: foldr};
   return _elm.Array.values;
};
Elm.Audio = Elm.Audio || {};
Elm.Audio.make = function (_elm) {
   "use strict";
   _elm.Audio = _elm.Audio || {};
   if (_elm.Audio.values)
   return _elm.Audio.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Audio",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Model = Elm.Model.make(_elm),
   $OSC = Elm.OSC.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $TopicData = Elm.TopicData.make(_elm),
   $Updates = Elm.Updates.make(_elm);
   var stopToken = function (token) {
      return function () {
         var update = A3($Updates.soundUpdate,
         token.id,
         false,
         $OSC.StopTokens);
         return A2($Signal.send,
         $Updates.soundUpdates.address,
         update);
      }();
   };
   var playToken = F2(function (token,
   data) {
      return function () {
         var tokens = _L.fromArray([{ctor: "_Tuple2"
                                    ,_0: token.id
                                    ,_1: 1.0}]);
         var update = A3($Updates.soundUpdate,
         token.id,
         true,
         $OSC.PlayTokens(tokens));
         return A2($Signal.send,
         $Updates.soundUpdates.address,
         update);
      }();
   });
   var stopTopic = function (topic) {
      return function () {
         var update = A3($Updates.soundUpdate,
         A2($Basics._op["++"],
         "topic",
         $Basics.toString(topic)),
         false,
         $OSC.StopTokens);
         return A2($Signal.send,
         $Updates.soundUpdates.address,
         update);
      }();
   };
   var playTopic = F2(function (topic,
   data) {
      return function () {
         var tokens = A2($TopicData.topicTokens,
         topic,
         data);
         var tokens$ = A2($List.map,
         function (t) {
            return {ctor: "_Tuple2"
                   ,_0: t.id
                   ,_1: t.prob};
         },
         tokens);
         var update = A3($Updates.soundUpdate,
         A2($Basics._op["++"],
         "topic",
         $Basics.toString(topic)),
         true,
         $OSC.PlayTokens(tokens$));
         return A2($Signal.send,
         $Updates.soundUpdates.address,
         update);
      }();
   });
   _elm.Audio.values = {_op: _op
                       ,playTopic: playTopic
                       ,stopTopic: stopTopic
                       ,playToken: playToken
                       ,stopToken: stopToken};
   return _elm.Audio.values;
};
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 595, column 3 to 8");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 573, column 3 to 4");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 567, column 3 to 4");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Bootstrap = Elm.Bootstrap || {};
Elm.Bootstrap.Html = Elm.Bootstrap.Html || {};
Elm.Bootstrap.Html.make = function (_elm) {
   "use strict";
   _elm.Bootstrap = _elm.Bootstrap || {};
   _elm.Bootstrap.Html = _elm.Bootstrap.Html || {};
   if (_elm.Bootstrap.Html.values)
   return _elm.Bootstrap.Html.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Bootstrap.Html",
   $Basics = Elm.Basics.make(_elm),
   $Bootstrap$Html$Internal = Elm.Bootstrap.Html.Internal.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Shorthand = Elm.Html.Shorthand.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var panelTitle_ = function (t) {
      return A2($Html$Shorthand.h2$,
      {_: {},$class: "panel-title"},
      _L.fromArray([$Html.text(t)]));
   };
   var panelBody_ = $Html$Shorthand.div$({_: {}
                                         ,$class: "panel-body"});
   var panelHeading_ = $Html$Shorthand.div$({_: {}
                                            ,$class: "panel-heading"});
   var panelDefault_ = $Html$Shorthand.div$({_: {}
                                            ,$class: "panel panel-default"});
   var navbarHeader_ = $Html$Shorthand.div$({_: {}
                                            ,$class: "navbar-header"});
   var navbar$ = function (c) {
      return $Html$Shorthand.nav$({_: {}
                                  ,$class: A2($Basics._op["++"],
                                  "navbar ",
                                  c)});
   };
   var navbarDefault$ = function (c) {
      return navbar$(A2($Basics._op["++"],
      "navbar-default ",
      c));
   };
   var glyphiconTreeDeciduous$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tree-deciduous ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTreeDeciduous_ = glyphiconTreeDeciduous$("");
   var glyphiconTreeConifer$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tree-conifer ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTreeConifer_ = glyphiconTreeConifer$("");
   var glyphiconCloudUpload$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-cloud-upload ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCloudUpload_ = glyphiconCloudUpload$("");
   var glyphiconCloudDownload$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-cloud-download ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCloudDownload_ = glyphiconCloudDownload$("");
   var glyphiconRegistrationMark$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-registration-mark ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRegistrationMark_ = glyphiconRegistrationMark$("");
   var glyphiconCopyrightMark$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-copyright-mark ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCopyrightMark_ = glyphiconCopyrightMark$("");
   var glyphiconSound71$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sound-7-1 ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSound71_ = glyphiconSound71$("");
   var glyphiconSound61$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sound-6-1 ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSound61_ = glyphiconSound61$("");
   var glyphiconSound51$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sound-5-1 ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSound51_ = glyphiconSound51$("");
   var glyphiconSoundDolby$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sound-dolby ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSoundDolby_ = glyphiconSoundDolby$("");
   var glyphiconSoundStereo$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sound-stereo ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSoundStereo_ = glyphiconSoundStereo$("");
   var glyphiconSubtitles$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-subtitles ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSubtitles_ = glyphiconSubtitles$("");
   var glyphiconHdVideo$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hd-video ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHdVideo_ = glyphiconHdVideo$("");
   var glyphiconSdVideo$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sd-video ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSdVideo_ = glyphiconSdVideo$("");
   var glyphiconStats$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-stats ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStats_ = glyphiconStats$("");
   var glyphiconTower$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tower ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTower_ = glyphiconTower$("");
   var glyphiconPhoneAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-phone-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPhoneAlt_ = glyphiconPhoneAlt$("");
   var glyphiconEarphone$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-earphone ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEarphone_ = glyphiconEarphone$("");
   var glyphiconCompressed$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-compressed ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCompressed_ = glyphiconCompressed$("");
   var glyphiconHeader$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-header ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHeader_ = glyphiconHeader$("");
   var glyphiconCutlery$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-cutlery ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCutlery_ = glyphiconCutlery$("");
   var glyphiconTransfer$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-transfer ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTransfer_ = glyphiconTransfer$("");
   var glyphiconCreditCard$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-credit-card ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCreditCard_ = glyphiconCreditCard$("");
   var glyphiconFloppyOpen$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-floppy-open ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFloppyOpen_ = glyphiconFloppyOpen$("");
   var glyphiconFloppySave$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-floppy-save ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFloppySave_ = glyphiconFloppySave$("");
   var glyphiconFloppyRemove$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-floppy-remove ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFloppyRemove_ = glyphiconFloppyRemove$("");
   var glyphiconFloppySaved$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-floppy-saved ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFloppySaved_ = glyphiconFloppySaved$("");
   var glyphiconFloppyDisk$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-floppy-disk ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFloppyDisk_ = glyphiconFloppyDisk$("");
   var glyphiconSend$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-send ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSend_ = glyphiconSend$("");
   var glyphiconExport$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-export ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconExport_ = glyphiconExport$("");
   var glyphiconImport$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-import ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconImport_ = glyphiconImport$("");
   var glyphiconSaved$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-saved ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSaved_ = glyphiconSaved$("");
   var glyphiconOpen$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-open ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconOpen_ = glyphiconOpen$("");
   var glyphiconSave$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-save ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSave_ = glyphiconSave$("");
   var glyphiconRecord$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-record ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRecord_ = glyphiconRecord$("");
   var glyphiconNewWindow$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-new-window ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconNewWindow_ = glyphiconNewWindow$("");
   var glyphiconLogOut$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-log-out ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconLogOut_ = glyphiconLogOut$("");
   var glyphiconFlash$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-flash ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFlash_ = glyphiconFlash$("");
   var glyphiconLogIn$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-log-in ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconLogIn_ = glyphiconLogIn$("");
   var glyphiconCollapseUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-collapse-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCollapseUp_ = glyphiconCollapseUp$("");
   var glyphiconCollapseDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-collapse-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCollapseDown_ = glyphiconCollapseDown$("");
   var glyphiconExpand$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-expand ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconExpand_ = glyphiconExpand$("");
   var glyphiconUnchecked$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-unchecked ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconUnchecked_ = glyphiconUnchecked$("");
   var glyphiconSortByAttributesAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-attributes-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByAttributesAlt_ = glyphiconSortByAttributesAlt$("");
   var glyphiconSortByAttributes$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-attributes ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByAttributes_ = glyphiconSortByAttributes$("");
   var glyphiconSortByOrderAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-order-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByOrderAlt_ = glyphiconSortByOrderAlt$("");
   var glyphiconSortByOrder$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-order ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByOrder_ = glyphiconSortByOrder$("");
   var glyphiconSortByAlphabetAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-alphabet-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByAlphabetAlt_ = glyphiconSortByAlphabetAlt$("");
   var glyphiconSortByAlphabet$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort-by-alphabet ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSortByAlphabet_ = glyphiconSortByAlphabet$("");
   var glyphiconSort$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-sort ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSort_ = glyphiconSort$("");
   var glyphiconGbp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-gbp ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconGbp_ = glyphiconGbp$("");
   var glyphiconUsd$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-usd ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconUsd_ = glyphiconUsd$("");
   var glyphiconPushpin$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-pushpin ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPushpin_ = glyphiconPushpin$("");
   var glyphiconPhone$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-phone ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPhone_ = glyphiconPhone$("");
   var glyphiconLink$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-link ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconLink_ = glyphiconLink$("");
   var glyphiconHeartEmpty$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-heart-empty ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHeartEmpty_ = glyphiconHeartEmpty$("");
   var glyphiconPaperclip$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-paperclip ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPaperclip_ = glyphiconPaperclip$("");
   var glyphiconDashboard$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-dashboard ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconDashboard_ = glyphiconDashboard$("");
   var glyphiconFullscreen$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-fullscreen ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFullscreen_ = glyphiconFullscreen$("");
   var glyphiconBriefcase$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-briefcase ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBriefcase_ = glyphiconBriefcase$("");
   var glyphiconFilter$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-filter ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFilter_ = glyphiconFilter$("");
   var glyphiconTasks$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tasks ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTasks_ = glyphiconTasks$("");
   var glyphiconWrench$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-wrench ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconWrench_ = glyphiconWrench$("");
   var glyphiconGlobe$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-globe ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconGlobe_ = glyphiconGlobe$("");
   var glyphiconCircleArrowDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-circle-arrow-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCircleArrowDown_ = glyphiconCircleArrowDown$("");
   var glyphiconCircleArrowUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-circle-arrow-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCircleArrowUp_ = glyphiconCircleArrowUp$("");
   var glyphiconCircleArrowLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-circle-arrow-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCircleArrowLeft_ = glyphiconCircleArrowLeft$("");
   var glyphiconCircleArrowRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-circle-arrow-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCircleArrowRight_ = glyphiconCircleArrowRight$("");
   var glyphiconHandDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hand-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHandDown_ = glyphiconHandDown$("");
   var glyphiconHandUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hand-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHandUp_ = glyphiconHandUp$("");
   var glyphiconHandLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hand-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHandLeft_ = glyphiconHandLeft$("");
   var glyphiconHandRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hand-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHandRight_ = glyphiconHandRight$("");
   var glyphiconThumbsDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-thumbs-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconThumbsDown_ = glyphiconThumbsDown$("");
   var glyphiconThumbsUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-thumbs-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconThumbsUp_ = glyphiconThumbsUp$("");
   var glyphiconCertificate$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-certificate ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCertificate_ = glyphiconCertificate$("");
   var glyphiconBell$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-bell ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBell_ = glyphiconBell$("");
   var glyphiconBullhorn$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-bullhorn ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBullhorn_ = glyphiconBullhorn$("");
   var glyphiconHdd$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-hdd ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHdd_ = glyphiconHdd$("");
   var glyphiconResizeHorizontal$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-resize-horizontal ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconResizeHorizontal_ = glyphiconResizeHorizontal$("");
   var glyphiconResizeVertical$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-resize-vertical ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconResizeVertical_ = glyphiconResizeVertical$("");
   var glyphiconFolderOpen$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-folder-open ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFolderOpen_ = glyphiconFolderOpen$("");
   var glyphiconFolderClose$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-folder-close ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFolderClose_ = glyphiconFolderClose$("");
   var glyphiconShoppingCart$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-shopping-cart ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconShoppingCart_ = glyphiconShoppingCart$("");
   var glyphiconRetweet$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-retweet ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRetweet_ = glyphiconRetweet$("");
   var glyphiconChevronDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-chevron-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconChevronDown_ = glyphiconChevronDown$("");
   var glyphiconChevronUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-chevron-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconChevronUp_ = glyphiconChevronUp$("");
   var glyphiconMagnet$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-magnet ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMagnet_ = glyphiconMagnet$("");
   var glyphiconComment$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-comment ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconComment_ = glyphiconComment$("");
   var glyphiconRandom$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-random ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRandom_ = glyphiconRandom$("");
   var glyphiconCalendar$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-calendar ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCalendar_ = glyphiconCalendar$("");
   var glyphiconPlane$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-plane ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPlane_ = glyphiconPlane$("");
   var glyphiconWarningSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-warning-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconWarningSign_ = glyphiconWarningSign$("");
   var glyphiconEyeClose$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-eye-close ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEyeClose_ = glyphiconEyeClose$("");
   var glyphiconEyeOpen$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-eye-open ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEyeOpen_ = glyphiconEyeOpen$("");
   var glyphiconFire$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-fire ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFire_ = glyphiconFire$("");
   var glyphiconLeaf$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-leaf ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconLeaf_ = glyphiconLeaf$("");
   var glyphiconGift$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-gift ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconGift_ = glyphiconGift$("");
   var glyphiconExclamationSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-exclamation-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconExclamationSign_ = glyphiconExclamationSign$("");
   var glyphiconResizeSmall$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-resize-small ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconResizeSmall_ = glyphiconResizeSmall$("");
   var glyphiconResizeFull$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-resize-full ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconResizeFull_ = glyphiconResizeFull$("");
   var glyphiconShareAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-share-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconShareAlt_ = glyphiconShareAlt$("");
   var glyphiconArrowDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-arrow-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconArrowDown_ = glyphiconArrowDown$("");
   var glyphiconArrowUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-arrow-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconArrowUp_ = glyphiconArrowUp$("");
   var glyphiconArrowRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-arrow-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconArrowRight_ = glyphiconArrowRight$("");
   var glyphiconArrowLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-arrow-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconArrowLeft_ = glyphiconArrowLeft$("");
   var glyphiconBanCircle$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-ban-circle ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBanCircle_ = glyphiconBanCircle$("");
   var glyphiconOkCircle$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-ok-circle ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconOkCircle_ = glyphiconOkCircle$("");
   var glyphiconRemoveCircle$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-remove-circle ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRemoveCircle_ = glyphiconRemoveCircle$("");
   var glyphiconScreenshot$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-screenshot ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconScreenshot_ = glyphiconScreenshot$("");
   var glyphiconInfoSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-info-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconInfoSign_ = glyphiconInfoSign$("");
   var glyphiconQuestionSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-question-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconQuestionSign_ = glyphiconQuestionSign$("");
   var glyphiconOkSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-ok-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconOkSign_ = glyphiconOkSign$("");
   var glyphiconRemoveSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-remove-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRemoveSign_ = glyphiconRemoveSign$("");
   var glyphiconMinusSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-minus-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMinusSign_ = glyphiconMinusSign$("");
   var glyphiconPlusSign$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-plus-sign ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPlusSign_ = glyphiconPlusSign$("");
   var glyphiconChevronRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-chevron-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconChevronRight_ = glyphiconChevronRight$("");
   var glyphiconChevronLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-chevron-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconChevronLeft_ = glyphiconChevronLeft$("");
   var glyphiconEject$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-eject ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEject_ = glyphiconEject$("");
   var glyphiconStepForward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-step-forward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStepForward_ = glyphiconStepForward$("");
   var glyphiconFastForward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-fast-forward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFastForward_ = glyphiconFastForward$("");
   var glyphiconForward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-forward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconForward_ = glyphiconForward$("");
   var glyphiconStop$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-stop ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStop_ = glyphiconStop$("");
   var glyphiconPause$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-pause ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPause_ = glyphiconPause$("");
   var glyphiconPlay$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-play ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPlay_ = glyphiconPlay$("");
   var glyphiconBackward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-backward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBackward_ = glyphiconBackward$("");
   var glyphiconFastBackward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-fast-backward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFastBackward_ = glyphiconFastBackward$("");
   var glyphiconStepBackward$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-step-backward ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStepBackward_ = glyphiconStepBackward$("");
   var glyphiconMove$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-move ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMove_ = glyphiconMove$("");
   var glyphiconCheck$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-check ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCheck_ = glyphiconCheck$("");
   var glyphiconShare$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-share ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconShare_ = glyphiconShare$("");
   var glyphiconEdit$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-edit ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEdit_ = glyphiconEdit$("");
   var glyphiconTint$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tint ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTint_ = glyphiconTint$("");
   var glyphiconAdjust$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-adjust ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAdjust_ = glyphiconAdjust$("");
   var glyphiconMapMarker$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-map-marker ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMapMarker_ = glyphiconMapMarker$("");
   var glyphiconPicture$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-picture ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPicture_ = glyphiconPicture$("");
   var glyphiconFacetimeVideo$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-facetime-video ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFacetimeVideo_ = glyphiconFacetimeVideo$("");
   var glyphiconIndentRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-indent-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconIndentRight_ = glyphiconIndentRight$("");
   var glyphiconIndentLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-indent-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconIndentLeft_ = glyphiconIndentLeft$("");
   var glyphiconList$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-list ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconList_ = glyphiconList$("");
   var glyphiconAlignJustify$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-align-justify ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAlignJustify_ = glyphiconAlignJustify$("");
   var glyphiconAlignRight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-align-right ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAlignRight_ = glyphiconAlignRight$("");
   var glyphiconAlignCenter$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-align-center ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAlignCenter_ = glyphiconAlignCenter$("");
   var glyphiconAlignLeft$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-align-left ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAlignLeft_ = glyphiconAlignLeft$("");
   var glyphiconTextWidth$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-text-width ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTextWidth_ = glyphiconTextWidth$("");
   var glyphiconTextHeight$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-text-height ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTextHeight_ = glyphiconTextHeight$("");
   var glyphiconItalic$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-italic ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconItalic_ = glyphiconItalic$("");
   var glyphiconBold$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-bold ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBold_ = glyphiconBold$("");
   var glyphiconFont$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-font ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFont_ = glyphiconFont$("");
   var glyphiconCamera$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-camera ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCamera_ = glyphiconCamera$("");
   var glyphiconPrint$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-print ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPrint_ = glyphiconPrint$("");
   var glyphiconBookmark$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-bookmark ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBookmark_ = glyphiconBookmark$("");
   var glyphiconBook$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-book ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBook_ = glyphiconBook$("");
   var glyphiconTags$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tags ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTags_ = glyphiconTags$("");
   var glyphiconTag$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-tag ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTag_ = glyphiconTag$("");
   var glyphiconBarcode$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-barcode ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconBarcode_ = glyphiconBarcode$("");
   var glyphiconQrcode$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-qrcode ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconQrcode_ = glyphiconQrcode$("");
   var glyphiconVolumeUp$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-volume-up ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconVolumeUp_ = glyphiconVolumeUp$("");
   var glyphiconVolumeDown$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-volume-down ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconVolumeDown_ = glyphiconVolumeDown$("");
   var glyphiconVolumeOff$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-volume-off ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconVolumeOff_ = glyphiconVolumeOff$("");
   var glyphiconHeadphones$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-headphones ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHeadphones_ = glyphiconHeadphones$("");
   var glyphiconFlag$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-flag ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFlag_ = glyphiconFlag$("");
   var glyphiconLock$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-lock ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconLock_ = glyphiconLock$("");
   var glyphiconListAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-list-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconListAlt_ = glyphiconListAlt$("");
   var glyphiconRefresh$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-refresh ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRefresh_ = glyphiconRefresh$("");
   var glyphiconRepeat$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-repeat ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRepeat_ = glyphiconRepeat$("");
   var glyphiconPlayCircle$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-play-circle ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPlayCircle_ = glyphiconPlayCircle$("");
   var glyphiconInbox$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-inbox ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconInbox_ = glyphiconInbox$("");
   var glyphiconUpload$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-upload ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconUpload_ = glyphiconUpload$("");
   var glyphiconDownload$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-download ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconDownload_ = glyphiconDownload$("");
   var glyphiconDownloadAlt$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-download-alt ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconDownloadAlt_ = glyphiconDownloadAlt$("");
   var glyphiconRoad$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-road ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRoad_ = glyphiconRoad$("");
   var glyphiconTime$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-time ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTime_ = glyphiconTime$("");
   var glyphiconFile$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-file ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFile_ = glyphiconFile$("");
   var glyphiconHome$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-home ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHome_ = glyphiconHome$("");
   var glyphiconTrash$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-trash ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTrash_ = glyphiconTrash$("");
   var glyphiconCog$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-cog ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCog_ = glyphiconCog$("");
   var glyphiconSignal$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-signal ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSignal_ = glyphiconSignal$("");
   var glyphiconOff$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-off ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconOff_ = glyphiconOff$("");
   var glyphiconZoomOut$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-zoom-out ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconZoomOut_ = glyphiconZoomOut$("");
   var glyphiconZoomIn$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-zoom-in ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconZoomIn_ = glyphiconZoomIn$("");
   var glyphiconRemove$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-remove ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconRemove_ = glyphiconRemove$("");
   var glyphiconOk$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-ok ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconOk_ = glyphiconOk$("");
   var glyphiconThList$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-th-list ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconThList_ = glyphiconThList$("");
   var glyphiconTh$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-th ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconTh_ = glyphiconTh$("");
   var glyphiconThLarge$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-th-large ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconThLarge_ = glyphiconThLarge$("");
   var glyphiconFilm$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-film ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconFilm_ = glyphiconFilm$("");
   var glyphiconUser$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-user ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconUser_ = glyphiconUser$("");
   var glyphiconStarEmpty$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-star-empty ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStarEmpty_ = glyphiconStarEmpty$("");
   var glyphiconStar$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-star ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconStar_ = glyphiconStar$("");
   var glyphiconHeart$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-heart ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconHeart_ = glyphiconHeart$("");
   var glyphiconSearch$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-search ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconSearch_ = glyphiconSearch$("");
   var glyphiconMusic$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-music ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMusic_ = glyphiconMusic$("");
   var glyphiconGlass$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-glass ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconGlass_ = glyphiconGlass$("");
   var glyphiconPencil$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-pencil ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPencil_ = glyphiconPencil$("");
   var glyphiconEnvelope$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-envelope ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEnvelope_ = glyphiconEnvelope$("");
   var glyphiconCloud$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-cloud ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconCloud_ = glyphiconCloud$("");
   var glyphiconMinus$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-minus ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconMinus_ = glyphiconMinus$("");
   var glyphiconEuro$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-euro ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconEuro_ = glyphiconEuro$("");
   var glyphiconPlus$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-plus ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconPlus_ = glyphiconPlus$("");
   var glyphiconAsterisk$ = function (c) {
      return A2($Html$Shorthand.span$,
      {_: {}
      ,$class: A2($Basics._op["++"],
      "glyphicon glyphicon-asterisk ",
      c)},
      _L.fromArray([]));
   };
   var glyphiconAsterisk_ = glyphiconAsterisk$("");
   var skipNavigation_ = function (t) {
      return A2($Html$Shorthand.a$,
      {_: {}
      ,$class: "sr-only sr-only-focusable"
      ,href: "#content"},
      _L.fromArray([$Html.text(t)]));
   };
   var btnSubmitLgPrimary_ = function (p) {
      return A4($Bootstrap$Html$Internal.btnc,
      "btn-lg btn-primary",
      "submit",
      p,
      $Maybe.Nothing);
   };
   var btnSubmitLgPrimary$ = F2(function (c,
   p) {
      return A4($Bootstrap$Html$Internal.btnc,
      A2($Basics._op["++"],
      "btn-lg btn-primary ",
      c),
      "submit",
      p,
      $Maybe.Nothing);
   });
   var btnSubmitSmPrimary_ = function (p) {
      return A4($Bootstrap$Html$Internal.btnc,
      "btn-sm btn-primary",
      "submit",
      p,
      $Maybe.Nothing);
   };
   var btnSubmitSmPrimary$ = F2(function (c,
   p) {
      return A4($Bootstrap$Html$Internal.btnc,
      A2($Basics._op["++"],
      "btn-sm btn-primary ",
      c),
      "submit",
      p,
      $Maybe.Nothing);
   });
   var btnSubmitXsPrimary_ = function (p) {
      return A4($Bootstrap$Html$Internal.btnc,
      "btn-xs btn-primary ",
      "submit",
      p,
      $Maybe.Nothing);
   };
   var btnSubmitXsPrimary$ = F2(function (c,
   p) {
      return A4($Bootstrap$Html$Internal.btnc,
      A2($Basics._op["++"],
      "btn-xs btn-primary ",
      c),
      "submit",
      p,
      $Maybe.Nothing);
   });
   var btnSubmitPrimary_ = function (p) {
      return A4($Bootstrap$Html$Internal.btnc,
      "btn-primary",
      "submit",
      p,
      $Maybe.Nothing);
   };
   var btnSubmitPrimary$ = F2(function (c,
   p) {
      return A4($Bootstrap$Html$Internal.btnc,
      A2($Basics._op["++"],
      "btn-primary ",
      c),
      "submit",
      p,
      $Maybe.Nothing);
   });
   var btnLgDanger_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-danger",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgDanger$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-danger ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmDanger_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-danger",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmDanger$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-danger ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsDanger_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-danger ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsDanger$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-danger ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnDanger_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-danger",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnDanger$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-danger ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnLgWarning_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-warning",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgWarning$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-warning ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmWarning_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-warning",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmWarning$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-warning ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsWarning_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-warning ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsWarning$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-warning ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnWarning_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-warning",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnWarning$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-warning ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnLgInfo_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-info",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgInfo$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-info ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmInfo_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-info",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmInfo$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-info ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsInfo_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-info ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsInfo$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-info ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnInfo_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-info",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnInfo$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-info ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnLgSuccess_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-success",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgSuccess$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-success ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmSuccess_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-success",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmSuccess$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-success ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsSuccess_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-success ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsSuccess$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-success ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var panelDefault$ = F3(function (t,
   btns,
   bs) {
      return panelDefault_(_L.fromArray([panelHeading_(A2($Basics._op["++"],
                                        A2($List.map,
                                        $Basics.uncurry(btnXsSuccess$("pull-right")),
                                        $List.reverse(btns)),
                                        _L.fromArray([panelTitle_(t)])))
                                        ,panelBody_(bs)]));
   });
   var btnSuccess_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-success",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSuccess$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-success ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnLgPrimary_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-primary",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgPrimary$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-primary ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmPrimary_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-primary",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmPrimary$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-primary ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsPrimary_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-primary ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsPrimary$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-primary ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnPrimary_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-primary",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnPrimary$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-primary ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnLgDefault_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-lg btn-default",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnLgDefault$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-lg btn-default ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnSmDefault_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-sm btn-default",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnSmDefault$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-sm btn-default ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnXsDefault_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-xs btn-default ",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnXsDefault$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-xs btn-default  ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnDefault_ = function (p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         "btn-default",
         "button",
         p)($Maybe.Just($));
      };
   };
   var btnDefault$ = F2(function (c,
   p) {
      return function ($) {
         return A3($Bootstrap$Html$Internal.btnc,
         A2($Basics._op["++"],
         "btn-default ",
         c),
         "button",
         p)($Maybe.Just($));
      };
   });
   var btnParam = {_: {}
                  ,icon: $Maybe.Nothing
                  ,label: $Maybe.Nothing
                  ,tooltip: $Maybe.Nothing};
   var formGroup_ = $Html$Shorthand.div$({_: {}
                                         ,$class: "form-group"});
   var tableBodyStriped$ = function (c) {
      return $Html$Shorthand.table$({_: {}
                                    ,$class: A2($Basics._op["++"],
                                    "table table-body-striped ",
                                    c)});
   };
   var tableBodyStriped_ = tableBodyStriped$("");
   var tableStriped_ = tableBodyStriped$("");
   var tableStriped$ = function (c) {
      return $Html$Shorthand.table$({_: {}
                                    ,$class: A2($Basics._op["++"],
                                    "table table-striped ",
                                    c)});
   };
   var colLg_ = F4(function (xs,
   sm,
   md,
   lg) {
      return $Html$Shorthand.div$({_: {}
                                  ,$class: A2($Basics._op["++"],
                                  "col-xs-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(xs),
                                  A2($Basics._op["++"],
                                  " col-sm-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(sm),
                                  A2($Basics._op["++"],
                                  " col-md-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(md),
                                  A2($Basics._op["++"],
                                  " col-lg-",
                                  $Basics.toString(lg))))))))});
   });
   var colMd_ = F3(function (xs,
   sm,
   md) {
      return $Html$Shorthand.div$({_: {}
                                  ,$class: A2($Basics._op["++"],
                                  "col-xs-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(xs),
                                  A2($Basics._op["++"],
                                  " col-sm-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(sm),
                                  A2($Basics._op["++"],
                                  " col-md-",
                                  $Basics.toString(md))))))});
   });
   var colSm_ = F2(function (xs,
   sm) {
      return $Html$Shorthand.div$({_: {}
                                  ,$class: A2($Basics._op["++"],
                                  "col-xs-",
                                  A2($Basics._op["++"],
                                  $Basics.toString(xs),
                                  A2($Basics._op["++"],
                                  " col-sm-",
                                  $Basics.toString(sm))))});
   });
   var colXs_ = function (xs) {
      return $Html$Shorthand.div$({_: {}
                                  ,$class: A2($Basics._op["++"],
                                  "col-xs-",
                                  $Basics.toString(xs))});
   };
   var row_ = $Html$Shorthand.div$({_: {}
                                   ,$class: "row"});
   var containerFluid_ = $Html$Shorthand.div$({_: {}
                                              ,$class: "container-fluid"});
   var container_ = $Html$Shorthand.div$({_: {}
                                         ,$class: "container"});
   _elm.Bootstrap.Html.values = {_op: _op
                                ,container_: container_
                                ,containerFluid_: containerFluid_
                                ,row_: row_
                                ,colXs_: colXs_
                                ,colSm_: colSm_
                                ,colMd_: colMd_
                                ,colLg_: colLg_
                                ,tableStriped$: tableStriped$
                                ,tableStriped_: tableStriped_
                                ,tableBodyStriped$: tableBodyStriped$
                                ,tableBodyStriped_: tableBodyStriped_
                                ,formGroup_: formGroup_
                                ,btnParam: btnParam
                                ,btnDefault$: btnDefault$
                                ,btnDefault_: btnDefault_
                                ,btnXsDefault$: btnXsDefault$
                                ,btnXsDefault_: btnXsDefault_
                                ,btnSmDefault$: btnSmDefault$
                                ,btnSmDefault_: btnSmDefault_
                                ,btnLgDefault$: btnLgDefault$
                                ,btnLgDefault_: btnLgDefault_
                                ,btnPrimary$: btnPrimary$
                                ,btnPrimary_: btnPrimary_
                                ,btnXsPrimary$: btnXsPrimary$
                                ,btnXsPrimary_: btnXsPrimary_
                                ,btnSmPrimary$: btnSmPrimary$
                                ,btnSmPrimary_: btnSmPrimary_
                                ,btnLgPrimary$: btnLgPrimary$
                                ,btnLgPrimary_: btnLgPrimary_
                                ,btnSuccess$: btnSuccess$
                                ,btnSuccess_: btnSuccess_
                                ,btnXsSuccess$: btnXsSuccess$
                                ,btnXsSuccess_: btnXsSuccess_
                                ,btnSmSuccess$: btnSmSuccess$
                                ,btnSmSuccess_: btnSmSuccess_
                                ,btnLgSuccess$: btnLgSuccess$
                                ,btnLgSuccess_: btnLgSuccess_
                                ,btnInfo$: btnInfo$
                                ,btnInfo_: btnInfo_
                                ,btnXsInfo$: btnXsInfo$
                                ,btnXsInfo_: btnXsInfo_
                                ,btnSmInfo$: btnSmInfo$
                                ,btnSmInfo_: btnSmInfo_
                                ,btnLgInfo$: btnLgInfo$
                                ,btnLgInfo_: btnLgInfo_
                                ,btnWarning$: btnWarning$
                                ,btnWarning_: btnWarning_
                                ,btnXsWarning$: btnXsWarning$
                                ,btnXsWarning_: btnXsWarning_
                                ,btnSmWarning$: btnSmWarning$
                                ,btnSmWarning_: btnSmWarning_
                                ,btnLgWarning$: btnLgWarning$
                                ,btnLgWarning_: btnLgWarning_
                                ,btnDanger$: btnDanger$
                                ,btnDanger_: btnDanger_
                                ,btnXsDanger$: btnXsDanger$
                                ,btnXsDanger_: btnXsDanger_
                                ,btnSmDanger$: btnSmDanger$
                                ,btnSmDanger_: btnSmDanger_
                                ,btnLgDanger$: btnLgDanger$
                                ,btnLgDanger_: btnLgDanger_
                                ,btnSubmitPrimary$: btnSubmitPrimary$
                                ,btnSubmitPrimary_: btnSubmitPrimary_
                                ,btnSubmitXsPrimary$: btnSubmitXsPrimary$
                                ,btnSubmitXsPrimary_: btnSubmitXsPrimary_
                                ,btnSubmitSmPrimary$: btnSubmitSmPrimary$
                                ,btnSubmitSmPrimary_: btnSubmitSmPrimary_
                                ,btnSubmitLgPrimary$: btnSubmitLgPrimary$
                                ,btnSubmitLgPrimary_: btnSubmitLgPrimary_
                                ,skipNavigation_: skipNavigation_
                                ,glyphiconAsterisk$: glyphiconAsterisk$
                                ,glyphiconAsterisk_: glyphiconAsterisk_
                                ,glyphiconPlus$: glyphiconPlus$
                                ,glyphiconPlus_: glyphiconPlus_
                                ,glyphiconEuro$: glyphiconEuro$
                                ,glyphiconEuro_: glyphiconEuro_
                                ,glyphiconMinus$: glyphiconMinus$
                                ,glyphiconMinus_: glyphiconMinus_
                                ,glyphiconCloud$: glyphiconCloud$
                                ,glyphiconCloud_: glyphiconCloud_
                                ,glyphiconEnvelope$: glyphiconEnvelope$
                                ,glyphiconEnvelope_: glyphiconEnvelope_
                                ,glyphiconPencil$: glyphiconPencil$
                                ,glyphiconPencil_: glyphiconPencil_
                                ,glyphiconGlass$: glyphiconGlass$
                                ,glyphiconGlass_: glyphiconGlass_
                                ,glyphiconMusic$: glyphiconMusic$
                                ,glyphiconMusic_: glyphiconMusic_
                                ,glyphiconSearch$: glyphiconSearch$
                                ,glyphiconSearch_: glyphiconSearch_
                                ,glyphiconHeart$: glyphiconHeart$
                                ,glyphiconHeart_: glyphiconHeart_
                                ,glyphiconStar$: glyphiconStar$
                                ,glyphiconStar_: glyphiconStar_
                                ,glyphiconStarEmpty$: glyphiconStarEmpty$
                                ,glyphiconStarEmpty_: glyphiconStarEmpty_
                                ,glyphiconUser$: glyphiconUser$
                                ,glyphiconUser_: glyphiconUser_
                                ,glyphiconFilm$: glyphiconFilm$
                                ,glyphiconFilm_: glyphiconFilm_
                                ,glyphiconThLarge$: glyphiconThLarge$
                                ,glyphiconThLarge_: glyphiconThLarge_
                                ,glyphiconTh$: glyphiconTh$
                                ,glyphiconTh_: glyphiconTh_
                                ,glyphiconThList$: glyphiconThList$
                                ,glyphiconThList_: glyphiconThList_
                                ,glyphiconOk$: glyphiconOk$
                                ,glyphiconOk_: glyphiconOk_
                                ,glyphiconRemove$: glyphiconRemove$
                                ,glyphiconRemove_: glyphiconRemove_
                                ,glyphiconZoomIn$: glyphiconZoomIn$
                                ,glyphiconZoomIn_: glyphiconZoomIn_
                                ,glyphiconZoomOut$: glyphiconZoomOut$
                                ,glyphiconZoomOut_: glyphiconZoomOut_
                                ,glyphiconOff$: glyphiconOff$
                                ,glyphiconOff_: glyphiconOff_
                                ,glyphiconSignal$: glyphiconSignal$
                                ,glyphiconSignal_: glyphiconSignal_
                                ,glyphiconCog$: glyphiconCog$
                                ,glyphiconCog_: glyphiconCog_
                                ,glyphiconTrash$: glyphiconTrash$
                                ,glyphiconTrash_: glyphiconTrash_
                                ,glyphiconHome$: glyphiconHome$
                                ,glyphiconHome_: glyphiconHome_
                                ,glyphiconFile$: glyphiconFile$
                                ,glyphiconFile_: glyphiconFile_
                                ,glyphiconTime$: glyphiconTime$
                                ,glyphiconTime_: glyphiconTime_
                                ,glyphiconRoad$: glyphiconRoad$
                                ,glyphiconRoad_: glyphiconRoad_
                                ,glyphiconDownloadAlt$: glyphiconDownloadAlt$
                                ,glyphiconDownloadAlt_: glyphiconDownloadAlt_
                                ,glyphiconDownload$: glyphiconDownload$
                                ,glyphiconDownload_: glyphiconDownload_
                                ,glyphiconUpload$: glyphiconUpload$
                                ,glyphiconUpload_: glyphiconUpload_
                                ,glyphiconInbox$: glyphiconInbox$
                                ,glyphiconInbox_: glyphiconInbox_
                                ,glyphiconPlayCircle$: glyphiconPlayCircle$
                                ,glyphiconPlayCircle_: glyphiconPlayCircle_
                                ,glyphiconRepeat$: glyphiconRepeat$
                                ,glyphiconRepeat_: glyphiconRepeat_
                                ,glyphiconRefresh$: glyphiconRefresh$
                                ,glyphiconRefresh_: glyphiconRefresh_
                                ,glyphiconListAlt$: glyphiconListAlt$
                                ,glyphiconListAlt_: glyphiconListAlt_
                                ,glyphiconLock$: glyphiconLock$
                                ,glyphiconLock_: glyphiconLock_
                                ,glyphiconFlag$: glyphiconFlag$
                                ,glyphiconFlag_: glyphiconFlag_
                                ,glyphiconHeadphones$: glyphiconHeadphones$
                                ,glyphiconHeadphones_: glyphiconHeadphones_
                                ,glyphiconVolumeOff$: glyphiconVolumeOff$
                                ,glyphiconVolumeOff_: glyphiconVolumeOff_
                                ,glyphiconVolumeDown$: glyphiconVolumeDown$
                                ,glyphiconVolumeDown_: glyphiconVolumeDown_
                                ,glyphiconVolumeUp$: glyphiconVolumeUp$
                                ,glyphiconVolumeUp_: glyphiconVolumeUp_
                                ,glyphiconQrcode$: glyphiconQrcode$
                                ,glyphiconQrcode_: glyphiconQrcode_
                                ,glyphiconBarcode$: glyphiconBarcode$
                                ,glyphiconBarcode_: glyphiconBarcode_
                                ,glyphiconTag$: glyphiconTag$
                                ,glyphiconTag_: glyphiconTag_
                                ,glyphiconTags$: glyphiconTags$
                                ,glyphiconTags_: glyphiconTags_
                                ,glyphiconBook$: glyphiconBook$
                                ,glyphiconBook_: glyphiconBook_
                                ,glyphiconBookmark$: glyphiconBookmark$
                                ,glyphiconBookmark_: glyphiconBookmark_
                                ,glyphiconPrint$: glyphiconPrint$
                                ,glyphiconPrint_: glyphiconPrint_
                                ,glyphiconCamera$: glyphiconCamera$
                                ,glyphiconCamera_: glyphiconCamera_
                                ,glyphiconFont$: glyphiconFont$
                                ,glyphiconFont_: glyphiconFont_
                                ,glyphiconBold$: glyphiconBold$
                                ,glyphiconBold_: glyphiconBold_
                                ,glyphiconItalic$: glyphiconItalic$
                                ,glyphiconItalic_: glyphiconItalic_
                                ,glyphiconTextHeight$: glyphiconTextHeight$
                                ,glyphiconTextHeight_: glyphiconTextHeight_
                                ,glyphiconTextWidth$: glyphiconTextWidth$
                                ,glyphiconTextWidth_: glyphiconTextWidth_
                                ,glyphiconAlignLeft$: glyphiconAlignLeft$
                                ,glyphiconAlignLeft_: glyphiconAlignLeft_
                                ,glyphiconAlignCenter$: glyphiconAlignCenter$
                                ,glyphiconAlignCenter_: glyphiconAlignCenter_
                                ,glyphiconAlignRight$: glyphiconAlignRight$
                                ,glyphiconAlignRight_: glyphiconAlignRight_
                                ,glyphiconAlignJustify$: glyphiconAlignJustify$
                                ,glyphiconAlignJustify_: glyphiconAlignJustify_
                                ,glyphiconList$: glyphiconList$
                                ,glyphiconList_: glyphiconList_
                                ,glyphiconIndentLeft$: glyphiconIndentLeft$
                                ,glyphiconIndentLeft_: glyphiconIndentLeft_
                                ,glyphiconIndentRight$: glyphiconIndentRight$
                                ,glyphiconIndentRight_: glyphiconIndentRight_
                                ,glyphiconFacetimeVideo$: glyphiconFacetimeVideo$
                                ,glyphiconFacetimeVideo_: glyphiconFacetimeVideo_
                                ,glyphiconPicture$: glyphiconPicture$
                                ,glyphiconPicture_: glyphiconPicture_
                                ,glyphiconMapMarker$: glyphiconMapMarker$
                                ,glyphiconMapMarker_: glyphiconMapMarker_
                                ,glyphiconAdjust$: glyphiconAdjust$
                                ,glyphiconAdjust_: glyphiconAdjust_
                                ,glyphiconTint$: glyphiconTint$
                                ,glyphiconTint_: glyphiconTint_
                                ,glyphiconEdit$: glyphiconEdit$
                                ,glyphiconEdit_: glyphiconEdit_
                                ,glyphiconShare$: glyphiconShare$
                                ,glyphiconShare_: glyphiconShare_
                                ,glyphiconCheck$: glyphiconCheck$
                                ,glyphiconCheck_: glyphiconCheck_
                                ,glyphiconMove$: glyphiconMove$
                                ,glyphiconMove_: glyphiconMove_
                                ,glyphiconStepBackward$: glyphiconStepBackward$
                                ,glyphiconStepBackward_: glyphiconStepBackward_
                                ,glyphiconFastBackward$: glyphiconFastBackward$
                                ,glyphiconFastBackward_: glyphiconFastBackward_
                                ,glyphiconBackward$: glyphiconBackward$
                                ,glyphiconBackward_: glyphiconBackward_
                                ,glyphiconPlay$: glyphiconPlay$
                                ,glyphiconPlay_: glyphiconPlay_
                                ,glyphiconPause$: glyphiconPause$
                                ,glyphiconPause_: glyphiconPause_
                                ,glyphiconStop$: glyphiconStop$
                                ,glyphiconStop_: glyphiconStop_
                                ,glyphiconForward$: glyphiconForward$
                                ,glyphiconForward_: glyphiconForward_
                                ,glyphiconFastForward$: glyphiconFastForward$
                                ,glyphiconFastForward_: glyphiconFastForward_
                                ,glyphiconStepForward$: glyphiconStepForward$
                                ,glyphiconStepForward_: glyphiconStepForward_
                                ,glyphiconEject$: glyphiconEject$
                                ,glyphiconEject_: glyphiconEject_
                                ,glyphiconChevronLeft$: glyphiconChevronLeft$
                                ,glyphiconChevronLeft_: glyphiconChevronLeft_
                                ,glyphiconChevronRight$: glyphiconChevronRight$
                                ,glyphiconChevronRight_: glyphiconChevronRight_
                                ,glyphiconPlusSign$: glyphiconPlusSign$
                                ,glyphiconPlusSign_: glyphiconPlusSign_
                                ,glyphiconMinusSign$: glyphiconMinusSign$
                                ,glyphiconMinusSign_: glyphiconMinusSign_
                                ,glyphiconRemoveSign$: glyphiconRemoveSign$
                                ,glyphiconRemoveSign_: glyphiconRemoveSign_
                                ,glyphiconOkSign$: glyphiconOkSign$
                                ,glyphiconOkSign_: glyphiconOkSign_
                                ,glyphiconQuestionSign$: glyphiconQuestionSign$
                                ,glyphiconQuestionSign_: glyphiconQuestionSign_
                                ,glyphiconInfoSign$: glyphiconInfoSign$
                                ,glyphiconInfoSign_: glyphiconInfoSign_
                                ,glyphiconScreenshot$: glyphiconScreenshot$
                                ,glyphiconScreenshot_: glyphiconScreenshot_
                                ,glyphiconRemoveCircle$: glyphiconRemoveCircle$
                                ,glyphiconRemoveCircle_: glyphiconRemoveCircle_
                                ,glyphiconOkCircle$: glyphiconOkCircle$
                                ,glyphiconOkCircle_: glyphiconOkCircle_
                                ,glyphiconBanCircle$: glyphiconBanCircle$
                                ,glyphiconBanCircle_: glyphiconBanCircle_
                                ,glyphiconArrowLeft$: glyphiconArrowLeft$
                                ,glyphiconArrowLeft_: glyphiconArrowLeft_
                                ,glyphiconArrowRight$: glyphiconArrowRight$
                                ,glyphiconArrowRight_: glyphiconArrowRight_
                                ,glyphiconArrowUp$: glyphiconArrowUp$
                                ,glyphiconArrowUp_: glyphiconArrowUp_
                                ,glyphiconArrowDown$: glyphiconArrowDown$
                                ,glyphiconArrowDown_: glyphiconArrowDown_
                                ,glyphiconShareAlt$: glyphiconShareAlt$
                                ,glyphiconShareAlt_: glyphiconShareAlt_
                                ,glyphiconResizeFull$: glyphiconResizeFull$
                                ,glyphiconResizeFull_: glyphiconResizeFull_
                                ,glyphiconResizeSmall$: glyphiconResizeSmall$
                                ,glyphiconResizeSmall_: glyphiconResizeSmall_
                                ,glyphiconExclamationSign$: glyphiconExclamationSign$
                                ,glyphiconExclamationSign_: glyphiconExclamationSign_
                                ,glyphiconGift$: glyphiconGift$
                                ,glyphiconGift_: glyphiconGift_
                                ,glyphiconLeaf$: glyphiconLeaf$
                                ,glyphiconLeaf_: glyphiconLeaf_
                                ,glyphiconFire$: glyphiconFire$
                                ,glyphiconFire_: glyphiconFire_
                                ,glyphiconEyeOpen$: glyphiconEyeOpen$
                                ,glyphiconEyeOpen_: glyphiconEyeOpen_
                                ,glyphiconEyeClose$: glyphiconEyeClose$
                                ,glyphiconEyeClose_: glyphiconEyeClose_
                                ,glyphiconWarningSign$: glyphiconWarningSign$
                                ,glyphiconWarningSign_: glyphiconWarningSign_
                                ,glyphiconPlane$: glyphiconPlane$
                                ,glyphiconPlane_: glyphiconPlane_
                                ,glyphiconCalendar$: glyphiconCalendar$
                                ,glyphiconCalendar_: glyphiconCalendar_
                                ,glyphiconRandom$: glyphiconRandom$
                                ,glyphiconRandom_: glyphiconRandom_
                                ,glyphiconComment$: glyphiconComment$
                                ,glyphiconComment_: glyphiconComment_
                                ,glyphiconMagnet$: glyphiconMagnet$
                                ,glyphiconMagnet_: glyphiconMagnet_
                                ,glyphiconChevronUp$: glyphiconChevronUp$
                                ,glyphiconChevronUp_: glyphiconChevronUp_
                                ,glyphiconChevronDown$: glyphiconChevronDown$
                                ,glyphiconChevronDown_: glyphiconChevronDown_
                                ,glyphiconRetweet$: glyphiconRetweet$
                                ,glyphiconRetweet_: glyphiconRetweet_
                                ,glyphiconShoppingCart$: glyphiconShoppingCart$
                                ,glyphiconShoppingCart_: glyphiconShoppingCart_
                                ,glyphiconFolderClose$: glyphiconFolderClose$
                                ,glyphiconFolderClose_: glyphiconFolderClose_
                                ,glyphiconFolderOpen$: glyphiconFolderOpen$
                                ,glyphiconFolderOpen_: glyphiconFolderOpen_
                                ,glyphiconResizeVertical$: glyphiconResizeVertical$
                                ,glyphiconResizeVertical_: glyphiconResizeVertical_
                                ,glyphiconResizeHorizontal$: glyphiconResizeHorizontal$
                                ,glyphiconResizeHorizontal_: glyphiconResizeHorizontal_
                                ,glyphiconHdd$: glyphiconHdd$
                                ,glyphiconHdd_: glyphiconHdd_
                                ,glyphiconBullhorn$: glyphiconBullhorn$
                                ,glyphiconBullhorn_: glyphiconBullhorn_
                                ,glyphiconBell$: glyphiconBell$
                                ,glyphiconBell_: glyphiconBell_
                                ,glyphiconCertificate$: glyphiconCertificate$
                                ,glyphiconCertificate_: glyphiconCertificate_
                                ,glyphiconThumbsUp$: glyphiconThumbsUp$
                                ,glyphiconThumbsUp_: glyphiconThumbsUp_
                                ,glyphiconThumbsDown$: glyphiconThumbsDown$
                                ,glyphiconThumbsDown_: glyphiconThumbsDown_
                                ,glyphiconHandRight$: glyphiconHandRight$
                                ,glyphiconHandRight_: glyphiconHandRight_
                                ,glyphiconHandLeft$: glyphiconHandLeft$
                                ,glyphiconHandLeft_: glyphiconHandLeft_
                                ,glyphiconHandUp$: glyphiconHandUp$
                                ,glyphiconHandUp_: glyphiconHandUp_
                                ,glyphiconHandDown$: glyphiconHandDown$
                                ,glyphiconHandDown_: glyphiconHandDown_
                                ,glyphiconCircleArrowRight$: glyphiconCircleArrowRight$
                                ,glyphiconCircleArrowRight_: glyphiconCircleArrowRight_
                                ,glyphiconCircleArrowLeft$: glyphiconCircleArrowLeft$
                                ,glyphiconCircleArrowLeft_: glyphiconCircleArrowLeft_
                                ,glyphiconCircleArrowUp$: glyphiconCircleArrowUp$
                                ,glyphiconCircleArrowUp_: glyphiconCircleArrowUp_
                                ,glyphiconCircleArrowDown$: glyphiconCircleArrowDown$
                                ,glyphiconCircleArrowDown_: glyphiconCircleArrowDown_
                                ,glyphiconGlobe$: glyphiconGlobe$
                                ,glyphiconGlobe_: glyphiconGlobe_
                                ,glyphiconWrench$: glyphiconWrench$
                                ,glyphiconWrench_: glyphiconWrench_
                                ,glyphiconTasks$: glyphiconTasks$
                                ,glyphiconTasks_: glyphiconTasks_
                                ,glyphiconFilter$: glyphiconFilter$
                                ,glyphiconFilter_: glyphiconFilter_
                                ,glyphiconBriefcase$: glyphiconBriefcase$
                                ,glyphiconBriefcase_: glyphiconBriefcase_
                                ,glyphiconFullscreen$: glyphiconFullscreen$
                                ,glyphiconFullscreen_: glyphiconFullscreen_
                                ,glyphiconDashboard$: glyphiconDashboard$
                                ,glyphiconDashboard_: glyphiconDashboard_
                                ,glyphiconPaperclip$: glyphiconPaperclip$
                                ,glyphiconPaperclip_: glyphiconPaperclip_
                                ,glyphiconHeartEmpty$: glyphiconHeartEmpty$
                                ,glyphiconHeartEmpty_: glyphiconHeartEmpty_
                                ,glyphiconLink$: glyphiconLink$
                                ,glyphiconLink_: glyphiconLink_
                                ,glyphiconPhone$: glyphiconPhone$
                                ,glyphiconPhone_: glyphiconPhone_
                                ,glyphiconPushpin$: glyphiconPushpin$
                                ,glyphiconPushpin_: glyphiconPushpin_
                                ,glyphiconUsd$: glyphiconUsd$
                                ,glyphiconUsd_: glyphiconUsd_
                                ,glyphiconGbp$: glyphiconGbp$
                                ,glyphiconGbp_: glyphiconGbp_
                                ,glyphiconSort$: glyphiconSort$
                                ,glyphiconSort_: glyphiconSort_
                                ,glyphiconSortByAlphabet$: glyphiconSortByAlphabet$
                                ,glyphiconSortByAlphabet_: glyphiconSortByAlphabet_
                                ,glyphiconSortByAlphabetAlt$: glyphiconSortByAlphabetAlt$
                                ,glyphiconSortByAlphabetAlt_: glyphiconSortByAlphabetAlt_
                                ,glyphiconSortByOrder$: glyphiconSortByOrder$
                                ,glyphiconSortByOrder_: glyphiconSortByOrder_
                                ,glyphiconSortByOrderAlt$: glyphiconSortByOrderAlt$
                                ,glyphiconSortByOrderAlt_: glyphiconSortByOrderAlt_
                                ,glyphiconSortByAttributes$: glyphiconSortByAttributes$
                                ,glyphiconSortByAttributes_: glyphiconSortByAttributes_
                                ,glyphiconSortByAttributesAlt$: glyphiconSortByAttributesAlt$
                                ,glyphiconSortByAttributesAlt_: glyphiconSortByAttributesAlt_
                                ,glyphiconUnchecked$: glyphiconUnchecked$
                                ,glyphiconUnchecked_: glyphiconUnchecked_
                                ,glyphiconExpand$: glyphiconExpand$
                                ,glyphiconExpand_: glyphiconExpand_
                                ,glyphiconCollapseDown$: glyphiconCollapseDown$
                                ,glyphiconCollapseDown_: glyphiconCollapseDown_
                                ,glyphiconCollapseUp$: glyphiconCollapseUp$
                                ,glyphiconCollapseUp_: glyphiconCollapseUp_
                                ,glyphiconLogIn$: glyphiconLogIn$
                                ,glyphiconLogIn_: glyphiconLogIn_
                                ,glyphiconFlash$: glyphiconFlash$
                                ,glyphiconFlash_: glyphiconFlash_
                                ,glyphiconLogOut$: glyphiconLogOut$
                                ,glyphiconLogOut_: glyphiconLogOut_
                                ,glyphiconNewWindow$: glyphiconNewWindow$
                                ,glyphiconNewWindow_: glyphiconNewWindow_
                                ,glyphiconRecord$: glyphiconRecord$
                                ,glyphiconRecord_: glyphiconRecord_
                                ,glyphiconSave$: glyphiconSave$
                                ,glyphiconSave_: glyphiconSave_
                                ,glyphiconOpen$: glyphiconOpen$
                                ,glyphiconOpen_: glyphiconOpen_
                                ,glyphiconSaved$: glyphiconSaved$
                                ,glyphiconSaved_: glyphiconSaved_
                                ,glyphiconImport$: glyphiconImport$
                                ,glyphiconImport_: glyphiconImport_
                                ,glyphiconExport$: glyphiconExport$
                                ,glyphiconExport_: glyphiconExport_
                                ,glyphiconSend$: glyphiconSend$
                                ,glyphiconSend_: glyphiconSend_
                                ,glyphiconFloppyDisk$: glyphiconFloppyDisk$
                                ,glyphiconFloppyDisk_: glyphiconFloppyDisk_
                                ,glyphiconFloppySaved$: glyphiconFloppySaved$
                                ,glyphiconFloppySaved_: glyphiconFloppySaved_
                                ,glyphiconFloppyRemove$: glyphiconFloppyRemove$
                                ,glyphiconFloppyRemove_: glyphiconFloppyRemove_
                                ,glyphiconFloppySave$: glyphiconFloppySave$
                                ,glyphiconFloppySave_: glyphiconFloppySave_
                                ,glyphiconFloppyOpen$: glyphiconFloppyOpen$
                                ,glyphiconFloppyOpen_: glyphiconFloppyOpen_
                                ,glyphiconCreditCard$: glyphiconCreditCard$
                                ,glyphiconCreditCard_: glyphiconCreditCard_
                                ,glyphiconTransfer$: glyphiconTransfer$
                                ,glyphiconTransfer_: glyphiconTransfer_
                                ,glyphiconCutlery$: glyphiconCutlery$
                                ,glyphiconCutlery_: glyphiconCutlery_
                                ,glyphiconHeader$: glyphiconHeader$
                                ,glyphiconHeader_: glyphiconHeader_
                                ,glyphiconCompressed$: glyphiconCompressed$
                                ,glyphiconCompressed_: glyphiconCompressed_
                                ,glyphiconEarphone$: glyphiconEarphone$
                                ,glyphiconEarphone_: glyphiconEarphone_
                                ,glyphiconPhoneAlt$: glyphiconPhoneAlt$
                                ,glyphiconPhoneAlt_: glyphiconPhoneAlt_
                                ,glyphiconTower$: glyphiconTower$
                                ,glyphiconTower_: glyphiconTower_
                                ,glyphiconStats$: glyphiconStats$
                                ,glyphiconStats_: glyphiconStats_
                                ,glyphiconSdVideo$: glyphiconSdVideo$
                                ,glyphiconSdVideo_: glyphiconSdVideo_
                                ,glyphiconHdVideo$: glyphiconHdVideo$
                                ,glyphiconHdVideo_: glyphiconHdVideo_
                                ,glyphiconSubtitles$: glyphiconSubtitles$
                                ,glyphiconSubtitles_: glyphiconSubtitles_
                                ,glyphiconSoundStereo$: glyphiconSoundStereo$
                                ,glyphiconSoundStereo_: glyphiconSoundStereo_
                                ,glyphiconSoundDolby$: glyphiconSoundDolby$
                                ,glyphiconSoundDolby_: glyphiconSoundDolby_
                                ,glyphiconSound51$: glyphiconSound51$
                                ,glyphiconSound51_: glyphiconSound51_
                                ,glyphiconSound61$: glyphiconSound61$
                                ,glyphiconSound61_: glyphiconSound61_
                                ,glyphiconSound71$: glyphiconSound71$
                                ,glyphiconSound71_: glyphiconSound71_
                                ,glyphiconCopyrightMark$: glyphiconCopyrightMark$
                                ,glyphiconCopyrightMark_: glyphiconCopyrightMark_
                                ,glyphiconRegistrationMark$: glyphiconRegistrationMark$
                                ,glyphiconRegistrationMark_: glyphiconRegistrationMark_
                                ,glyphiconCloudDownload$: glyphiconCloudDownload$
                                ,glyphiconCloudDownload_: glyphiconCloudDownload_
                                ,glyphiconCloudUpload$: glyphiconCloudUpload$
                                ,glyphiconCloudUpload_: glyphiconCloudUpload_
                                ,glyphiconTreeConifer$: glyphiconTreeConifer$
                                ,glyphiconTreeConifer_: glyphiconTreeConifer_
                                ,glyphiconTreeDeciduous$: glyphiconTreeDeciduous$
                                ,glyphiconTreeDeciduous_: glyphiconTreeDeciduous_
                                ,navbar$: navbar$
                                ,navbarDefault$: navbarDefault$
                                ,navbarHeader_: navbarHeader_
                                ,panelDefault_: panelDefault_
                                ,panelHeading_: panelHeading_
                                ,panelBody_: panelBody_
                                ,panelTitle_: panelTitle_
                                ,panelDefault$: panelDefault$};
   return _elm.Bootstrap.Html.values;
};
Elm.Bootstrap = Elm.Bootstrap || {};
Elm.Bootstrap.Html = Elm.Bootstrap.Html || {};
Elm.Bootstrap.Html.Internal = Elm.Bootstrap.Html.Internal || {};
Elm.Bootstrap.Html.Internal.make = function (_elm) {
   "use strict";
   _elm.Bootstrap = _elm.Bootstrap || {};
   _elm.Bootstrap.Html = _elm.Bootstrap.Html || {};
   _elm.Bootstrap.Html.Internal = _elm.Bootstrap.Html.Internal || {};
   if (_elm.Bootstrap.Html.Internal.values)
   return _elm.Bootstrap.Html.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Bootstrap.Html.Internal",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Html$Shorthand = Elm.Html.Shorthand.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var btnc = F4(function (c,
   typ,
   _v0,
   click) {
      return function () {
         return function () {
            var filter = $List.filterMap($Basics.identity);
            return A2($Html.button,
            A2($List._op["::"],
            $Html$Attributes.type$(typ),
            A2($List._op["::"],
            $Html$Shorthand.class$(A2($Basics._op["++"],
            "btn ",
            c)),
            filter(_L.fromArray([A2($Maybe.map,
                                function ($) {
                                   return A2($Html$Events.on,
                                   "click",
                                   $Json$Decode.value)($Basics.always($));
                                },
                                click)
                                ,A2($Maybe.map,
                                $Html$Attributes.title,
                                _v0.tooltip)])))),
            function () {
               var _v2 = {ctor: "_Tuple2"
                         ,_0: _v0.icon
                         ,_1: _v0.label};
               switch (_v2.ctor)
               {case "_Tuple2":
                  switch (_v2._0.ctor)
                    {case "Just":
                       switch (_v2._1.ctor)
                         {case "Just":
                            return _L.fromArray([_v2._0._0
                                                ,$Html.text(A2($String.cons,
                                                _U.chr(" "),
                                                _v2._1._0))]);}
                         return _L.fromArray([_v2._0._0]);}
                    switch (_v2._1.ctor)
                    {case "Just":
                       return _L.fromArray([$Html.text(_v2._1._0)]);}
                    break;}
               return _L.fromArray([]);
            }());
         }();
      }();
   });
   var BtnParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,icon: a
             ,label: b
             ,tooltip: c};
   });
   _elm.Bootstrap.Html.Internal.values = {_op: _op
                                         ,BtnParam: BtnParam
                                         ,btnc: btnc};
   return _elm.Bootstrap.Html.Internal.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Char",
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,
   high,
   $char) {
      return function () {
         var code = toCode($char);
         return _U.cmp(code,
         toCode(low)) > -1 && _U.cmp(code,
         toCode(high)) < 1;
      }();
   });
   var isUpper = A2(isBetween,
   _U.chr("A"),
   _U.chr("Z"));
   var isLower = A2(isBetween,
   _U.chr("a"),
   _U.chr("z"));
   var isDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("9"));
   var isOctDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,
      _U.chr("A"),
      _U.chr("F"),
      $char));
   };
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 150 and 152"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 114 and 121");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 111");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgb: rgb
                       ,rgba: rgba
                       ,hsl: hsl
                       ,hsla: hsla
                       ,greyscale: greyscale
                       ,grayscale: grayscale
                       ,complement: complement
                       ,linear: linear
                       ,radial: radial
                       ,toRgb: toRgb
                       ,toHsl: toHsl
                       ,red: red
                       ,orange: orange
                       ,yellow: yellow
                       ,green: green
                       ,blue: blue
                       ,purple: purple
                       ,brown: brown
                       ,lightRed: lightRed
                       ,lightOrange: lightOrange
                       ,lightYellow: lightYellow
                       ,lightGreen: lightGreen
                       ,lightBlue: lightBlue
                       ,lightPurple: lightPurple
                       ,lightBrown: lightBrown
                       ,darkRed: darkRed
                       ,darkOrange: darkOrange
                       ,darkYellow: darkYellow
                       ,darkGreen: darkGreen
                       ,darkBlue: darkBlue
                       ,darkPurple: darkPurple
                       ,darkBrown: darkBrown
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,black: black
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray};
   return _elm.Color.values;
};
Elm.Common = Elm.Common || {};
Elm.Common.make = function (_elm) {
   "use strict";
   _elm.Common = _elm.Common || {};
   if (_elm.Common.values)
   return _elm.Common.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Common",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $String = Elm.String.make(_elm);
   var roundPct = function (x) {
      return function () {
         var pct = $Basics.toString(x * 100.0);
         return A2($Basics._op["++"],
         A2($String.left,4,pct),
         "%");
      }();
   };
   var orElse = F2(function (res,
   x) {
      return $Maybe.withDefault(x)($Result.toMaybe(res));
   });
   var argsort = function (arr) {
      return function () {
         var idxs = $Array.toIndexedList(arr);
         return $List.map($Basics.fst)(A2($List.sortBy,
         $Basics.snd,
         idxs));
      }();
   };
   var argmax = function ($) {
      return $Maybe.withDefault(-1)($List.head($List.reverse(argsort($))));
   };
   var nth = F2(function (i,arr) {
      return $Maybe.withDefault(0.0)(A2($Array.get,
      i,
      arr));
   });
   var toList = function (x) {
      return function () {
         switch (x.ctor)
         {case "Just":
            return _L.fromArray([x._0]);
            case "Nothing":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 14 and 16");
      }();
   };
   var last = function (arr) {
      return A2($Array.get,
      $Array.length(arr) - 1,
      arr);
   };
   _elm.Common.values = {_op: _op
                        ,last: last
                        ,toList: toList
                        ,nth: nth
                        ,argsort: argsort
                        ,argmax: argmax
                        ,orElse: orElse
                        ,roundPct: roundPct};
   return _elm.Common.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 408 and 416");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 397 and 405");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 173 and 179");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 129 and 135");
              }();}
         _U.badCase($moduleName,
         "between lines 124 and 135");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 138 and 140");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 100 and 121");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 70 and 72");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 385 and 394");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 56 and 60");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 145 and 157");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 330 and 332");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 369 and 371");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 235 and 239");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 244 and 248");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 253 and 255");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 377 and 382");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 314 and 319");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 273 and 277");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 271 and 300");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 185 and 189");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 192 and 197");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 206 and 211");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 199 and 204");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 190 and 211");
                    }();}
               _U.badCase($moduleName,
               "between lines 183 and 211");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 213 and 219");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 457, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 478 and 480");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.GraphData = Elm.GraphData || {};
Elm.GraphData.make = function (_elm) {
   "use strict";
   _elm.GraphData = _elm.GraphData || {};
   if (_elm.GraphData.values)
   return _elm.GraphData.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "GraphData",
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var graphRetrieve = $Signal.mailbox($Maybe.Nothing);
   var sendGraphData = function (d) {
      return A2($Signal.send,
      graphRetrieve.address,
      $Maybe.Just(d));
   };
   var link = A3($Json$Decode.tuple2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   $Json$Decode.string,
   $Json$Decode.string);
   var node = $Json$Decode.string;
   var graphDec = A3($Json$Decode.object2,
   $Model.GraphData,
   A2($Json$Decode._op[":="],
   "nodes",
   $Json$Decode.list(node)),
   A2($Json$Decode._op[":="],
   "links",
   $Json$Decode.list(link)));
   _elm.GraphData.values = {_op: _op
                           ,node: node
                           ,link: link
                           ,graphDec: graphDec
                           ,graphRetrieve: graphRetrieve
                           ,sendGraphData: sendGraphData};
   return _elm.GraphData.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 226, column 7 to 35");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FText = function (a) {
      return {ctor: "FText",_0: a};
   };
   var text = function (t) {
      return form(FText(t));
   };
   var FOutlinedText = F2(function (a,
   b) {
      return {ctor: "FOutlinedText"
             ,_0: a
             ,_1: b};
   });
   var outlinedText = F2(function (ls,
   t) {
      return form(A2(FOutlinedText,
      ls,
      t));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,collage: collage
                                  ,toForm: toForm
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,text: text
                                  ,outlinedText: outlinedText
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,rect: rect
                                  ,oval: oval
                                  ,square: square
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,polygon: polygon
                                  ,segment: segment
                                  ,path: path
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,defaultLine: defaultLine
                                  ,Form: Form
                                  ,LineStyle: LineStyle
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         A2($Maybe.withDefault,
         0,
         $List.maximum(ws)),
         A2($Maybe.withDefault,
         0,
         $List.maximum(hs)),
         A2(Flow,DOut,es));
      }();
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var maxOrZero = function (list) {
            return A2($Maybe.withDefault,
            0,
            $List.maximum(list));
         };
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DOut": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DUp": return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 362 and 373");
         }();
      }();
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   _elm.Graphics.Element.values = {_op: _op
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,leftAligned: leftAligned
                                  ,rightAligned: rightAligned
                                  ,centered: centered
                                  ,justified: justified
                                  ,show: show
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,color: color
                                  ,opacity: opacity
                                  ,link: link
                                  ,tag: tag
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,flow: flow
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,layers: layers
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,empty: empty
                                  ,spacer: spacer
                                  ,container: container
                                  ,middle: middle
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middleAt: middleAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};
Elm.History = Elm.History || {};
Elm.History.make = function (_elm) {
   "use strict";
   _elm.History = _elm.History || {};
   if (_elm.History.values)
   return _elm.History.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "History",
   $Native$History = Elm.Native.History.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var path = $Native$History.path;
   var hash = $Native$History.hash;
   var length = $Native$History.length;
   var forward = $Native$History.forward;
   var back = $Native$History.back;
   var go = $Native$History.go;
   var replacePath = $Native$History.replacePath;
   var setPath = $Native$History.setPath;
   _elm.History.values = {_op: _op
                         ,setPath: setPath
                         ,replacePath: replacePath
                         ,go: go
                         ,back: back
                         ,forward: forward
                         ,length: length
                         ,hash: hash
                         ,path: path};
   return _elm.History.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values)
   return _elm.Html.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   _elm.Html.values = {_op: _op
                      ,node: node
                      ,text: text
                      ,toElement: toElement
                      ,fromElement: fromElement
                      ,body: body
                      ,section: section
                      ,nav: nav
                      ,article: article
                      ,aside: aside
                      ,h1: h1
                      ,h2: h2
                      ,h3: h3
                      ,h4: h4
                      ,h5: h5
                      ,h6: h6
                      ,header: header
                      ,footer: footer
                      ,address: address
                      ,main$: main$
                      ,p: p
                      ,hr: hr
                      ,pre: pre
                      ,blockquote: blockquote
                      ,ol: ol
                      ,ul: ul
                      ,li: li
                      ,dl: dl
                      ,dt: dt
                      ,dd: dd
                      ,figure: figure
                      ,figcaption: figcaption
                      ,div: div
                      ,a: a
                      ,em: em
                      ,strong: strong
                      ,small: small
                      ,s: s
                      ,cite: cite
                      ,q: q
                      ,dfn: dfn
                      ,abbr: abbr
                      ,time: time
                      ,code: code
                      ,$var: $var
                      ,samp: samp
                      ,kbd: kbd
                      ,sub: sub
                      ,sup: sup
                      ,i: i
                      ,b: b
                      ,u: u
                      ,mark: mark
                      ,ruby: ruby
                      ,rt: rt
                      ,rp: rp
                      ,bdi: bdi
                      ,bdo: bdo
                      ,span: span
                      ,br: br
                      ,wbr: wbr
                      ,ins: ins
                      ,del: del
                      ,img: img
                      ,iframe: iframe
                      ,embed: embed
                      ,object: object
                      ,param: param
                      ,video: video
                      ,audio: audio
                      ,source: source
                      ,track: track
                      ,canvas: canvas
                      ,svg: svg
                      ,math: math
                      ,table: table
                      ,caption: caption
                      ,colgroup: colgroup
                      ,col: col
                      ,tbody: tbody
                      ,thead: thead
                      ,tfoot: tfoot
                      ,tr: tr
                      ,td: td
                      ,th: th
                      ,form: form
                      ,fieldset: fieldset
                      ,legend: legend
                      ,label: label
                      ,input: input
                      ,button: button
                      ,select: select
                      ,datalist: datalist
                      ,optgroup: optgroup
                      ,option: option
                      ,textarea: textarea
                      ,keygen: keygen
                      ,output: output
                      ,progress: progress
                      ,meter: meter
                      ,details: details
                      ,summary: summary
                      ,menuitem: menuitem
                      ,menu: menu};
   return _elm.Html.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Attributes",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var attribute = $VirtualDom.attribute;
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,
   string) {
      return A2(property,
      name,
      $Json$Encode.string(string));
   });
   var $class = function (name) {
      return A2(stringProperty,
      "className",
      name);
   };
   var id = function (name) {
      return A2(stringProperty,
      "id",
      name);
   };
   var title = function (name) {
      return A2(stringProperty,
      "title",
      name);
   };
   var accesskey = function ($char) {
      return A2(stringProperty,
      "accesskey",
      $String.fromList(_L.fromArray([$char])));
   };
   var contextmenu = function (value) {
      return A2(stringProperty,
      "contextmenu",
      value);
   };
   var dir = function (value) {
      return A2(stringProperty,
      "dir",
      value);
   };
   var draggable = function (value) {
      return A2(stringProperty,
      "draggable",
      value);
   };
   var dropzone = function (value) {
      return A2(stringProperty,
      "dropzone",
      value);
   };
   var itemprop = function (value) {
      return A2(stringProperty,
      "itemprop",
      value);
   };
   var lang = function (value) {
      return A2(stringProperty,
      "lang",
      value);
   };
   var tabindex = function (n) {
      return A2(stringProperty,
      "tabIndex",
      $Basics.toString(n));
   };
   var charset = function (value) {
      return A2(stringProperty,
      "charset",
      value);
   };
   var content = function (value) {
      return A2(stringProperty,
      "content",
      value);
   };
   var httpEquiv = function (value) {
      return A2(stringProperty,
      "httpEquiv",
      value);
   };
   var language = function (value) {
      return A2(stringProperty,
      "language",
      value);
   };
   var src = function (value) {
      return A2(stringProperty,
      "src",
      value);
   };
   var height = function (value) {
      return A2(stringProperty,
      "height",
      $Basics.toString(value));
   };
   var width = function (value) {
      return A2(stringProperty,
      "width",
      $Basics.toString(value));
   };
   var alt = function (value) {
      return A2(stringProperty,
      "alt",
      value);
   };
   var preload = function (value) {
      return A2(stringProperty,
      "preload",
      value);
   };
   var poster = function (value) {
      return A2(stringProperty,
      "poster",
      value);
   };
   var kind = function (value) {
      return A2(stringProperty,
      "kind",
      value);
   };
   var srclang = function (value) {
      return A2(stringProperty,
      "srclang",
      value);
   };
   var sandbox = function (value) {
      return A2(stringProperty,
      "sandbox",
      value);
   };
   var srcdoc = function (value) {
      return A2(stringProperty,
      "srcdoc",
      value);
   };
   var type$ = function (value) {
      return A2(stringProperty,
      "type",
      value);
   };
   var value = function (value) {
      return A2(stringProperty,
      "value",
      value);
   };
   var placeholder = function (value) {
      return A2(stringProperty,
      "placeholder",
      value);
   };
   var accept = function (value) {
      return A2(stringProperty,
      "accept",
      value);
   };
   var acceptCharset = function (value) {
      return A2(stringProperty,
      "acceptCharset",
      value);
   };
   var action = function (value) {
      return A2(stringProperty,
      "action",
      value);
   };
   var autocomplete = function (bool) {
      return A2(stringProperty,
      "autocomplete",
      bool ? "on" : "off");
   };
   var autosave = function (value) {
      return A2(stringProperty,
      "autosave",
      value);
   };
   var enctype = function (value) {
      return A2(stringProperty,
      "enctype",
      value);
   };
   var formaction = function (value) {
      return A2(stringProperty,
      "formaction",
      value);
   };
   var list = function (value) {
      return A2(stringProperty,
      "list",
      value);
   };
   var minlength = function (n) {
      return A2(stringProperty,
      "minLength",
      $Basics.toString(n));
   };
   var maxlength = function (n) {
      return A2(stringProperty,
      "maxLength",
      $Basics.toString(n));
   };
   var method = function (value) {
      return A2(stringProperty,
      "method",
      value);
   };
   var name = function (value) {
      return A2(stringProperty,
      "name",
      value);
   };
   var pattern = function (value) {
      return A2(stringProperty,
      "pattern",
      value);
   };
   var size = function (n) {
      return A2(stringProperty,
      "size",
      $Basics.toString(n));
   };
   var $for = function (value) {
      return A2(stringProperty,
      "htmlFor",
      value);
   };
   var form = function (value) {
      return A2(stringProperty,
      "form",
      value);
   };
   var max = function (value) {
      return A2(stringProperty,
      "max",
      value);
   };
   var min = function (value) {
      return A2(stringProperty,
      "min",
      value);
   };
   var step = function (n) {
      return A2(stringProperty,
      "step",
      n);
   };
   var cols = function (n) {
      return A2(stringProperty,
      "cols",
      $Basics.toString(n));
   };
   var rows = function (n) {
      return A2(stringProperty,
      "rows",
      $Basics.toString(n));
   };
   var wrap = function (value) {
      return A2(stringProperty,
      "wrap",
      value);
   };
   var usemap = function (value) {
      return A2(stringProperty,
      "useMap",
      value);
   };
   var shape = function (value) {
      return A2(stringProperty,
      "shape",
      value);
   };
   var coords = function (value) {
      return A2(stringProperty,
      "coords",
      value);
   };
   var challenge = function (value) {
      return A2(stringProperty,
      "challenge",
      value);
   };
   var keytype = function (value) {
      return A2(stringProperty,
      "keytype",
      value);
   };
   var align = function (value) {
      return A2(stringProperty,
      "align",
      value);
   };
   var cite = function (value) {
      return A2(stringProperty,
      "cite",
      value);
   };
   var href = function (value) {
      return A2(stringProperty,
      "href",
      value);
   };
   var target = function (value) {
      return A2(stringProperty,
      "target",
      value);
   };
   var downloadAs = function (value) {
      return A2(stringProperty,
      "download",
      value);
   };
   var hreflang = function (value) {
      return A2(stringProperty,
      "hreflang",
      value);
   };
   var media = function (value) {
      return A2(stringProperty,
      "media",
      value);
   };
   var ping = function (value) {
      return A2(stringProperty,
      "ping",
      value);
   };
   var rel = function (value) {
      return A2(stringProperty,
      "rel",
      value);
   };
   var datetime = function (value) {
      return A2(stringProperty,
      "datetime",
      value);
   };
   var pubdate = function (value) {
      return A2(stringProperty,
      "pubdate",
      value);
   };
   var start = function (n) {
      return A2(stringProperty,
      "start",
      $Basics.toString(n));
   };
   var colspan = function (n) {
      return A2(stringProperty,
      "colSpan",
      $Basics.toString(n));
   };
   var headers = function (value) {
      return A2(stringProperty,
      "headers",
      value);
   };
   var rowspan = function (n) {
      return A2(stringProperty,
      "rowSpan",
      $Basics.toString(n));
   };
   var scope = function (value) {
      return A2(stringProperty,
      "scope",
      value);
   };
   var manifest = function (value) {
      return A2(stringProperty,
      "manifest",
      value);
   };
   var boolProperty = F2(function (name,
   bool) {
      return A2(property,
      name,
      $Json$Encode.bool(bool));
   });
   var hidden = function (bool) {
      return A2(boolProperty,
      "hidden",
      bool);
   };
   var contenteditable = function (bool) {
      return A2(boolProperty,
      "contentEditable",
      bool);
   };
   var spellcheck = function (bool) {
      return A2(boolProperty,
      "spellcheck",
      bool);
   };
   var async = function (bool) {
      return A2(boolProperty,
      "async",
      bool);
   };
   var defer = function (bool) {
      return A2(boolProperty,
      "defer",
      bool);
   };
   var scoped = function (bool) {
      return A2(boolProperty,
      "scoped",
      bool);
   };
   var autoplay = function (bool) {
      return A2(boolProperty,
      "autoplay",
      bool);
   };
   var controls = function (bool) {
      return A2(boolProperty,
      "controls",
      bool);
   };
   var loop = function (bool) {
      return A2(boolProperty,
      "loop",
      bool);
   };
   var $default = function (bool) {
      return A2(boolProperty,
      "default",
      bool);
   };
   var seamless = function (bool) {
      return A2(boolProperty,
      "seamless",
      bool);
   };
   var checked = function (bool) {
      return A2(boolProperty,
      "checked",
      bool);
   };
   var selected = function (bool) {
      return A2(boolProperty,
      "selected",
      bool);
   };
   var autofocus = function (bool) {
      return A2(boolProperty,
      "autofocus",
      bool);
   };
   var disabled = function (bool) {
      return A2(boolProperty,
      "disabled",
      bool);
   };
   var multiple = function (bool) {
      return A2(boolProperty,
      "multiple",
      bool);
   };
   var novalidate = function (bool) {
      return A2(boolProperty,
      "noValidate",
      bool);
   };
   var readonly = function (bool) {
      return A2(boolProperty,
      "readOnly",
      bool);
   };
   var required = function (bool) {
      return A2(boolProperty,
      "required",
      bool);
   };
   var ismap = function (value) {
      return A2(boolProperty,
      "isMap",
      value);
   };
   var download = function (bool) {
      return A2(boolProperty,
      "download",
      bool);
   };
   var reversed = function (bool) {
      return A2(boolProperty,
      "reversed",
      bool);
   };
   var classList = function (list) {
      return $class($String.join(" ")($List.map($Basics.fst)($List.filter($Basics.snd)(list))));
   };
   var style = function (props) {
      return property("style")($Json$Encode.object($List.map(function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: _v0._0
                      ,_1: $Json$Encode.string(_v0._1)};}
            _U.badCase($moduleName,
            "on line 133, column 35 to 57");
         }();
      })(props)));
   };
   var key = function (k) {
      return A2(stringProperty,
      "key",
      k);
   };
   _elm.Html.Attributes.values = {_op: _op
                                 ,key: key
                                 ,style: style
                                 ,classList: classList
                                 ,property: property
                                 ,stringProperty: stringProperty
                                 ,boolProperty: boolProperty
                                 ,attribute: attribute
                                 ,$class: $class
                                 ,hidden: hidden
                                 ,id: id
                                 ,title: title
                                 ,accesskey: accesskey
                                 ,contenteditable: contenteditable
                                 ,contextmenu: contextmenu
                                 ,dir: dir
                                 ,draggable: draggable
                                 ,dropzone: dropzone
                                 ,itemprop: itemprop
                                 ,lang: lang
                                 ,spellcheck: spellcheck
                                 ,tabindex: tabindex
                                 ,async: async
                                 ,charset: charset
                                 ,content: content
                                 ,defer: defer
                                 ,httpEquiv: httpEquiv
                                 ,language: language
                                 ,scoped: scoped
                                 ,src: src
                                 ,height: height
                                 ,width: width
                                 ,alt: alt
                                 ,autoplay: autoplay
                                 ,controls: controls
                                 ,loop: loop
                                 ,preload: preload
                                 ,poster: poster
                                 ,$default: $default
                                 ,kind: kind
                                 ,srclang: srclang
                                 ,sandbox: sandbox
                                 ,seamless: seamless
                                 ,srcdoc: srcdoc
                                 ,type$: type$
                                 ,value: value
                                 ,checked: checked
                                 ,placeholder: placeholder
                                 ,selected: selected
                                 ,accept: accept
                                 ,acceptCharset: acceptCharset
                                 ,action: action
                                 ,autocomplete: autocomplete
                                 ,autofocus: autofocus
                                 ,autosave: autosave
                                 ,disabled: disabled
                                 ,enctype: enctype
                                 ,formaction: formaction
                                 ,list: list
                                 ,minlength: minlength
                                 ,maxlength: maxlength
                                 ,method: method
                                 ,multiple: multiple
                                 ,name: name
                                 ,novalidate: novalidate
                                 ,pattern: pattern
                                 ,readonly: readonly
                                 ,required: required
                                 ,size: size
                                 ,$for: $for
                                 ,form: form
                                 ,max: max
                                 ,min: min
                                 ,step: step
                                 ,cols: cols
                                 ,rows: rows
                                 ,wrap: wrap
                                 ,ismap: ismap
                                 ,usemap: usemap
                                 ,shape: shape
                                 ,coords: coords
                                 ,challenge: challenge
                                 ,keytype: keytype
                                 ,align: align
                                 ,cite: cite
                                 ,href: href
                                 ,target: target
                                 ,download: download
                                 ,downloadAs: downloadAs
                                 ,hreflang: hreflang
                                 ,media: media
                                 ,ping: ping
                                 ,rel: rel
                                 ,datetime: datetime
                                 ,pubdate: pubdate
                                 ,reversed: reversed
                                 ,start: start
                                 ,colspan: colspan
                                 ,headers: headers
                                 ,rowspan: rowspan
                                 ,scope: scope
                                 ,manifest: manifest};
   return _elm.Html.Attributes.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.Extra = Elm.Html.Attributes.Extra || {};
Elm.Html.Attributes.Extra.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   _elm.Html.Attributes.Extra = _elm.Html.Attributes.Extra || {};
   if (_elm.Html.Attributes.Extra.values)
   return _elm.Html.Attributes.Extra.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Attributes.Extra",
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm);
   var optimum = $Html$Attributes.stringProperty("optimum");
   var high = $Html$Attributes.stringProperty("high");
   var low = $Html$Attributes.stringProperty("low");
   var role = function (r) {
      return A2($Html$Attributes.attribute,
      "role",
      r);
   };
   var intProperty = F2(function (name,
   $int) {
      return A2($Html$Attributes.property,
      name,
      $Json$Encode.$int($int));
   });
   var valueAsInt = function (value) {
      return A2(intProperty,
      "valueAsNumber",
      value);
   };
   var floatProperty = F2(function (name,
   $float) {
      return A2($Html$Attributes.property,
      name,
      $Json$Encode.$float($float));
   });
   var valueAsFloat = function (value) {
      return A2(floatProperty,
      "valueAsNumber",
      value);
   };
   var volume = floatProperty("volume");
   _elm.Html.Attributes.Extra.values = {_op: _op
                                       ,floatProperty: floatProperty
                                       ,intProperty: intProperty
                                       ,valueAsFloat: valueAsFloat
                                       ,valueAsInt: valueAsInt
                                       ,role: role
                                       ,low: low
                                       ,high: high
                                       ,optimum: optimum
                                       ,volume: volume};
   return _elm.Html.Attributes.Extra.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values)
   return _elm.Html.Events.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Events",
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var keyCode = A2($Json$Decode._op[":="],
   "keyCode",
   $Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"checked"]),
   $Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"value"]),
   $Json$Decode.string);
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,
   addr,
   msg) {
      return A3(on,
      name,
      $Json$Decode.value,
      function (_v0) {
         return function () {
            return A2($Signal.message,
            addr,
            msg);
         }();
      });
   });
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,
   addr,
   handler) {
      return A3(on,
      name,
      keyCode,
      function (code) {
         return A2($Signal.message,
         addr,
         handler(code));
      });
   });
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   _elm.Html.Events.values = {_op: _op
                             ,onBlur: onBlur
                             ,onFocus: onFocus
                             ,onSubmit: onSubmit
                             ,onKeyUp: onKeyUp
                             ,onKeyDown: onKeyDown
                             ,onKeyPress: onKeyPress
                             ,onClick: onClick
                             ,onDoubleClick: onDoubleClick
                             ,onMouseMove: onMouseMove
                             ,onMouseDown: onMouseDown
                             ,onMouseUp: onMouseUp
                             ,onMouseEnter: onMouseEnter
                             ,onMouseLeave: onMouseLeave
                             ,onMouseOver: onMouseOver
                             ,onMouseOut: onMouseOut
                             ,on: on
                             ,targetValue: targetValue
                             ,targetChecked: targetChecked
                             ,keyCode: keyCode};
   return _elm.Html.Events.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.Extra = Elm.Html.Events.Extra || {};
Elm.Html.Events.Extra.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   _elm.Html.Events.Extra = _elm.Html.Events.Extra || {};
   if (_elm.Html.Events.Extra.values)
   return _elm.Html.Events.Extra.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Events.Extra",
   $Basics = Elm.Basics.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $String = Elm.String.make(_elm);
   var targetValueIntParse = A2($Json$Decode.customDecoder,
   $Html$Events.targetValue,
   $String.toInt);
   var targetValueFloatParse = A2($Json$Decode.customDecoder,
   $Html$Events.targetValue,
   $String.toFloat);
   var targetValueMaybe = A2($Json$Decode.customDecoder,
   $Html$Events.targetValue,
   function (s) {
      return $Result.Ok(_U.eq(s,
      "") ? $Maybe.Nothing : $Maybe.Just(s));
   });
   var targetValueMaybeInt = function () {
      var traverse = F2(function (f,
      mx) {
         return function () {
            switch (mx.ctor)
            {case "Just":
               return A2($Result.map,
                 $Maybe.Just,
                 f(mx._0));
               case "Nothing":
               return $Result.Ok($Maybe.Nothing);}
            _U.badCase($moduleName,
            "between lines 92 and 95");
         }();
      });
      return A2($Json$Decode.customDecoder,
      targetValueMaybe,
      traverse($String.toInt));
   }();
   var targetValueMaybeFloatParse = function () {
      var traverse = F2(function (f,
      mx) {
         return function () {
            switch (mx.ctor)
            {case "Just":
               return A2($Result.map,
                 $Maybe.Just,
                 f(mx._0));
               case "Nothing":
               return $Result.Ok($Maybe.Nothing);}
            _U.badCase($moduleName,
            "between lines 116 and 119");
         }();
      });
      return A2($Json$Decode.customDecoder,
      targetValueMaybe,
      traverse($String.toFloat));
   }();
   var targetValueMaybeIntParse = function () {
      var traverse = F2(function (f,
      mx) {
         return function () {
            switch (mx.ctor)
            {case "Just":
               return A2($Result.map,
                 $Maybe.Just,
                 f(mx._0));
               case "Nothing":
               return $Result.Ok($Maybe.Nothing);}
            _U.badCase($moduleName,
            "between lines 126 and 129");
         }();
      });
      return A2($Json$Decode.customDecoder,
      targetValueMaybe,
      traverse($String.toInt));
   }();
   var targetValueInt = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"valueAsNumber"]),
   $Json$Decode.$int);
   var targetValueFloat = $Json$Decode.customDecoder(A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"valueAsNumber"]),
   $Json$Decode.$float))(function (v) {
      return $Basics.isNaN(v) ? $Result.Err("Not a number") : $Result.Ok(v);
   });
   var targetValueMaybeFloat = A2($Json$Decode.andThen,
   targetValueMaybe,
   function (mval) {
      return function () {
         switch (mval.ctor)
         {case "Just":
            return A2($Json$Decode.map,
              $Maybe.Just,
              targetValueFloat);
            case "Nothing":
            return $Json$Decode.succeed($Maybe.Nothing);}
         _U.badCase($moduleName,
         "between lines 84 and 89");
      }();
   });
   var charCode = A2($Json$Decode.map,
   function ($) {
      return $Maybe.map($Basics.fst)($String.uncons($));
   },
   A2($Json$Decode._op[":="],
   "charCode",
   $Json$Decode.string));
   _elm.Html.Events.Extra.values = {_op: _op
                                   ,charCode: charCode
                                   ,targetValueFloat: targetValueFloat
                                   ,targetValueInt: targetValueInt
                                   ,targetValueMaybe: targetValueMaybe
                                   ,targetValueMaybeFloat: targetValueMaybeFloat
                                   ,targetValueMaybeInt: targetValueMaybeInt
                                   ,targetValueFloatParse: targetValueFloatParse
                                   ,targetValueIntParse: targetValueIntParse
                                   ,targetValueMaybeFloatParse: targetValueMaybeFloatParse
                                   ,targetValueMaybeIntParse: targetValueMaybeIntParse};
   return _elm.Html.Events.Extra.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Shorthand = Elm.Html.Shorthand || {};
Elm.Html.Shorthand.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Shorthand = _elm.Html.Shorthand || {};
   if (_elm.Html.Shorthand.values)
   return _elm.Html.Shorthand.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Shorthand",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Attributes$Extra = Elm.Html.Attributes.Extra.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Html$Events$Extra = Elm.Html.Events.Extra.make(_elm),
   $Html$Shorthand$Event = Elm.Html.Shorthand.Event.make(_elm),
   $Html$Shorthand$Internal = Elm.Html.Shorthand.Internal.make(_elm),
   $Html$Shorthand$Type = Elm.Html.Shorthand.Type.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var meter$ = F2(function (p,t) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return A2($Html.meter,
         A2($Basics._op["++"],
         _L.fromArray([$Html$Attributes.value($Basics.toString(p.value))
                      ,$Html$Attributes.min($Basics.toString($Basics.min))
                      ,$Html$Attributes.max($Basics.toString(p.max))]),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes$Extra.low($Basics.toString($));
                             },
                             p.low)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes$Extra.high($Basics.toString($));
                             },
                             p.high)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes$Extra.optimum($Basics.toString($));
                             },
                             p.optimum)]))),
         _L.fromArray([$Html.text(t)]));
      }();
   });
   var progress$ = F2(function (p,
   t) {
      return A2($Html.progress,
      _L.fromArray([$Html$Attributes.value($Basics.toString(p.value))
                   ,$Html$Attributes.max($Basics.toString(p.max))]),
      _L.fromArray([$Html.text(t)]));
   });
   var option$ = function (p) {
      return A2($Html.option,
      _L.fromArray([A2($Html$Attributes.stringProperty,
                   "label",
                   p.label)
                   ,$Html$Attributes.value($Basics.toString(p.value))
                   ,$Html$Attributes.selected(p.selected)]),
      _L.fromArray([]));
   };
   var option_ = F2(function (val,
   sel) {
      return A2($Html.option,
      _L.fromArray([$Html$Attributes.selected(sel)]),
      _L.fromArray([$Html.text(val)]));
   });
   var buttonReset_ = function (t) {
      return A2($Html.button,
      _L.fromArray([$Html$Attributes.type$("reset")]),
      _L.fromArray([$Html.text(t)]));
   };
   var buttonSubmit_ = function (t) {
      return A2($Html.button,
      _L.fromArray([$Html$Attributes.type$("submit")]),
      _L.fromArray([$Html.text(t)]));
   };
   var buttonLink_ = F3(function (t,
   clickAddr,
   click) {
      return A2($Html.button,
      _L.fromArray([$Html$Attributes.type$("button")
                   ,A2($Html$Events.onClick,
                   clickAddr,
                   click)]),
      _L.fromArray([$Html.text(t)]));
   });
   var button_ = F3(function (t,
   clickAddr,
   click) {
      return A2($Html.button,
      _L.fromArray([$Html$Attributes.type$("button")
                   ,A2($Html$Events.onClick,
                   clickAddr,
                   click)]),
      _L.fromArray([$Html.text(t)]));
   });
   var label_ = F2(function ($for,
   t) {
      return A2($Html.label,
      _L.fromArray([$Html$Attributes.$for($for)]),
      _L.fromArray([$Html.text(t)]));
   });
   var legend_ = function (t) {
      return A2($Html.legend,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var fieldset_ = function (disabled) {
      return $Html.fieldset(_L.fromArray([$Html$Attributes.disabled(disabled)]));
   };
   var th_ = $Html.th(_L.fromArray([]));
   var td_ = $Html.td(_L.fromArray([]));
   var tr_ = $Html.tr(_L.fromArray([]));
   var tfoot_ = $Html.tfoot(_L.fromArray([]));
   var thead_ = $Html.thead(_L.fromArray([]));
   var tbody_ = $Html.tbody(_L.fromArray([]));
   var caption_ = $Html.caption(_L.fromArray([]));
   var table_ = $Html.table(_L.fromArray([]));
   var audio_ = function (url) {
      return A2($Html.audio,
      _L.fromArray([$Html$Attributes.src(url)]),
      _L.fromArray([]));
   };
   var video_ = function (url) {
      return A2($Html.video,
      _L.fromArray([$Html$Attributes.src(url)]),
      _L.fromArray([]));
   };
   var param$ = F2(function (n,v) {
      return A2($Html.param,
      _L.fromArray([$Html$Attributes.name(n)
                   ,$Html$Attributes.value(v)]),
      _L.fromArray([]));
   });
   var img_ = F4(function (w,
   h,
   s,
   a) {
      return A2($Html.img,
      _L.fromArray([$Html$Attributes.width(w)
                   ,$Html$Attributes.height(h)
                   ,$Html$Attributes.src(s)
                   ,$Html$Attributes.alt(a)]),
      _L.fromArray([]));
   });
   var del_ = $Html.del(_L.fromArray([]));
   var ins_ = $Html.ins(_L.fromArray([]));
   var wbr$ = A2($Html.wbr,
   _L.fromArray([]),
   _L.fromArray([]));
   var br$ = A2($Html.br,
   _L.fromArray([]),
   _L.fromArray([]));
   var span_ = $Html.span(_L.fromArray([]));
   var bdo$ = function (dir) {
      return $Html.bdo(_L.fromArray([$Html$Attributes.dir(function () {
         switch (dir.ctor)
         {case "AutoDirection":
            return "auto";
            case "LeftToRight":
            return "ltr";
            case "RightToLeft":
            return "rtl";}
         _U.badCase($moduleName,
         "between lines 1016 and 1020");
      }())]));
   };
   var bdi_ = function (t) {
      return A2($Html.bdi,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var rp_ = function (t) {
      return A2($Html.rp,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var rt_ = function (t) {
      return A2($Html.rt,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var ruby_ = $Html.ruby(_L.fromArray([]));
   var mark_ = $Html.mark(_L.fromArray([]));
   var u_ = function (t) {
      return A2($Html.u,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var b_ = function (t) {
      return A2($Html.b,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var i_ = function (t) {
      return A2($Html.i,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var sup_ = function (t) {
      return A2($Html.sup,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var sub_ = function (t) {
      return A2($Html.sub,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var kbd_ = $Html.kbd(_L.fromArray([]));
   var samp_ = $Html.samp(_L.fromArray([]));
   var var_ = function (t) {
      return A2($Html.$var,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var code_ = $Html.code(_L.fromArray([]));
   var abbr_ = function (t) {
      return A2($Html.abbr,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var q_ = function (t) {
      return A2($Html.q,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var cite_ = $Html.cite(_L.fromArray([]));
   var s_ = function (t) {
      return A2($Html.s,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var small_ = function (t) {
      return A2($Html.small,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var strong_ = function (t) {
      return A2($Html.em,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var em_ = function (t) {
      return A2($Html.em,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var a_ = F2(function (href,t) {
      return A2($Html.a,
      _L.fromArray([$Html$Attributes.href(href)]),
      _L.fromArray([$Html.text(t)]));
   });
   var div_ = $Html.div(_L.fromArray([]));
   var figcaption_ = $Html.figcaption(_L.fromArray([]));
   var dd_ = $Html.dd(_L.fromArray([]));
   var dl_ = $Html.dl(_L.fromArray([]));
   var li_ = $Html.li(_L.fromArray([]));
   var ul_ = $Html.ul(_L.fromArray([]));
   var ol_ = $Html.ol(_L.fromArray([]));
   var blockquote_ = $Html.blockquote(_L.fromArray([]));
   var pre_ = $Html.pre(_L.fromArray([]));
   var hr_ = A2($Html.hr,
   _L.fromArray([]),
   _L.fromArray([]));
   var p_ = $Html.p(_L.fromArray([]));
   var main_ = $Html.main$(_L.fromArray([]));
   var address_ = $Html.address(_L.fromArray([]));
   var footer_ = $Html.footer(_L.fromArray([]));
   var header_ = $Html.header(_L.fromArray([]));
   var h6_ = function (t) {
      return A2($Html.h6,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var h5_ = function (t) {
      return A2($Html.h5,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var h4_ = function (t) {
      return A2($Html.h4,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var h3_ = function (t) {
      return A2($Html.h3,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var h2_ = function (t) {
      return A2($Html.h2,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var h1_ = function (t) {
      return A2($Html.h1,
      _L.fromArray([]),
      _L.fromArray([$Html.text(t)]));
   };
   var nav_ = $Html.nav(_L.fromArray([]));
   var body_ = $Html.body(_L.fromArray([]));
   var class$ = $Html$Shorthand$Internal.class$;
   var body$ = function (p) {
      return $Html.body(_L.fromArray([class$(p.$class)]));
   };
   var nav$ = function (p) {
      return $Html.nav(_L.fromArray([class$(p.$class)]));
   };
   var h1$ = function (p) {
      return $Html.h1(_L.fromArray([class$(p.$class)]));
   };
   var h2$ = function (p) {
      return $Html.h2(_L.fromArray([class$(p.$class)]));
   };
   var h3$ = function (p) {
      return $Html.h3(_L.fromArray([class$(p.$class)]));
   };
   var h4$ = function (p) {
      return $Html.h4(_L.fromArray([class$(p.$class)]));
   };
   var h5$ = function (p) {
      return $Html.h5(_L.fromArray([class$(p.$class)]));
   };
   var h6$ = function (p) {
      return $Html.h6(_L.fromArray([class$(p.$class)]));
   };
   var header$ = function (p) {
      return $Html.header(_L.fromArray([class$(p.$class)]));
   };
   var footer$ = function (p) {
      return $Html.footer(_L.fromArray([class$(p.$class)]));
   };
   var address$ = function (p) {
      return $Html.address(_L.fromArray([class$(p.$class)]));
   };
   var p$ = function (param) {
      return $Html.p(_L.fromArray([class$(param.$class)]));
   };
   var pre$ = function (p) {
      return $Html.pre(_L.fromArray([class$(p.$class)]));
   };
   var blockquote$ = function (p) {
      return $Html.blockquote(_L.fromArray([class$(p.$class)
                                           ,$Html$Attributes.cite(p.cite)]));
   };
   var ol$ = function (p) {
      return $Html.ol(_L.fromArray([class$(p.$class)]));
   };
   var ul$ = function (p) {
      return $Html.ul(_L.fromArray([class$(p.$class)]));
   };
   var li$ = function (p) {
      return $Html.li(_L.fromArray([class$(p.$class)]));
   };
   var dl$ = function (p) {
      return $Html.dl(_L.fromArray([class$(p.$class)]));
   };
   var dd$ = function (p) {
      return $Html.dd(_L.fromArray([class$(p.$class)]));
   };
   var figcaption$ = function (p) {
      return $Html.figcaption(_L.fromArray([class$(p.$class)]));
   };
   var div$ = function (p) {
      return $Html.div(_L.fromArray([class$(p.$class)]));
   };
   var a$ = function (p) {
      return $Html.a(_L.fromArray([class$(p.$class)
                                  ,$Html$Attributes.href(p.href)]));
   };
   var em$ = function (p) {
      return $Html.em(_L.fromArray([class$(p.$class)]));
   };
   var strong$ = function (p) {
      return $Html.strong(_L.fromArray([class$(p.$class)]));
   };
   var small$ = function (p) {
      return $Html.small(_L.fromArray([class$(p.$class)]));
   };
   var s$ = function (p) {
      return $Html.s(_L.fromArray([class$(p.$class)]));
   };
   var cite$ = function (p) {
      return $Html.cite(_L.fromArray([class$(p.$class)]));
   };
   var q$ = function (p) {
      return $Html.q(_L.fromArray([class$(p.$class)
                                  ,$Html$Attributes.cite(p.cite)]));
   };
   var abbr$ = function (p) {
      return $Html.abbr(_L.fromArray([class$(p.$class)]));
   };
   var code$ = function (p) {
      return $Html.code(_L.fromArray([class$(p.$class)]));
   };
   var var$ = function (p) {
      return $Html.$var(_L.fromArray([class$(p.$class)]));
   };
   var samp$ = function (p) {
      return $Html.samp(_L.fromArray([class$(p.$class)]));
   };
   var kbd$ = function (p) {
      return $Html.kbd(_L.fromArray([class$(p.$class)]));
   };
   var sub$ = function (p) {
      return $Html.sub(_L.fromArray([class$(p.$class)]));
   };
   var sup$ = function (p) {
      return $Html.sup(_L.fromArray([class$(p.$class)]));
   };
   var i$ = function (p) {
      return $Html.i(_L.fromArray([class$(p.$class)]));
   };
   var b$ = function (p) {
      return $Html.b(_L.fromArray([class$(p.$class)]));
   };
   var u$ = function (p) {
      return $Html.u(_L.fromArray([class$(p.$class)]));
   };
   var mark$ = function (p) {
      return $Html.mark(_L.fromArray([class$(p.$class)]));
   };
   var ruby$ = function (p) {
      return $Html.ruby(_L.fromArray([class$(p.$class)]));
   };
   var rt$ = function (p) {
      return $Html.rt(_L.fromArray([class$(p.$class)]));
   };
   var rp$ = function (p) {
      return $Html.rp(_L.fromArray([class$(p.$class)]));
   };
   var bdi$ = function (p) {
      return $Html.bdi(_L.fromArray([class$(p.$class)]));
   };
   var span$ = function (p) {
      return $Html.span(_L.fromArray([class$(p.$class)]));
   };
   var ins$ = function (p) {
      return $Html.ins(_L.fromArray([class$(p.$class)
                                    ,$Html$Attributes.cite(p.cite)
                                    ,$Html$Attributes.datetime(p.datetime)]));
   };
   var del$ = function (p) {
      return $Html.del(_L.fromArray([class$(p.$class)
                                    ,$Html$Attributes.cite(p.cite)
                                    ,$Html$Attributes.datetime(p.datetime)]));
   };
   var img$ = function (p) {
      return A2($Html.img,
      _L.fromArray([class$(p.$class)
                   ,$Html$Attributes.src(p.src)
                   ,$Html$Attributes.width(p.width)
                   ,$Html$Attributes.height(p.height)
                   ,$Html$Attributes.alt(p.alt)]),
      _L.fromArray([]));
   };
   var video$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return $Html.video(A2($Basics._op["++"],
         _L.fromArray([class$(p.$class)
                      ,$Html$Attributes.width(p.width)
                      ,$Html$Attributes.height(p.height)
                      ,$Html$Attributes.autoplay(p.autoplay)
                      ,$Html$Attributes.controls(p.controls)
                      ,$Html$Attributes.loop(p.loop)]),
         filter(_L.fromArray([A2($Maybe.map,
                             $Html$Attributes.src,
                             p.src)
                             ,A2($Maybe.map,
                             $Html$Attributes.stringProperty("preload"),
                             p.preload)
                             ,A2($Maybe.map,
                             $Html$Attributes.poster,
                             p.poster)
                             ,A2($Maybe.map,
                             $Html$Attributes$Extra.volume,
                             p.volume)]))));
      }();
   };
   var audio$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return $Html.audio(A2($Basics._op["++"],
         _L.fromArray([class$(p.$class)
                      ,$Html$Attributes.autoplay(p.autoplay)
                      ,$Html$Attributes.controls(p.controls)
                      ,$Html$Attributes.loop(p.loop)]),
         filter(_L.fromArray([A2($Maybe.map,
                             $Html$Attributes.src,
                             p.src)
                             ,A2($Maybe.map,
                             $Html$Attributes.stringProperty("preload"),
                             p.preload)
                             ,A2($Maybe.map,
                             $Html$Attributes.poster,
                             p.poster)
                             ,A2($Maybe.map,
                             $Html$Attributes$Extra.volume,
                             p.volume)]))));
      }();
   };
   var table$ = function (p) {
      return $Html.table(_L.fromArray([class$(p.$class)]));
   };
   var caption$ = function (p) {
      return $Html.caption(_L.fromArray([class$(p.$class)]));
   };
   var tbody$ = function (p) {
      return $Html.tbody(_L.fromArray([class$(p.$class)]));
   };
   var thead$ = function (p) {
      return $Html.thead(_L.fromArray([class$(p.$class)]));
   };
   var tfoot$ = function (p) {
      return $Html.tfoot(_L.fromArray([class$(p.$class)]));
   };
   var tr$ = function (p) {
      return $Html.tr(_L.fromArray([class$(p.$class)]));
   };
   var td$ = function (p) {
      return $Html.td(_L.fromArray([class$(p.$class)]));
   };
   var th$ = function (p) {
      return $Html.th(_L.fromArray([class$(p.$class)]));
   };
   var form$ = function (p) {
      return function () {
         var onEnter$ = function (msg) {
            return A3($Html$Events.on,
            "keypress",
            $Json$Decode.customDecoder($Html$Events.keyCode)(function (c) {
               return _U.eq(c,
               13) ? $Result.Ok({ctor: "_Tuple0"}) : $Result.Err("expected key code 13");
            }),
            $Basics.always(msg));
         };
         var filter = $List.filterMap($Basics.identity);
         return $Html.form(A2($List._op["::"],
         class$(p.$class),
         A2($List._op["::"],
         $Html$Attributes.novalidate(p.novalidate),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return A2($Html$Events.on,
                                "submit",
                                $Json$Decode.value)($Basics.always($));
                             },
                             p.update.onSubmit)
                             ,A2($Maybe.map,
                             onEnter$,
                             p.update.onSubmit)])))));
      }();
   };
   var fieldset$ = function (p) {
      return $Html.fieldset(_L.fromArray([class$(p.$class)
                                         ,$Html$Attributes.disabled(p.disabled)]));
   };
   var legend$ = function (p) {
      return $Html.legend(_L.fromArray([class$(p.$class)]));
   };
   var label$ = function (p) {
      return $Html.label(_L.fromArray([class$(p.$class)
                                      ,$Html$Attributes.$for(p.$for)]));
   };
   var button$ = function (p) {
      return $Html.button(_L.fromArray([class$(p.$class)
                                       ,$Html$Attributes.type$("button")
                                       ,A3($Html$Events.on,
                                       "click",
                                       $Json$Decode.value,
                                       $Basics.always(p.update.onClick))]));
   };
   var buttonLink$ = function (p) {
      return $Html.a(_L.fromArray([class$(p.$class)
                                  ,$Html$Attributes.href("#")
                                  ,A3($Html$Events.on,
                                  "click",
                                  $Json$Decode.value,
                                  $Basics.always(p.update.onClick))]));
   };
   var buttonSubmit$ = function (p) {
      return $Html.button(_L.fromArray([class$(p.$class)
                                       ,$Html$Attributes.type$("submit")]));
   };
   var buttonReset$ = function (p) {
      return $Html.button(_L.fromArray([class$(p.$class)
                                       ,$Html$Attributes.type$("reset")]));
   };
   var id$ = $Html$Shorthand$Internal.id$;
   var section_ = function (i) {
      return $Html.section(_L.fromArray([id$(i)]));
   };
   var section$ = function (p) {
      return $Html.section(_L.fromArray([class$(p.$class)
                                        ,id$(p.id)]));
   };
   var article_ = function (i) {
      return $Html.article(_L.fromArray([id$(i)]));
   };
   var article$ = function (p) {
      return $Html.article(_L.fromArray([class$(p.$class)
                                        ,id$(p.id)]));
   };
   var aside$ = function (p) {
      return $Html.aside(_L.fromArray([class$(p.$class)
                                      ,id$(p.id)]));
   };
   var dt$ = function (p) {
      return $Html.dt(_L.fromArray([class$(p.$class)
                                   ,id$(p.id)]));
   };
   var figure$ = function (p) {
      return $Html.figure(_L.fromArray([class$(p.$class)
                                       ,id$(p.id)]));
   };
   var dfn$ = function (p) {
      return $Html.dfn(_L.fromArray([class$(p.$class)
                                    ,id$(p.id)]));
   };
   var embed$ = function (p) {
      return A2($Html.embed,
      _L.fromArray([class$(p.$class)
                   ,id$(p.id)
                   ,$Html$Attributes.src(p.src)
                   ,$Html$Attributes.type$(p.type$)
                   ,$Html$Attributes.width(p.width)
                   ,$Html$Attributes.height(p.height)]),
      _L.fromArray([]));
   };
   var encodeClass = $Html$Shorthand$Internal.encodeClass;
   var encodeId = $Html$Shorthand$Internal.encodeId;
   var iframe$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         var i$ = encodeId(p.name);
         return A2($Html.iframe,
         A2($Basics._op["++"],
         _L.fromArray([class$(p.$class)
                      ,$Html$Attributes.id(i$)
                      ,$Html$Attributes.name(i$)
                      ,$Html$Attributes.src(p.src)
                      ,$Html$Attributes.width(p.width)
                      ,$Html$Attributes.height(p.height)
                      ,$Html$Attributes.seamless(p.seamless)]),
         filter(_L.fromArray([A2($Maybe.map,
         $Html$Attributes.sandbox,
         p.sandbox)]))),
         _L.fromArray([]));
      }();
   };
   var object$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         var attrs = filter(_L.fromArray([A2($Maybe.map,
         function ($) {
            return $Html$Attributes.usemap($String.cons(_U.chr("#"))(encodeId($)));
         },
         p.useMapName)]));
         var i$ = encodeId(p.name);
         return $Html.object(A2($Basics._op["++"],
         _L.fromArray([class$(p.$class)
                      ,$Html$Attributes.id(i$)
                      ,$Html$Attributes.name(i$)
                      ,A2($Html$Attributes.attribute,
                      "data",
                      p.data)
                      ,$Html$Attributes.type$(p.type$)]),
         A2($Basics._op["++"],
         attrs,
         _L.fromArray([$Html$Attributes.height(p.height)
                      ,$Html$Attributes.width(p.width)]))));
      }();
   };
   var inputField$ = F2(function (p,
   attrs) {
      return function () {
         var i$ = encodeId(p.name);
         var filter = $List.filterMap($Basics.identity);
         var pattrs = A2($Basics._op["++"],
         _L.fromArray([$Html$Attributes.type$(p.type$)
                      ,$Html$Attributes.id(i$)
                      ,$Html$Attributes.name(i$)
                      ,$Html$Attributes.required(p.required)]),
         filter(_L.fromArray([A2($Maybe.map,
                             class$,
                             _U.eq(p.$class,
                             "") ? $Maybe.Nothing : $Maybe.Just(p.$class))
                             ,A2($Maybe.map,
                             function (onEvent) {
                                return A2($Html$Shorthand$Event.onInput,
                                A2($Html$Shorthand$Event.messageDecoder,
                                p.decoder,
                                onEvent),
                                $Basics.identity);
                             },
                             p.update.onInput)
                             ,A2($Maybe.map,
                             function (onEvent) {
                                return A2($Html$Shorthand$Event.onEnter,
                                A2($Html$Shorthand$Event.messageDecoder,
                                p.decoder,
                                onEvent),
                                $Basics.identity);
                             },
                             p.update.onEnter)
                             ,A2($Maybe.map,
                             function (onEvent) {
                                return A2($Html$Shorthand$Event.onKeyboardLost,
                                A2($Html$Shorthand$Event.messageDecoder,
                                p.decoder,
                                onEvent),
                                $Basics.identity);
                             },
                             p.update.onKeyboardLost)
                             ,A2($Maybe.map,
                             $Html$Attributes.placeholder,
                             p.placeholder)
                             ,A2($Maybe.map,
                             $Html$Attributes.pattern,
                             p.pattern)])));
         return A2($Html.input,
         A2($Basics._op["++"],
         pattrs,
         attrs),
         _L.fromArray([]));
      }();
   });
   var inputText$ = function (p) {
      return A2(inputField$,
      {_: {}
      ,$class: p.$class
      ,decoder: $Html$Events.targetValue
      ,name: p.name
      ,pattern: $Maybe.Nothing
      ,placeholder: p.placeholder
      ,required: p.required
      ,type$: "text"
      ,update: p.update},
      _L.fromArray([$Html$Attributes.value(p.value)
                   ,$Html$Attributes.autocomplete(p.autocomplete)]));
   };
   var inputMaybeText$ = function (p) {
      return A2(inputField$,
      {_: {}
      ,$class: p.$class
      ,decoder: $Html$Events$Extra.targetValueMaybe
      ,name: p.name
      ,pattern: $Maybe.Nothing
      ,placeholder: p.placeholder
      ,required: false
      ,type$: "text"
      ,update: p.update},
      _L.fromArray([$Html$Attributes.value(A2($Maybe.withDefault,
                   "",
                   p.value))
                   ,$Html$Attributes.autocomplete(p.autocomplete)]));
   };
   var inputFloat$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return inputField$({_: {}
                            ,$class: p.$class
                            ,decoder: function () {
                               var _v1 = {ctor: "_Tuple2"
                                         ,_0: p.min
                                         ,_1: p.max};
                               switch (_v1.ctor)
                               {case "_Tuple2":
                                  switch (_v1._0.ctor)
                                    {case "Nothing":
                                       switch (_v1._1.ctor)
                                         {case "Nothing":
                                            return $Html$Events$Extra.targetValueFloat;}
                                         break;}
                                    break;}
                               return $Json$Decode.customDecoder($Html$Events$Extra.targetValueFloat)(function (v) {
                                  return _U.cmp(v,
                                  A2($Maybe.withDefault,
                                  -1 / 0,
                                  p.min)) < 0 || _U.cmp(v,
                                  A2($Maybe.withDefault,
                                  1 / 0,
                                  p.max)) > 0 ? $Result.Err("out of bounds") : $Result.Ok(v);
                               });
                            }()
                            ,name: p.name
                            ,pattern: $Maybe.Nothing
                            ,placeholder: p.placeholder
                            ,required: true
                            ,type$: "number"
                            ,update: p.update})(A2($List._op["::"],
         $Html$Attributes$Extra.valueAsFloat(p.value),
         A2($List._op["::"],
         A2($Html$Attributes.stringProperty,
         "step",
         $Maybe.withDefault("any")(A2($Maybe.map,
         $Basics.toString,
         p.step))),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.min($Basics.toString($));
                             },
                             p.min)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.max($Basics.toString($));
                             },
                             p.max)])))));
      }();
   };
   var inputMaybeFloat$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return inputField$({_: {}
                            ,$class: p.$class
                            ,decoder: function () {
                               var _v6 = {ctor: "_Tuple2"
                                         ,_0: p.min
                                         ,_1: p.max};
                               switch (_v6.ctor)
                               {case "_Tuple2":
                                  switch (_v6._0.ctor)
                                    {case "Nothing":
                                       switch (_v6._1.ctor)
                                         {case "Nothing":
                                            return $Html$Events$Extra.targetValueMaybeFloat;}
                                         break;}
                                    break;}
                               return $Json$Decode.customDecoder($Html$Events$Extra.targetValueMaybeFloat)(function (mv) {
                                  return function () {
                                     switch (mv.ctor)
                                     {case "Just":
                                        return _U.cmp(mv._0,
                                          A2($Maybe.withDefault,
                                          -1 / 0,
                                          p.min)) < 0 || _U.cmp(mv._0,
                                          A2($Maybe.withDefault,
                                          1 / 0,
                                          p.max)) > 0 ? $Result.Err("out of bounds") : $Result.Ok(mv);
                                        case "Nothing":
                                        return $Result.Ok($Maybe.Nothing);}
                                     _U.badCase($moduleName,
                                     "between lines 1482 and 1487");
                                  }();
                               });
                            }()
                            ,name: p.name
                            ,pattern: $Maybe.Nothing
                            ,placeholder: p.placeholder
                            ,required: false
                            ,type$: "number"
                            ,update: p.update})(A2($List._op["::"],
         function () {
            var _v4 = p.value;
            switch (_v4.ctor)
            {case "Just":
               return $Html$Attributes$Extra.valueAsFloat(_v4._0);
               case "Nothing":
               return $Html$Attributes.value("");}
            _U.badCase($moduleName,
            "between lines 1488 and 1491");
         }(),
         A2($List._op["::"],
         A2($Html$Attributes.stringProperty,
         "step",
         $Maybe.withDefault("any")(A2($Maybe.map,
         $Basics.toString,
         p.step))),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.min($Basics.toString($));
                             },
                             p.min)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.max($Basics.toString($));
                             },
                             p.max)])))));
      }();
   };
   var inputInt$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return inputField$({_: {}
                            ,$class: p.$class
                            ,decoder: function () {
                               var _v11 = {ctor: "_Tuple2"
                                          ,_0: p.min
                                          ,_1: p.max};
                               switch (_v11.ctor)
                               {case "_Tuple2":
                                  switch (_v11._0.ctor)
                                    {case "Nothing":
                                       switch (_v11._1.ctor)
                                         {case "Nothing":
                                            return $Html$Events$Extra.targetValueInt;}
                                         break;}
                                    break;}
                               return $Json$Decode.customDecoder($Html$Events$Extra.targetValueInt)(function (v) {
                                  return _U.cmp(v,
                                  A2($Maybe.withDefault,
                                  $Basics.floor(-1 / 0),
                                  p.min)) < 0 || _U.cmp(v,
                                  A2($Maybe.withDefault,
                                  $Basics.ceiling(1 / 0),
                                  p.max)) > 0 ? $Result.Err("out of bounds") : $Result.Ok(v);
                               });
                            }()
                            ,name: p.name
                            ,pattern: $Maybe.Nothing
                            ,placeholder: p.placeholder
                            ,required: true
                            ,type$: "number"
                            ,update: p.update})(A2($List._op["::"],
         $Html$Attributes$Extra.valueAsInt(p.value),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.min($Basics.toString($));
                             },
                             p.min)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.max($Basics.toString($));
                             },
                             p.max)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.stringProperty("step")($Basics.toString($));
                             },
                             p.step)]))));
      }();
   };
   var inputMaybeInt$ = function (p) {
      return function () {
         var filter = $List.filterMap($Basics.identity);
         return inputField$({_: {}
                            ,$class: p.$class
                            ,decoder: function () {
                               var _v16 = {ctor: "_Tuple2"
                                          ,_0: p.min
                                          ,_1: p.max};
                               switch (_v16.ctor)
                               {case "_Tuple2":
                                  switch (_v16._0.ctor)
                                    {case "Nothing":
                                       switch (_v16._1.ctor)
                                         {case "Nothing":
                                            return $Html$Events$Extra.targetValueMaybeInt;}
                                         break;}
                                    break;}
                               return $Json$Decode.customDecoder($Html$Events$Extra.targetValueMaybeInt)(function (mv) {
                                  return function () {
                                     switch (mv.ctor)
                                     {case "Just":
                                        return _U.cmp(mv._0,
                                          A2($Maybe.withDefault,
                                          $Basics.floor(-1 / 0),
                                          p.min)) < 0 || _U.cmp(mv._0,
                                          A2($Maybe.withDefault,
                                          $Basics.ceiling(1 / 0),
                                          p.max)) > 0 ? $Result.Err("out of bounds") : $Result.Ok(mv);
                                        case "Nothing":
                                        return $Result.Ok($Maybe.Nothing);}
                                     _U.badCase($moduleName,
                                     "between lines 1541 and 1546");
                                  }();
                               });
                            }()
                            ,name: p.name
                            ,pattern: $Maybe.Nothing
                            ,placeholder: p.placeholder
                            ,required: false
                            ,type$: "number"
                            ,update: p.update})(A2($List._op["::"],
         function () {
            var _v14 = p.value;
            switch (_v14.ctor)
            {case "Just":
               return $Html$Attributes$Extra.valueAsInt(_v14._0);
               case "Nothing":
               return $Html$Attributes.value("");}
            _U.badCase($moduleName,
            "between lines 1547 and 1550");
         }(),
         filter(_L.fromArray([A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.min($Basics.toString($));
                             },
                             p.min)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.max($Basics.toString($));
                             },
                             p.max)
                             ,A2($Maybe.map,
                             function ($) {
                                return $Html$Attributes.stringProperty("step")($Basics.toString($));
                             },
                             p.step)]))));
      }();
   };
   var inputUrl$ = function (p) {
      return A2(inputField$,
      {_: {}
      ,$class: p.$class
      ,decoder: $Html$Events.targetValue
      ,name: p.name
      ,pattern: $Maybe.Nothing
      ,placeholder: p.placeholder
      ,required: p.required
      ,type$: "url"
      ,update: p.update},
      _L.fromArray([$Html$Attributes.value(p.value)
                   ,$Html$Attributes.autocomplete(p.autocomplete)]));
   };
   var inputMaybeUrl$ = function (p) {
      return A2(inputField$,
      {_: {}
      ,$class: p.$class
      ,decoder: $Html$Events$Extra.targetValueMaybe
      ,name: p.name
      ,pattern: $Maybe.Nothing
      ,placeholder: p.placeholder
      ,required: false
      ,type$: "url"
      ,update: p.update},
      _L.fromArray([$Html$Attributes.value(A2($Maybe.withDefault,
                   "",
                   p.value))
                   ,$Html$Attributes.autocomplete(p.autocomplete)]));
   };
   var select$ = function (p) {
      return function () {
         var i$ = encodeId(p.name);
         return $Html.select(_L.fromArray([class$(p.$class)
                                          ,$Html$Attributes.id(i$)
                                          ,$Html$Attributes.name(i$)
                                          ,A2($Html$Shorthand$Event.onChange,
                                          $Html$Events.targetValue,
                                          p.update.onSelect)]));
      }();
   };
   var output$ = function (p) {
      return function () {
         var i$ = encodeId(p.name);
         return $Html.output(_L.fromArray([class$(p.$class)
                                          ,$Html$Attributes.id(i$)
                                          ,$Html$Attributes.name(i$)
                                          ,$Html$Attributes.$for($String.join(" ")(A2($List.map,
                                          encodeId,
                                          p.$for)))]));
      }();
   };
   var fieldUpdateFallbackFocusLost = function (handler) {
      return function () {
         var doErr = function (r) {
            return function () {
               switch (r.ctor)
               {case "Err":
                  return function () {
                       var _v24 = A2($Json$Decode.decodeValue,
                       $Html$Events.targetValue,
                       r._0.event);
                       switch (_v24.ctor)
                       {case "Err":
                          return $Maybe.Nothing;
                          case "Ok":
                          return $Maybe.Just(handler.onFallback(_v24._0));}
                       _U.badCase($moduleName,
                       "between lines 256 and 259");
                    }();
                  case "Ok":
                  return $Maybe.Nothing;}
               _U.badCase($moduleName,
               "between lines 253 and 259");
            }();
         };
         var doOk = function (r) {
            return function () {
               switch (r.ctor)
               {case "Err":
                  return $Maybe.Nothing;
                  case "Ok":
                  return $Maybe.Just(handler.onInput(r._0));}
               _U.badCase($moduleName,
               "between lines 250 and 253");
            }();
         };
         return {_: {}
                ,onEnter: $Maybe.Just(doErr)
                ,onInput: $Maybe.Just(doOk)
                ,onKeyboardLost: $Maybe.Just(doErr)};
      }();
   };
   var fieldUpdate = {_: {}
                     ,onEnter: $Maybe.Nothing
                     ,onInput: $Maybe.Nothing
                     ,onKeyboardLost: $Maybe.Nothing};
   var fieldUpdateContinuous = function (handler) {
      return function () {
         var doOk = function (r) {
            return function () {
               switch (r.ctor)
               {case "Err":
                  return $Maybe.Nothing;
                  case "Ok":
                  return $Maybe.Just(handler.onInput(r._0));}
               _U.badCase($moduleName,
               "between lines 198 and 201");
            }();
         };
         return _U.replace([["onInput"
                            ,$Maybe.Just(doOk)]],
         fieldUpdate);
      }();
   };
   var fieldUpdateFocusLost = function (handler) {
      return function () {
         var doOk = function (r) {
            return function () {
               switch (r.ctor)
               {case "Err":
                  return $Maybe.Nothing;
                  case "Ok":
                  return $Maybe.Just(handler.onInput(r._0));}
               _U.badCase($moduleName,
               "between lines 211 and 214");
            }();
         };
         return _U.replace([["onEnter"
                            ,$Maybe.Just(doOk)]
                           ,["onKeyboardLost"
                            ,$Maybe.Just(doOk)]],
         fieldUpdate);
      }();
   };
   var fieldUpdateFallbackContinuous = function (handler) {
      return function () {
         var doOkErr = function (r) {
            return function () {
               switch (r.ctor)
               {case "Err":
                  return function () {
                       var _v39 = A2($Json$Decode.decodeValue,
                       $Html$Events.targetValue,
                       r._0.event);
                       switch (_v39.ctor)
                       {case "Err":
                          return $Maybe.Nothing;
                          case "Ok":
                          return $Maybe.Just(handler.onFallback(_v39._0));}
                       _U.badCase($moduleName,
                       "between lines 289 and 292");
                    }();
                  case "Ok":
                  return $Maybe.Just(handler.onInput(r._0));}
               _U.badCase($moduleName,
               "between lines 286 and 292");
            }();
         };
         return _U.replace([["onInput"
                            ,$Maybe.Just(doOkErr)]],
         fieldUpdate);
      }();
   };
   var AutoDirection = {ctor: "AutoDirection"};
   var RightToLeft = {ctor: "RightToLeft"};
   var LeftToRight = {ctor: "LeftToRight"};
   _elm.Html.Shorthand.values = {_op: _op
                                ,LeftToRight: LeftToRight
                                ,RightToLeft: RightToLeft
                                ,AutoDirection: AutoDirection
                                ,fieldUpdate: fieldUpdate
                                ,fieldUpdateContinuous: fieldUpdateContinuous
                                ,fieldUpdateFocusLost: fieldUpdateFocusLost
                                ,fieldUpdateFallbackFocusLost: fieldUpdateFallbackFocusLost
                                ,fieldUpdateFallbackContinuous: fieldUpdateFallbackContinuous
                                ,encodeId: encodeId
                                ,encodeClass: encodeClass
                                ,id$: id$
                                ,class$: class$
                                ,body_: body_
                                ,body$: body$
                                ,section_: section_
                                ,section$: section$
                                ,nav_: nav_
                                ,nav$: nav$
                                ,article_: article_
                                ,article$: article$
                                ,aside$: aside$
                                ,h1_: h1_
                                ,h1$: h1$
                                ,h2_: h2_
                                ,h2$: h2$
                                ,h3_: h3_
                                ,h3$: h3$
                                ,h4_: h4_
                                ,h4$: h4$
                                ,h5_: h5_
                                ,h5$: h5$
                                ,h6_: h6_
                                ,h6$: h6$
                                ,header_: header_
                                ,header$: header$
                                ,footer_: footer_
                                ,footer$: footer$
                                ,address_: address_
                                ,address$: address$
                                ,main_: main_
                                ,p_: p_
                                ,p$: p$
                                ,hr_: hr_
                                ,pre_: pre_
                                ,pre$: pre$
                                ,blockquote_: blockquote_
                                ,blockquote$: blockquote$
                                ,ol_: ol_
                                ,ol$: ol$
                                ,ul_: ul_
                                ,ul$: ul$
                                ,li_: li_
                                ,li$: li$
                                ,dl_: dl_
                                ,dl$: dl$
                                ,dt$: dt$
                                ,dd_: dd_
                                ,dd$: dd$
                                ,figure$: figure$
                                ,figcaption_: figcaption_
                                ,figcaption$: figcaption$
                                ,div_: div_
                                ,div$: div$
                                ,a_: a_
                                ,a$: a$
                                ,em_: em_
                                ,em$: em$
                                ,strong_: strong_
                                ,strong$: strong$
                                ,small_: small_
                                ,small$: small$
                                ,s_: s_
                                ,s$: s$
                                ,cite_: cite_
                                ,cite$: cite$
                                ,q_: q_
                                ,q$: q$
                                ,dfn$: dfn$
                                ,abbr_: abbr_
                                ,abbr$: abbr$
                                ,code_: code_
                                ,code$: code$
                                ,var_: var_
                                ,var$: var$
                                ,samp_: samp_
                                ,samp$: samp$
                                ,kbd_: kbd_
                                ,kbd$: kbd$
                                ,sub_: sub_
                                ,sub$: sub$
                                ,sup_: sup_
                                ,sup$: sup$
                                ,i_: i_
                                ,i$: i$
                                ,b_: b_
                                ,b$: b$
                                ,u_: u_
                                ,u$: u$
                                ,mark_: mark_
                                ,mark$: mark$
                                ,ruby_: ruby_
                                ,ruby$: ruby$
                                ,rt_: rt_
                                ,rt$: rt$
                                ,rp_: rp_
                                ,rp$: rp$
                                ,bdi_: bdi_
                                ,bdi$: bdi$
                                ,bdo$: bdo$
                                ,span_: span_
                                ,span$: span$
                                ,br$: br$
                                ,wbr$: wbr$
                                ,ins_: ins_
                                ,ins$: ins$
                                ,del_: del_
                                ,del$: del$
                                ,img$: img$
                                ,img_: img_
                                ,iframe$: iframe$
                                ,embed$: embed$
                                ,object$: object$
                                ,param$: param$
                                ,video_: video_
                                ,video$: video$
                                ,audio_: audio_
                                ,audio$: audio$
                                ,table_: table_
                                ,table$: table$
                                ,caption_: caption_
                                ,caption$: caption$
                                ,tbody_: tbody_
                                ,tbody$: tbody$
                                ,thead_: thead_
                                ,thead$: thead$
                                ,tfoot_: tfoot_
                                ,tfoot$: tfoot$
                                ,tr_: tr_
                                ,tr$: tr$
                                ,td_: td_
                                ,td$: td$
                                ,th_: th_
                                ,th$: th$
                                ,form$: form$
                                ,fieldset_: fieldset_
                                ,fieldset$: fieldset$
                                ,legend_: legend_
                                ,legend$: legend$
                                ,label_: label_
                                ,label$: label$
                                ,inputField$: inputField$
                                ,inputText$: inputText$
                                ,inputMaybeText$: inputMaybeText$
                                ,inputFloat$: inputFloat$
                                ,inputMaybeFloat$: inputMaybeFloat$
                                ,inputInt$: inputInt$
                                ,inputMaybeInt$: inputMaybeInt$
                                ,inputUrl$: inputUrl$
                                ,inputMaybeUrl$: inputMaybeUrl$
                                ,button_: button_
                                ,button$: button$
                                ,buttonLink_: buttonLink_
                                ,buttonLink$: buttonLink$
                                ,buttonSubmit_: buttonSubmit_
                                ,buttonSubmit$: buttonSubmit$
                                ,buttonReset_: buttonReset_
                                ,buttonReset$: buttonReset$
                                ,select$: select$
                                ,option_: option_
                                ,option$: option$
                                ,output$: output$
                                ,progress$: progress$
                                ,meter$: meter$};
   return _elm.Html.Shorthand.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Shorthand = Elm.Html.Shorthand || {};
Elm.Html.Shorthand.Event = Elm.Html.Shorthand.Event || {};
Elm.Html.Shorthand.Event.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Shorthand = _elm.Html.Shorthand || {};
   _elm.Html.Shorthand.Event = _elm.Html.Shorthand.Event || {};
   if (_elm.Html.Shorthand.Event.values)
   return _elm.Html.Shorthand.Event.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Shorthand.Event",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Html$Shorthand$Type = Elm.Html.Shorthand.Type.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var messageDecoder = F2(function (dec,
   f) {
      return $Json$Decode.customDecoder($Json$Decode.value)(function (event) {
         return function () {
            var r = A2($Json$Decode.decodeValue,
            dec,
            event);
            var r$ = A2($Result.formatError,
            $Html$Shorthand$Type.EventDecodeError(event),
            r);
            return function () {
               var _v0 = {ctor: "_Tuple2"
                         ,_0: f(r$)
                         ,_1: r};
               switch (_v0.ctor)
               {case "_Tuple2":
                  switch (_v0._0.ctor)
                    {case "Just":
                       return $Result.Ok(_v0._0._0);
                       case "Nothing":
                       switch (_v0._1.ctor)
                         {case "Err":
                            return $Result.Err(_v0._1._0);
                            case "Ok":
                            return $Result.Err("no message in response to event");}
                         break;}
                    break;}
               _U.badCase($moduleName,
               "between lines 104 and 107");
            }();
         }();
      });
   });
   var onMouseLost = $Html$Events.on("mouseleave");
   var onKeyboardLost = $Html$Events.on("blur");
   var onEnter = F2(function (dec,
   f) {
      return A3($Html$Events.on,
      "keydown",
      $Json$Decode.customDecoder(A3($Json$Decode.object2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      $Html$Events.keyCode,
      dec))(function (_v6) {
         return function () {
            switch (_v6.ctor)
            {case "_Tuple2":
               return _U.eq(_v6._0,
                 13) ? $Result.Ok(_v6._1) : $Result.Err("expected key code 13");}
            _U.badCase($moduleName,
            "on line 37, column 72 to 126");
         }();
      }),
      f);
   });
   var onChange = $Html$Events.on("change");
   var onInput = $Html$Events.on("input");
   _elm.Html.Shorthand.Event.values = {_op: _op
                                      ,onInput: onInput
                                      ,onChange: onChange
                                      ,onEnter: onEnter
                                      ,onKeyboardLost: onKeyboardLost
                                      ,onMouseLost: onMouseLost
                                      ,messageDecoder: messageDecoder};
   return _elm.Html.Shorthand.Event.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Shorthand = Elm.Html.Shorthand || {};
Elm.Html.Shorthand.Internal = Elm.Html.Shorthand.Internal || {};
Elm.Html.Shorthand.Internal.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Shorthand = _elm.Html.Shorthand || {};
   _elm.Html.Shorthand.Internal = _elm.Html.Shorthand.Internal || {};
   if (_elm.Html.Shorthand.Internal.values)
   return _elm.Html.Shorthand.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Shorthand.Internal",
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Shorthand$Type = Elm.Html.Shorthand.Type.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $String = Elm.String.make(_elm);
   var encodeClass = function () {
      var isAlpha = function (c) {
         return function () {
            var cc = $Char.toCode($Char.toLower(c));
            return _U.cmp(cc,
            $Char.toCode(_U.chr("a"))) > -1 && _U.cmp(cc,
            $Char.toCode(_U.chr("z"))) < 1;
         }();
      };
      var startWithAlpha = function (s) {
         return function () {
            var _v0 = $String.uncons(s);
            switch (_v0.ctor)
            {case "Just":
               switch (_v0._0.ctor)
                 {case "_Tuple2":
                    return $Basics.not(isAlpha(_v0._0._0)) ? A2($String.cons,
                      _U.chr("x"),
                      s) : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 53 and 56");
         }();
      };
      var hu = _L.fromArray([_U.chr("-")
                            ,_U.chr("_")]);
      var isClassChar = function (c) {
         return $Char.isDigit(c) || (isAlpha(c) || A2($List.member,
         c,
         hu));
      };
      var smartTrimLeft = function (s) {
         return function () {
            var _v4 = $String.uncons(s);
            switch (_v4.ctor)
            {case "Just":
               switch (_v4._0.ctor)
                 {case "_Tuple2":
                    return A2($List.member,
                      _v4._0._0,
                      hu) ? _v4._0._1 : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 56 and 59");
         }();
      };
      var smartTrimRight = function (s) {
         return function () {
            var _v8 = $String.uncons($String.reverse(s));
            switch (_v8.ctor)
            {case "Just":
               switch (_v8._0.ctor)
                 {case "_Tuple2":
                    return A2($List.member,
                      _v8._0._0,
                      hu) ? $String.reverse(_v8._0._1) : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 59 and 62");
         }();
      };
      var smartTrim = function ($) {
         return smartTrimRight(smartTrimLeft($));
      };
      return function ($) {
         return $String.join(" ")($List.map(function ($) {
            return startWithAlpha(smartTrim($String.filter(isClassChar)($String.toLower($))));
         })($String.words($)));
      };
   }();
   var class$ = function ($) {
      return $Html$Attributes.$class(encodeClass($));
   };
   var encodeId = function () {
      var isAlpha = function (c) {
         return function () {
            var cc = $Char.toCode($Char.toLower(c));
            return _U.cmp(cc,
            $Char.toCode(_U.chr("a"))) > -1 && _U.cmp(cc,
            $Char.toCode(_U.chr("z"))) < 1;
         }();
      };
      var startWithAlpha = function (s) {
         return function () {
            var _v12 = $String.uncons(s);
            switch (_v12.ctor)
            {case "Just":
               switch (_v12._0.ctor)
                 {case "_Tuple2":
                    return $Basics.not(isAlpha(_v12._0._0)) ? A2($String.cons,
                      _U.chr("x"),
                      s) : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 24 and 27");
         }();
      };
      var hu = _L.fromArray([_U.chr("-")
                            ,_U.chr("_")]);
      var isIdChar = function (c) {
         return $Char.isDigit(c) || (isAlpha(c) || A2($List.member,
         c,
         hu));
      };
      var smartTrimLeft = function (s) {
         return function () {
            var _v16 = $String.uncons(s);
            switch (_v16.ctor)
            {case "Just":
               switch (_v16._0.ctor)
                 {case "_Tuple2":
                    return A2($List.member,
                      _v16._0._0,
                      hu) ? _v16._0._1 : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 27 and 30");
         }();
      };
      var smartTrimRight = function (s) {
         return function () {
            var _v20 = $String.uncons($String.reverse(s));
            switch (_v20.ctor)
            {case "Just":
               switch (_v20._0.ctor)
                 {case "_Tuple2":
                    return A2($List.member,
                      _v20._0._0,
                      hu) ? $String.reverse(_v20._0._1) : s;}
                 break;
               case "Nothing": return s;}
            _U.badCase($moduleName,
            "between lines 30 and 33");
         }();
      };
      var smartTrim = function ($) {
         return smartTrimRight(smartTrimLeft($));
      };
      return function ($) {
         return startWithAlpha($String.join("-")($List.map(function ($) {
            return smartTrim($String.filter(isIdChar)($String.toLower($)));
         })($String.words($))));
      };
   }();
   var id$ = function ($) {
      return $Html$Attributes.id(encodeId($));
   };
   _elm.Html.Shorthand.Internal.values = {_op: _op
                                         ,encodeId: encodeId
                                         ,encodeClass: encodeClass
                                         ,id$: id$
                                         ,class$: class$};
   return _elm.Html.Shorthand.Internal.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Shorthand = Elm.Html.Shorthand || {};
Elm.Html.Shorthand.Type = Elm.Html.Shorthand.Type || {};
Elm.Html.Shorthand.Type.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Shorthand = _elm.Html.Shorthand || {};
   _elm.Html.Shorthand.Type = _elm.Html.Shorthand.Type || {};
   if (_elm.Html.Shorthand.Type.values)
   return _elm.Html.Shorthand.Type.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Shorthand.Type",
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var MeterParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,high: f
             ,low: e
             ,max: d
             ,min: c
             ,optimum: g
             ,value: b};
   });
   var ProgressParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,$class: a
             ,max: c
             ,value: b};
   });
   var OutputParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,$class: a
             ,$for: c
             ,name: b};
   });
   var OptionParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,label: a
             ,selected: c
             ,value: b};
   });
   var SelectParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,$class: a
             ,name: b
             ,update: c};
   });
   var ButtonParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,update: b};
   });
   var InputMaybeIntParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,max: f
             ,min: e
             ,name: b
             ,placeholder: c
             ,step: g
             ,update: h
             ,value: d};
   });
   var InputIntParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,max: f
             ,min: e
             ,name: b
             ,placeholder: c
             ,step: g
             ,update: h
             ,value: d};
   });
   var InputMaybeFloatParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,max: f
             ,min: e
             ,name: b
             ,placeholder: c
             ,step: g
             ,update: h
             ,value: d};
   });
   var InputFloatParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,max: f
             ,min: e
             ,name: b
             ,placeholder: c
             ,step: g
             ,update: h
             ,value: d};
   });
   var InputMaybeUrlParam = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,$class: a
             ,autocomplete: e
             ,name: b
             ,placeholder: c
             ,update: f
             ,value: d};
   });
   var InputUrlParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,autocomplete: f
             ,name: b
             ,placeholder: c
             ,required: e
             ,update: g
             ,value: d};
   });
   var InputMaybeTextParam = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,$class: a
             ,autocomplete: e
             ,name: b
             ,placeholder: c
             ,update: f
             ,value: d};
   });
   var InputTextParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,autocomplete: f
             ,name: b
             ,placeholder: c
             ,required: e
             ,update: g
             ,value: d};
   });
   var InputFieldParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,decoder: h
             ,name: b
             ,pattern: f
             ,placeholder: c
             ,required: g
             ,type$: e
             ,update: d};
   });
   var LabelParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,$for: b};
   });
   var FieldsetParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,disabled: b};
   });
   var FormParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,$class: a
             ,novalidate: b
             ,update: c};
   });
   var VideoParam = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return {_: {}
                                              ,$class: a
                                              ,autoplay: g
                                              ,controls: h
                                              ,height: d
                                              ,loop: i
                                              ,poster: k
                                              ,preload: j
                                              ,src: b
                                              ,videoHeight: e
                                              ,videoWidth: f
                                              ,volume: l
                                              ,width: c};
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var MediaParam = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,$class: a
             ,autoplay: c
             ,controls: d
             ,loop: e
             ,poster: g
             ,preload: f
             ,src: b
             ,volume: h};
   });
   var ObjectParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,data: c
             ,height: f
             ,name: b
             ,type$: d
             ,useMapName: e
             ,width: g};
   });
   var EmbedParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,height: f
             ,id: b
             ,src: c
             ,type$: d
             ,useMapName: e
             ,width: g};
   });
   var IframeParam = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,$class: a
             ,height: e
             ,name: b
             ,sandbox: f
             ,seamless: g
             ,src: c
             ,width: d};
   });
   var ImgParam = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,$class: a
             ,alt: e
             ,height: d
             ,src: b
             ,width: c};
   });
   var ModParam = F3(function (a,
   b,
   c) {
      return {_: {}
             ,$class: a
             ,cite: b
             ,datetime: c};
   });
   var AnchorParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,href: b};
   });
   var ClassCiteParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,cite: b};
   });
   var ClassIdParam = F2(function (a,
   b) {
      return {_: {}
             ,$class: a
             ,id: b};
   });
   var ClassParam = function (a) {
      return {_: {},$class: a};
   };
   var SelectUpdate = function (a) {
      return {_: {},onSelect: a};
   };
   var ButtonUpdate = function (a) {
      return {_: {},onClick: a};
   };
   var FieldUpdate = F3(function (a,
   b,
   c) {
      return {_: {}
             ,onEnter: b
             ,onInput: a
             ,onKeyboardLost: c};
   });
   var FormUpdate = F2(function (a,
   b) {
      return {_: {}
             ,onEnter: b
             ,onSubmit: a};
   });
   var EventDecodeError = F2(function (a,
   b) {
      return {_: {}
             ,event: a
             ,reason: b};
   });
   _elm.Html.Shorthand.Type.values = {_op: _op
                                     ,EventDecodeError: EventDecodeError
                                     ,FormUpdate: FormUpdate
                                     ,FieldUpdate: FieldUpdate
                                     ,ButtonUpdate: ButtonUpdate
                                     ,SelectUpdate: SelectUpdate
                                     ,ClassParam: ClassParam
                                     ,ClassIdParam: ClassIdParam
                                     ,ClassCiteParam: ClassCiteParam
                                     ,AnchorParam: AnchorParam
                                     ,ModParam: ModParam
                                     ,ImgParam: ImgParam
                                     ,IframeParam: IframeParam
                                     ,EmbedParam: EmbedParam
                                     ,ObjectParam: ObjectParam
                                     ,MediaParam: MediaParam
                                     ,VideoParam: VideoParam
                                     ,FormParam: FormParam
                                     ,FieldsetParam: FieldsetParam
                                     ,LabelParam: LabelParam
                                     ,InputFieldParam: InputFieldParam
                                     ,InputTextParam: InputTextParam
                                     ,InputMaybeTextParam: InputMaybeTextParam
                                     ,InputUrlParam: InputUrlParam
                                     ,InputMaybeUrlParam: InputMaybeUrlParam
                                     ,InputFloatParam: InputFloatParam
                                     ,InputMaybeFloatParam: InputMaybeFloatParam
                                     ,InputIntParam: InputIntParam
                                     ,InputMaybeIntParam: InputMaybeIntParam
                                     ,ButtonParam: ButtonParam
                                     ,SelectParam: SelectParam
                                     ,OptionParam: OptionParam
                                     ,OutputParam: OutputParam
                                     ,ProgressParam: ProgressParam
                                     ,MeterParam: MeterParam};
   return _elm.Html.Shorthand.Type.values;
};
Elm.Http = Elm.Http || {};
Elm.Http.make = function (_elm) {
   "use strict";
   _elm.Http = _elm.Http || {};
   if (_elm.Http.values)
   return _elm.Http.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Http",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Http = Elm.Native.Http.make(_elm),
   $Result = Elm.Result.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var send = $Native$Http.send;
   var BadResponse = F2(function (a,
   b) {
      return {ctor: "BadResponse"
             ,_0: a
             ,_1: b};
   });
   var UnexpectedPayload = function (a) {
      return {ctor: "UnexpectedPayload"
             ,_0: a};
   };
   var handleResponse = F2(function (handle,
   response) {
      return function () {
         var _v0 = _U.cmp(200,
         response.status) < 1 && _U.cmp(response.status,
         300) < 0;
         switch (_v0)
         {case false:
            return $Task.fail(A2(BadResponse,
              response.status,
              response.statusText));
            case true: return function () {
                 var _v1 = response.value;
                 switch (_v1.ctor)
                 {case "Text":
                    return handle(_v1._0);}
                 return $Task.fail(UnexpectedPayload("Response body is a blob, expecting a string."));
              }();}
         _U.badCase($moduleName,
         "between lines 419 and 426");
      }();
   });
   var NetworkError = {ctor: "NetworkError"};
   var Timeout = {ctor: "Timeout"};
   var promoteError = function (rawError) {
      return function () {
         switch (rawError.ctor)
         {case "RawNetworkError":
            return NetworkError;
            case "RawTimeout":
            return Timeout;}
         _U.badCase($moduleName,
         "between lines 431 and 433");
      }();
   };
   var fromJson = F2(function (decoder,
   response) {
      return function () {
         var decode = function (str) {
            return function () {
               var _v4 = A2($Json$Decode.decodeString,
               decoder,
               str);
               switch (_v4.ctor)
               {case "Err":
                  return $Task.fail(UnexpectedPayload(_v4._0));
                  case "Ok":
                  return $Task.succeed(_v4._0);}
               _U.badCase($moduleName,
               "between lines 409 and 412");
            }();
         };
         return A2($Task.andThen,
         A2($Task.mapError,
         promoteError,
         response),
         handleResponse(decode));
      }();
   });
   var RawNetworkError = {ctor: "RawNetworkError"};
   var RawTimeout = {ctor: "RawTimeout"};
   var Blob = function (a) {
      return {ctor: "Blob",_0: a};
   };
   var Text = function (a) {
      return {ctor: "Text",_0: a};
   };
   var Response = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,headers: c
             ,status: a
             ,statusText: b
             ,url: d
             ,value: e};
   });
   var defaultSettings = {_: {}
                         ,desiredResponseType: $Maybe.Nothing
                         ,onProgress: $Maybe.Nothing
                         ,onStart: $Maybe.Nothing
                         ,timeout: 0};
   var post = F3(function (decoder,
   url,
   body) {
      return function () {
         var request = {_: {}
                       ,body: body
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "POST"};
         return A2(fromJson,
         decoder,
         A2(send,
         defaultSettings,
         request));
      }();
   });
   var Settings = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,desiredResponseType: d
             ,onProgress: c
             ,onStart: b
             ,timeout: a};
   });
   var multipart = $Native$Http.multipart;
   var FileData = F3(function (a,
   b,
   c) {
      return {ctor: "FileData"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var BlobData = F3(function (a,
   b,
   c) {
      return {ctor: "BlobData"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var blobData = BlobData;
   var StringData = F2(function (a,
   b) {
      return {ctor: "StringData"
             ,_0: a
             ,_1: b};
   });
   var stringData = StringData;
   var BodyBlob = function (a) {
      return {ctor: "BodyBlob"
             ,_0: a};
   };
   var BodyFormData = {ctor: "BodyFormData"};
   var ArrayBuffer = {ctor: "ArrayBuffer"};
   var BodyString = function (a) {
      return {ctor: "BodyString"
             ,_0: a};
   };
   var string = BodyString;
   var Empty = {ctor: "Empty"};
   var empty = Empty;
   var getString = function (url) {
      return function () {
         var request = {_: {}
                       ,body: empty
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "GET"};
         return A2($Task.andThen,
         A2($Task.mapError,
         promoteError,
         A2(send,
         defaultSettings,
         request)),
         handleResponse($Task.succeed));
      }();
   };
   var get = F2(function (decoder,
   url) {
      return function () {
         var request = {_: {}
                       ,body: empty
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "GET"};
         return A2(fromJson,
         decoder,
         A2(send,
         defaultSettings,
         request));
      }();
   });
   var Request = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,body: d
             ,headers: b
             ,url: c
             ,verb: a};
   });
   var uriDecode = $Native$Http.uriDecode;
   var uriEncode = $Native$Http.uriEncode;
   var queryEscape = function (string) {
      return A2($String.join,
      "+",
      A2($String.split,
      "%20",
      uriEncode(string)));
   };
   var queryPair = function (_v7) {
      return function () {
         switch (_v7.ctor)
         {case "_Tuple2":
            return A2($Basics._op["++"],
              queryEscape(_v7._0),
              A2($Basics._op["++"],
              "=",
              queryEscape(_v7._1)));}
         _U.badCase($moduleName,
         "on line 63, column 3 to 46");
      }();
   };
   var url = F2(function (domain,
   args) {
      return function () {
         switch (args.ctor)
         {case "[]": return domain;}
         return A2($Basics._op["++"],
         domain,
         A2($Basics._op["++"],
         "?",
         A2($String.join,
         "&",
         A2($List.map,queryPair,args))));
      }();
   });
   var TODO_implement_file_in_another_library = {ctor: "TODO_implement_file_in_another_library"};
   var TODO_implement_blob_in_another_library = {ctor: "TODO_implement_blob_in_another_library"};
   _elm.Http.values = {_op: _op
                      ,getString: getString
                      ,get: get
                      ,post: post
                      ,send: send
                      ,url: url
                      ,uriEncode: uriEncode
                      ,uriDecode: uriDecode
                      ,empty: empty
                      ,string: string
                      ,multipart: multipart
                      ,stringData: stringData
                      ,blobData: blobData
                      ,defaultSettings: defaultSettings
                      ,fromJson: fromJson
                      ,Request: Request
                      ,Settings: Settings
                      ,Response: Response
                      ,Text: Text
                      ,Blob: Blob
                      ,Timeout: Timeout
                      ,NetworkError: NetworkError
                      ,UnexpectedPayload: UnexpectedPayload
                      ,BadResponse: BadResponse
                      ,RawTimeout: RawTimeout
                      ,RawNetworkError: RawNetworkError};
   return _elm.Http.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values)
   return _elm.Json.Decode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Decode",
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,
   decoder) {
      return A3($List.foldr,
      F2(function (x,y) {
         return A2(_op[":="],x,y);
      }),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,
      $Dict.fromList,
      keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   _elm.Json.Decode.values = {_op: _op
                             ,Decoder: Decoder
                             ,map: map
                             ,decodeString: decodeString
                             ,at: at
                             ,object1: object1
                             ,object2: object2
                             ,object3: object3
                             ,object4: object4
                             ,object5: object5
                             ,object6: object6
                             ,object7: object7
                             ,object8: object8
                             ,keyValuePairs: keyValuePairs
                             ,dict: dict
                             ,oneOf: oneOf
                             ,string: string
                             ,$float: $float
                             ,$int: $int
                             ,bool: bool
                             ,list: list
                             ,array: array
                             ,$null: $null
                             ,maybe: maybe
                             ,value: value
                             ,decodeValue: decodeValue
                             ,customDecoder: customDecoder
                             ,andThen: andThen
                             ,fail: fail
                             ,succeed: succeed
                             ,tuple1: tuple1
                             ,tuple2: tuple2
                             ,tuple3: tuple3
                             ,tuple4: tuple4
                             ,tuple5: tuple5
                             ,tuple6: tuple6
                             ,tuple7: tuple7
                             ,tuple8: tuple8};
   return _elm.Json.Decode.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values)
   return _elm.Json.Encode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Encode",
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   _elm.Json.Encode.values = {_op: _op
                             ,Value: Value
                             ,encode: encode
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,object: object
                             ,array: array
                             ,list: list};
   return _elm.Json.Encode.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,
      $Basics.identity,
      xs);
   };
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = $Native$List.any;
   var all = F2(function (pred,
   xs) {
      return $Basics.not(A2(any,
      function ($) {
         return $Basics.not(pred($));
      },
      xs));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_v0,i) {
         return function () {
            return i + 1;
         }();
      }),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var maximum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.max,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var minimum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.min,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var member = F2(function (x,
   xs) {
      return A2(any,
      function (a) {
         return _U.eq(a,x);
      },
      xs);
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._1);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var head = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 75 and 84");
      }();
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {
         return A2(_op["::"],
         f(x),
         acc);
      }),
      _L.fromArray([]),
      xs);
   });
   var filter = F2(function (pred,
   xs) {
      return function () {
         var conditionalCons = F2(function (x,
         xs$) {
            return pred(x) ? A2(_op["::"],
            x,
            xs$) : xs$;
         });
         return A3(foldr,
         conditionalCons,
         _L.fromArray([]),
         xs);
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v15 = f(mx);
         switch (_v15.ctor)
         {case "Just":
            return A2(_op["::"],_v15._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 179 and 186");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {
         return A2(_op["::"],x,y);
      }),
      _L.fromArray([]),
      list);
   };
   var scanl = F3(function (f,
   b,
   xs) {
      return function () {
         var scan1 = F2(function (x,
         accAcc) {
            return function () {
               switch (accAcc.ctor)
               {case "::": return A2(_op["::"],
                    A2(f,x,accAcc._0),
                    accAcc);
                  case "[]":
                  return _L.fromArray([]);}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         });
         return reverse(A3(foldl,
         scan1,
         _L.fromArray([b]),
         xs));
      }();
   });
   var append = F2(function (xs,
   ys) {
      return function () {
         switch (ys.ctor)
         {case "[]": return xs;}
         return A3(foldr,
         F2(function (x,y) {
            return A2(_op["::"],x,y);
         }),
         ys,
         xs);
      }();
   });
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v21._0)
                                   ,_1: _v21._1} : {ctor: "_Tuple2"
                                                   ,_0: _v21._0
                                                   ,_1: A2(_op["::"],
                                                   x,
                                                   _v21._1)};}
               _U.badCase($moduleName,
               "between lines 301 and 303");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v25,
         _v26) {
            return function () {
               switch (_v26.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v25.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],
                                 _v25._0,
                                 _v26._0)
                                 ,_1: A2(_op["::"],
                                 _v25._1,
                                 _v26._1)};}
                       _U.badCase($moduleName,
                       "on line 339, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 339, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 350 and 361");
      }();
   });
   _elm.List.values = {_op: _op
                      ,isEmpty: isEmpty
                      ,length: length
                      ,reverse: reverse
                      ,member: member
                      ,head: head
                      ,tail: tail
                      ,filter: filter
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,append: append
                      ,concat: concat
                      ,intersperse: intersperse
                      ,partition: partition
                      ,unzip: unzip
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,filterMap: filterMap
                      ,concatMap: concatMap
                      ,indexedMap: indexedMap
                      ,foldr: foldr
                      ,foldl: foldl
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,all: all
                      ,any: any
                      ,scanl: scanl
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main",
   $Basics = Elm.Basics.make(_elm),
   $Common = Elm.Common.make(_elm),
   $GraphData = Elm.GraphData.make(_elm),
   $History = Elm.History.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Http = Elm.Http.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $OSC = Elm.OSC.make(_elm),
   $Router = Elm.Router.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $TopicData = Elm.TopicData.make(_elm),
   $Updates = Elm.Updates.make(_elm),
   $View = Elm.View.make(_elm);
   var neighborhood = Elm.Native.Port.make(_elm).inboundSignal("neighborhood",
   "List Model.Node",
   function (v) {
      return typeof v === "object" && v instanceof Array ? Elm.Native.List.make(_elm).fromArray(v.map(function (v) {
         return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _U.badPort("a string",
         v);
      })) : _U.badPort("an array",v);
   });
   var graphClosed = Elm.Native.Port.make(_elm).outboundSignal("graphClosed",
   function (v) {
      return v;
   },
   A2($Signal.map,
   function (x) {
      return !_U.eq(x,"/graph");
   },
   $History.path));
   var graphData = Elm.Native.Port.make(_elm).outboundSignal("graphData",
   function (v) {
      return v.ctor === "Nothing" ? null : {nodes: Elm.Native.List.make(_elm).toArray(v._0.nodes).map(function (v) {
                                              return v;
                                           })
                                           ,links: Elm.Native.List.make(_elm).toArray(v._0.links).map(function (v) {
                                              return [v._0,v._1];
                                           })};
   },
   $GraphData.graphRetrieve.signal);
   var oscOut = Elm.Native.Port.make(_elm).outboundSignal("oscOut",
   function (v) {
      return v.ctor === "Nothing" ? null : [v._0._0
                                           ,Elm.Native.List.make(_elm).toArray(v._0._1).map(function (v) {
                                              return [Elm.Native.List.make(_elm).toArray(v._0).map(function (v) {
                                                        return v;
                                                     })
                                                     ,Elm.Native.List.make(_elm).toArray(v._1).map(function (v) {
                                                        return v;
                                                     })];
                                           })];
   },
   $Updates.oscMessages);
   var oscConnection = Elm.Native.Port.make(_elm).inboundSignal("oscConnection",
   "Bool",
   function (v) {
      return typeof v === "boolean" ? v : _U.badPort("a boolean (true or false)",
      v);
   });
   var runActions = Elm.Native.Task.make(_elm).performSignal("runActions",
   $Updates.actions.signal);
   var onlyTasks = function (rr) {
      return function () {
         switch (rr.ctor)
         {case "ActionPage":
            return $Maybe.Just(rr._0);
            case "Redirect":
            return $Maybe.Just(rr._0);}
         return $Maybe.Nothing;
      }();
   };
   var onlyHtml = function (rr) {
      return function () {
         switch (rr.ctor)
         {case "ActionPage":
            return $Maybe.Just(rr._1);
            case "Page":
            return $Maybe.Just(rr._0);}
         return $Maybe.Nothing;
      }();
   };
   var state = function () {
      var f = F3(function (x,y,z) {
         return _U.replace([["oscConnected"
                            ,x]
                           ,["currentPath",y]
                           ,["playing",z]],
         $Model.defaultState);
      });
      return A4($Signal.map3,
      f,
      oscConnection,
      $History.path,
      $Updates.nowPlaying);
   }();
   var loadGraph = A2($Task.andThen,
   A2($Http.get,
   $GraphData.graphDec,
   "http://susurrant-audio.github.io/demo/data/graph.json"),
   $GraphData.sendGraphData);
   var fetchTopicData = Elm.Native.Task.make(_elm).perform(A2($Task.andThen,
   $TopicData.loadData,
   $TopicData.receivedData));
   var trackData = $Signal.mailbox($Maybe.Nothing);
   var loadTrack = function (trackID) {
      return function () {
         var trackUrl = A2($Basics._op["++"],
         "/data/tracks/",
         A2($Basics._op["++"],
         trackID,
         ".json"));
         return A2($Task.andThen,
         A2($Http.get,
         $TopicData.trackDataDec(trackID),
         trackUrl),
         function ($) {
            return $Signal.send(trackData.address)($Maybe.Just($));
         });
      }();
   };
   var model = A3($Signal.map2,
   $Model.Model,
   $TopicData.topicData.signal,
   trackData.signal);
   var ActionPage = F2(function (a,
   b) {
      return {ctor: "ActionPage"
             ,_0: a
             ,_1: b};
   });
   var trackRoute = F3(function (path,
   model,
   state) {
      return function () {
         var track = model.track;
         var data = A2($Common.orElse,
         model.data,
         $TopicData.emptyData);
         var trackID = A2($String.dropLeft,
         1,
         path);
         return ActionPage(loadTrack(trackID))($View.wrap(state)(A4($View.viewDoc,
         trackID,
         data,
         track,
         state)));
      }();
   });
   var graphRoute = F3(function (_v8,
   _v9,
   state) {
      return function () {
         return function () {
            return ActionPage(loadGraph)($View.wrap(state)($View.viewGraph));
         }();
      }();
   });
   var Redirect = function (a) {
      return {ctor: "Redirect"
             ,_0: a};
   };
   var routeToPath = function (x) {
      return Redirect($Updates.toPath(x));
   };
   var startPage = F3(function (_v12,
   _v13,
   _v14) {
      return function () {
         return function () {
            return function () {
               return routeToPath("/demo/index.html");
            }();
         }();
      }();
   });
   var Page = function (a) {
      return {ctor: "Page",_0: a};
   };
   var topicOverviewRoute = F3(function (path,
   model,
   state) {
      return Page($Html.text("null"));
   });
   var topicRoute = F3(function (path,
   model,
   state) {
      return function () {
         var data = A2($Common.orElse,
         model.data,
         $TopicData.emptyData);
         var topic = A2($Common.orElse,
         $String.toInt(A2($String.dropLeft,
         1,
         path)),
         -1);
         return Page($View.wrap(state)(A3($View.viewTopic,
         data,
         state,
         topic)));
      }();
   });
   var displayOverview = F3(function (path,
   model,
   state) {
      return Page($View.wrap(state)(A2($View.viewOverview,
      model,
      state)));
   });
   var route = A2($Router.match,
   _L.fromArray([A2($Router._op[":->"],
                "/demo/index.html",
                displayOverview)
                ,A2($Router._op[":->"],
                "/track",
                trackRoute)
                ,A2($Router._op[":->"],
                "/topics",
                topicOverviewRoute)
                ,A2($Router._op[":->"],
                "/topic",
                topicRoute)
                ,A2($Router._op[":->"],
                "/graph",
                graphRoute)]),
   startPage);
   var routed = A4($Signal.map3,
   route,
   $History.path,
   model,
   state);
   var routingTasks = Elm.Native.Task.make(_elm).performSignal("routingTasks",
   A3($Signal.filterMap,
   onlyTasks,
   $Task.succeed({ctor: "_Tuple0"}),
   routed));
   var main = A3($Signal.filterMap,
   onlyHtml,
   $Html.text(""),
   routed);
   _elm.Main.values = {_op: _op
                      ,Page: Page
                      ,Redirect: Redirect
                      ,ActionPage: ActionPage
                      ,routeToPath: routeToPath
                      ,startPage: startPage
                      ,topicOverviewRoute: topicOverviewRoute
                      ,topicRoute: topicRoute
                      ,trackRoute: trackRoute
                      ,displayOverview: displayOverview
                      ,graphRoute: graphRoute
                      ,route: route
                      ,trackData: trackData
                      ,loadTrack: loadTrack
                      ,loadGraph: loadGraph
                      ,model: model
                      ,state: state
                      ,routed: routed
                      ,onlyHtml: onlyHtml
                      ,onlyTasks: onlyTasks
                      ,main: main};
   return _elm.Main.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 56");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 73");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 73");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 107");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Model = Elm.Model || {};
Elm.Model.make = function (_elm) {
   "use strict";
   _elm.Model = _elm.Model || {};
   if (_elm.Model.values)
   return _elm.Model.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Model",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $String = Elm.String.make(_elm);
   var noInfo = function (track) {
      return {_: {}
             ,title: ""
             ,trackID: track
             ,url: ""
             ,username: ""};
   };
   var State = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,currentPath: b
             ,mode: a
             ,oscConnected: c
             ,playing: d};
   });
   var DocFocus = function (a) {
      return {ctor: "DocFocus"
             ,_0: a};
   };
   var TopicFocus = function (a) {
      return {ctor: "TopicFocus"
             ,_0: a};
   };
   var Overview = {ctor: "Overview"};
   var defaultState = {_: {}
                      ,currentPath: "/demo/index.html"
                      ,mode: Overview
                      ,oscConnected: false
                      ,playing: $Set.empty};
   var GraphData = F2(function (a,
   b) {
      return {_: {}
             ,links: b
             ,nodes: a};
   });
   var TrackTopics = F2(function (a,
   b) {
      return {_: {}
             ,topics: b
             ,track: a};
   });
   var TaggedToken = F3(function (a,
   b,
   c) {
      return {_: {}
             ,beat_coef: a
             ,chroma: b
             ,gfcc: c};
   });
   var TrackInfo = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,title: b
             ,trackID: a
             ,url: d
             ,username: c};
   });
   var Data = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,docMetadata: e
             ,docTopics: b
             ,tokenMaxLikelyTopic: g
             ,tokenTopics: c
             ,topicPrevalence: a
             ,topicTokens: d
             ,vocab: f};
   });
   var TokenDatum = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,id: b
             ,prob: d
             ,tokenType: c
             ,values: a};
   });
   var Text = {ctor: "Text"};
   var Chroma = {ctor: "Chroma"};
   var BeatCoef = {ctor: "BeatCoef"};
   var Gfcc = {ctor: "Gfcc"};
   var tokenTypeOf = function (x) {
      return A2($String.startsWith,
      "gfcc",
      x) ? Gfcc : A2($String.startsWith,
      "chroma",
      x) ? Chroma : A2($String.startsWith,
      "beat_coef",
      x) ? BeatCoef : Text;
   };
   var Model = F2(function (a,b) {
      return {_: {}
             ,data: a
             ,track: b};
   });
   _elm.Model.values = {_op: _op
                       ,Model: Model
                       ,Gfcc: Gfcc
                       ,BeatCoef: BeatCoef
                       ,Chroma: Chroma
                       ,Text: Text
                       ,TokenDatum: TokenDatum
                       ,Data: Data
                       ,TrackInfo: TrackInfo
                       ,TaggedToken: TaggedToken
                       ,TrackTopics: TrackTopics
                       ,GraphData: GraphData
                       ,Overview: Overview
                       ,TopicFocus: TopicFocus
                       ,DocFocus: DocFocus
                       ,State: State
                       ,defaultState: defaultState
                       ,noInfo: noInfo
                       ,tokenTypeOf: tokenTypeOf};
   return _elm.Model.values;
};
Elm.Mouse = Elm.Mouse || {};
Elm.Mouse.make = function (_elm) {
   "use strict";
   _elm.Mouse = _elm.Mouse || {};
   if (_elm.Mouse.values)
   return _elm.Mouse.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Mouse",
   $Basics = Elm.Basics.make(_elm),
   $Native$Mouse = Elm.Native.Mouse.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clicks = $Native$Mouse.clicks;
   var isDown = $Native$Mouse.isDown;
   var position = $Native$Mouse.position;
   var x = A2($Signal.map,
   $Basics.fst,
   position);
   var y = A2($Signal.map,
   $Basics.snd,
   position);
   _elm.Mouse.values = {_op: _op
                       ,position: position
                       ,x: x
                       ,y: y
                       ,isDown: isDown
                       ,clicks: clicks};
   return _elm.Mouse.values;
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: "_Array",
		height: 0,
		table: new Array()
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				"Index " + i + " is out of range. Check the length of " +
				"your array first or use getMaybe or getWithDefault.");
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height == 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len == 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h == 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: "_Array",
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list == List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = new Array();
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i == M)
			{
				var leaf = {
					ctor: "_Array",
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: "_Array",
				height: 0,
				table: table.splice(0,i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length == 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length == h)
		{
			var node = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length == M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height == 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: "_Array",
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed != null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height == 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? 0 : a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to == length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right == 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from == 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left == a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a,b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a,b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node)
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index == 0 || index == a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: "_Array",
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M == 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length)

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length == M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1,0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height == 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: "_Array",
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h == tree.height)
		{
			return tree;
		}

		return {
			ctor: "_Array",
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: "_Array",
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height == 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t == 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length == 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray:toJSArray,
		fromJSArray:fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;

}

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a/b)|0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error("Cannot perform mod 0. Division by zero error.");
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a,b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a,b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity
	}

	function truncate(n)
	{
		return n|0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees:  degrees,
		turns:  turns,
		fromPolar:  fromPolar,
		toPolar:  toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode : function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode   : function(c) { return c.charCodeAt(0); },
		toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
		toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Show.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary:F3(watchSummary),
	};
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx,style,path)
	{
		(style.dashing.ctor === '[]')
			? trace(ctx, path)
			: customLineHelp(ctx, style, path);
		ctx.scale(1,-1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0,y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(ctx, style);
		ctx.scale(1,-1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0,-2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w/2,
			destY = -h/2,
			destW = w,
			destH = h;

		ctx.scale(1,-1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta);
		}
		if (scale !== 1)
		{
			ctx.scale(scale,scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i].form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i];
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w,h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = "block";
		canvas.style.position = "absolute";
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w,h,div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w,h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w,h,div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w,h,forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w:w, h:h, forms:forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};

};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	function createNode(elementType)
	{
		var node = document.createElement(elementType);
		node.style.padding = "0";
		node.style.margin = "0";
		return node;
	}


	function newElement(width, height, elementPrim)
	{
		return {
			_: {},
			element: elementPrim,
			props: {
				_: {},
				id: Utils.guid(),
				width: width,
				height: height,
				opacity: 1,
				color: Maybe.Nothing,
				href: "",
				tag: "",
				hover: Utils.Tuple0,
				click: Utils.Tuple0
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width |0) + 'px';
		node.style.height = (height|0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img,props.width,props.height,img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = "block";
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = "hidden";

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw)|0) + 'px';
			img.style.height = ((this.height * sh)|0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
			img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp    : goDown,
		DDown  : goDown,
		DLeft  : goRight,
		DRight : goRight,
		DIn    : goOut,
		DOut   : goOut
	};
	function needsReversal(dir)
	{
		return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
	}

	function flow(dir,elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir == goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === "Absolute"
			? pos._0 + "px"
			: (pos._0 * 100) + "%";
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos,elem,e)
	{
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w/2)|0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h/2)|0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos,elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var guid = elem.guid;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = "hidden";
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(elem)
	{
		return setProps(elem, makeElement(elem));
	}
	function makeElement(e)
	{
		var elem = e.element;
		switch(elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, curr, next)
	{
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(next);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch(nextE.ctor)
		{
			case "Spacer":
				updateProps(node, curr, next);
				return rootNode;

			case "RawHtml":
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Image":
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE,currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(next);
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Flow":
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i].element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(next);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(next);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i-- ;)
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Container":
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case "Custom":
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(next);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width|0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height|0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor :'RawHtml',
				html : Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		}
	}

	function markdown(text)
	{
		var raw = {
			ctor:'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	function htmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + "px";
		}
		temp.style.visibility = "hidden";
		temp.style.styleFloat = "left";
		temp.style.cssFloat   = "left";

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
		var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w,h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.History = {};
Elm.Native.History.make = function(localRuntime){

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.History = localRuntime.Native.History || {};

  if (localRuntime.Native.History.values){
    return localRuntime.Native.History.values;
  }

  var NS = Elm.Native.Signal.make(localRuntime);
  var Task = Elm.Native.Task.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);
  var node = window;

  // path : Signal String
  var path = NS.input('History.path', window.location.pathname);

  // length : Signal Int
  var length = NS.input('History.length', window.history.length);

  // hash : Signal String
  var hash = NS.input('History.hash', window.location.hash);

  localRuntime.addListener([path.id, length.id], node, 'popstate', function getPath(event){
    localRuntime.notify(path.id, window.location.pathname);
    localRuntime.notify(length.id, window.history.length);
    localRuntime.notify(hash.id, window.location.hash);
  });

  localRuntime.addListener([hash.id], node, 'hashchange', function getHash(event){
    localRuntime.notify(hash.id, window.location.hash);
  });

  // setPath : String -> Task error ()
  var setPath = function(urlpath){
    return Task.asyncFunction(function(callback){
      setTimeout(function(){
        localRuntime.notify(path.id, urlpath);
        window.history.pushState({}, "", urlpath);
        localRuntime.notify(hash.id, window.location.hash);
        localRuntime.notify(length.id, window.history.length);

      },0);
      return callback(Task.succeed(Utils.Tuple0));
    });
  };

  // replacePath : String -> Task error ()
  var replacePath = function(urlpath){
    return Task.asyncFunction(function(callback){
      setTimeout(function(){
        localRuntime.notify(path.id, urlpath);
        window.history.replaceState({}, "", urlpath);
        localRuntime.notify(hash.id, window.location.hash);
        localRuntime.notify(length.id, window.history.length);
      },0);
      return callback(Task.succeed(Utils.Tuple0));
    });
  };

  // go : Int -> Task error ()
  var go = function(n){
    return Task.asyncFunction(function(callback){
      setTimeout(function(){
        window.history.go(n);
        localRuntime.notify(length.id, window.history.length);
        localRuntime.notify(hash.id, window.location.hash);
      }, 0);
      return callback(Task.succeed(Utils.Tuple0));
    });
  };

  // back : Task error ()
  var back = Task.asyncFunction(function(callback){
    setTimeout(function(){
      localRuntime.notify(hash.id, window.location.hash);
      window.history.back();
      localRuntime.notify(length.id, window.history.length);

    }, 0);
    return callback(Task.succeed(Utils.Tuple0));
  });

  // forward : Task error ()
  var forward = Task.asyncFunction(function(callback){
    setTimeout(function(){
      window.history.forward();
      localRuntime.notify(length.id, window.history.length);
      localRuntime.notify(hash.id, window.location.hash);
    }, 0);
    return callback(Task.succeed(Utils.Tuple0));
  });



  return {
    path        : path,
    setPath     : setPath,
    replacePath : replacePath,
    go          : go,
    back        : back,
    forward     : forward,
    length      : length,
    hash        : hash
  };

};

Elm.Native.Http = {};
Elm.Native.Http.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Http = localRuntime.Native.Http || {};
	if (localRuntime.Native.Http.values)
	{
		return localRuntime.Native.Http.values;
	}

	var Dict = Elm.Dict.make(localRuntime);
	var List = Elm.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);


	function send(settings, request)
	{
		return Task.asyncFunction(function(callback) {
			var req = new XMLHttpRequest();

			// start
			if (settings.onStart.ctor === 'Just')
			{
				req.addEventListener('loadStart', function() {
					var task = settings.onStart._0;
					Task.spawn(task);
				});
			}

			// progress
			if (settings.onProgress.ctor === 'Just')
			{
				req.addEventListener('progress', function(event) {
					var progress = !event.lengthComputable
						? Maybe.Nothing
						: Maybe.Just({
							_: {},
							loaded: event.loaded,
							total: event.total
						});
					var task = settings.onProgress._0(progress);
					Task.spawn(task);
				});
			}

			// end
			req.addEventListener('error', function() {
				return callback(Task.fail({ ctor: 'RawNetworkError' }));
			});

			req.addEventListener('timeout', function() {
				return callback(Task.fail({ ctor: 'RawTimeout' }));
			});

			req.addEventListener('load', function() {
				return callback(Task.succeed(toResponse(req)));
			});

			req.open(request.verb, request.url, true);

			// set all the headers
			function setHeader(pair) {
				req.setRequestHeader(pair._0, pair._1);
			}
			A2(List.map, setHeader, request.headers);

			// set the timeout
			req.timeout = settings.timeout;

			// ask for a specific MIME type for the response
			if (settings.desiredResponseType.ctor === 'Just')
			{
				req.overrideMimeType(settings.desiredResponseType._0);
			}

			req.send(request.body._0);
		});
	}


	// deal with responses

	function toResponse(req)
	{
		var tag = typeof req.response === 'string' ? 'Text' : 'Blob';
		return {
			_: {},
			status: req.status,
			statusText: req.statusText,
			headers: parseHeaders(req.getAllResponseHeaders()),
			url: req.responseURL,
			value: { ctor: tag, _0: req.response }
		};
	}


	function parseHeaders(rawHeaders)
	{
		var headers = Dict.empty;

		if (!rawHeaders)
		{
			return headers;
		}

		var headerPairs = rawHeaders.split('\u000d\u000a');
		for (var i = headerPairs.length; i--; )
		{
			var headerPair = headerPairs[i];
			var index = headerPair.indexOf('\u003a\u0020');
			if (index > 0)
			{
				var key = headerPair.substring(0, index);
				var value = headerPair.substring(index + 2);

				headers = A3(Dict.update, key, function(oldValue) {
					if (oldValue.ctor === 'Just')
					{
						return Maybe.Just(value + ', ' + oldValue._0);
					}
					return Maybe.Just(value);
				}, headers);
			}
		}

		return headers;
	}


	function multipart(dataList)
	{
		var formData = new FormData();

		while (dataList.ctor !== '[]')
		{
			var data = dataList._0;
			if (type === 'StringData')
			{
				formData.append(data._0, data._1);
			}
			else
			{
				var fileName = data._1.ctor === 'Nothing'
					? undefined
					: data._1._0;
				formData.append(data._0, data._2, fileName);
			}
			dataList = dataList._1;
		}

		return { ctor: 'FormData', formData: formData };
	}


	function uriEncode(string)
	{
		return encodeURIComponent(string);
	}

	function uriDecode(string)
	{
		return decodeURIComponent(string);
	}

	return localRuntime.Native.Http.values = {
		send: F2(send),
		multipart: multipart,
		uriEncode: uriEncode,
		uriDecode: uriDecode
	};
};

Elm.Native.Json = {};
Elm.Native.Json.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value === 'number' && (value|0) === value) {
			return value;
		}
		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i-- ; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i-- ; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value) {
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash("an object", value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		}
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		}
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		}
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		}
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		}
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};

};

Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo,hi)
	{
		var lst = Nil;
		if (lo <= hi)
		{
			do { lst = Cons(hi,lst) } while (hi-->lo);
		}
		return lst
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function any(pred, xs)
	{
		while (xs.ctor !== '[]')
		{
			if (pred(xs._0))
			{
				return true;
			}
			xs = xs._1;
		}
		return false;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}

	function drop(n, xs)
	{
		while (xs.ctor !== '[]' && n > 0)
		{
			xs = xs._1;
			--n;
		}
		return xs;
	}

	function repeat(n, x)
	{
		var arr = [];
		var pattern = [x];
		while (n > 0)
		{
			if (n & 1)
			{
				arr = arr.concat(pattern);
			}
			n >>= 1, pattern = pattern.concat(pattern);
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil:Nil,
		Cons:Cons,
		cons:F2(Cons),
		toArray:toArray,
		fromArray:fromArray,
		range:range,

		foldl:F3(foldl),
		foldr:F3(foldr),

		any:F2(any),
		map2:F3(map2),
		map3:F4(map3),
		map4:F5(map4),
		map5:F6(map5),
		sortBy:F2(sortBy),
		sortWith:F2(sortWith),
		take:F2(take),
		drop:F2(drop),
		repeat:F2(repeat)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;

};

Elm.Native = Elm.Native || {};
Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Mouse = localRuntime.Native.Mouse || {};
	if (localRuntime.Native.Mouse.values)
	{
		return localRuntime.Native.Mouse.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var position = NS.input('Mouse.position', Utils.Tuple2(0,0));

	var isDown = NS.input('Mouse.isDown', false);

	var clicks = NS.input('Mouse.clicks', Utils.Tuple0);

	var node = localRuntime.isFullscreen()
		? document
		: localRuntime.node;

	localRuntime.addListener([clicks.id], node, 'click', function click() {
		localRuntime.notify(clicks.id, Utils.Tuple0);
	});
	localRuntime.addListener([isDown.id], node, 'mousedown', function down() {
		localRuntime.notify(isDown.id, true);
	});
	localRuntime.addListener([isDown.id], node, 'mouseup', function up() {
		localRuntime.notify(isDown.id, false);
	});
	localRuntime.addListener([position.id], node, 'mousemove', function move(e) {
		localRuntime.notify(position.id, Utils.getXY(e));
	});

	return localRuntime.Native.Mouse.values = {
		position: position,
		isDown: isDown,
		clicks: clicks
	};
};

Elm.Native.Port = {};
Elm.Native.Port.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				"Port Error:\n" +
				"No argument was given for the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You need to provide an initial value!\n\n" +
				"Find out more about ports here <http://elm-lang.org/learn/Ports.elm>"
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				"Port Error:\n" +
				"Regarding the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You just sent the value:\n\n" +
				"    " + JSON.stringify(arg.value) + "\n\n" +
				"but it cannot be converted to the necessary type.\n" +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};


if (!Elm.fullscreen) {

	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];
				// elm.swap = newElm.swap;
				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild == 'undefined')
				{
					console.log(error.message);
				}
				else
				{
					container.appendChild(errorNode(error.message));
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		};

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode("Open the developer console for more details."));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.props)
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000/60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							"Unexpected draw callback.\n" +
							"Please report this to <https://github.com/elm-lang/core/issues>."
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld,nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld,nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b) } }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a,b,c) }}
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a,b,c,d) }}}
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a,b,c,d,e,f) }}}}}
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a,b,c,d,e,f,g,h)}}}}}}}
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun,a,b)
	{
		return fun.arity === 2
			? fun.func(a,b)
			: fun(a)(b);
	}
	function A3(fun,a,b,c)
	{
		return fun.arity === 3
			? fun.func(a,b,c)
			: fun(a)(b)(c);
	}
	function A4(fun,a,b,c,d)
	{
		return fun.arity === 4
			? fun.func(a,b,c,d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun,a,b,c,d,e)
	{
		return fun.arity === 5
			? fun.func(a,b,c,d,e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun,a,b,c,d,e,f)
	{
		return fun.arity === 6
			? fun.func(a,b,c,d,e,f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun,a,b,c,d,e,f,g)
	{
		return fun.arity === 7
			? fun.func(a,b,c,d,e,f,g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun,a,b,c,d,e,f,g,h)
	{
		return fun.arity === 8
			? fun.func(a,b,c,d,e,f,g,h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun,a,b,c,d,e,f,g,h,i)
	{
		return fun.arity === 9
			? fun.func(a,b,c,d,e,f,g,h,i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native.Show = {};
Elm.Native.Show.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Show = localRuntime.Native.Show || {};
	if (localRuntime.Native.Show.values)
	{
		return localRuntime.Native.Show.values;
	}

	var _Array;
	var Dict;
	var List;
	var Utils = Elm.Native.Utils.make(localRuntime);

	var toString = function(v)
	{
		var type = typeof v;
		if (type === "function")
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === "boolean")
		{
			return v ? "True" : "False";
		}
		else if (type === "number")
		{
			return v + "";
		}
		else if ((v instanceof String) && v.isChar)
		{
			return "'" + addSlashes(v, true) + "'";
		}
		else if (type === "string")
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === "object" && '_' in v && probablyPublic(v))
		{
			var output = [];
			for (var k in v._)
			{
				for (var i = v._[k].length; i--; )
				{
					output.push(k + " = " + toString(v._[k][i]));
				}
			}
			for (var k in v)
			{
				if (k === '_') continue;
				output.push(k + " = " + toString(v[k]));
			}
			if (output.length === 0)
			{
				return "{}";
			}
			return "{ " + output.join(", ") + " }";
		}
		else if (type === "object" && 'ctor' in v)
		{
			if (v.ctor.substring(0,6) === "_Tuple")
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return "(" + output.join(",") + ")";
			}
			else if (v.ctor === "_Array")
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return "Array.fromList " + toString(list);
			}
			else if (v.ctor === "::")
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === "::")
				{
					output += "," + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === "[]")
			{
				return "[]";
			}
			else if (v.ctor === "RBNode" || v.ctor === "RBEmpty")
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				if (!List)
				{
					List = Elm.List.make(localRuntime);
				}
				var list = Dict.toList(v);
				var name = "Dict";
				if (list.ctor === "::" && list._0._1.ctor === "_Tuple0")
				{
					name = "Set";
					list = A2(List.map, function(x){return x._0}, list);
				}
				return name + ".fromList " + toString(list);
			}
			else if (v.ctor.slice(0,5) === "Text:")
			{
				return '<text>'
			}
			else
			{
				var output = "";
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return 'initialValue' in v
				? '<Signal>'
				: '<Stream>';
		}
		return "<internal structure>";
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, "\\'")
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}

	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v)
		{
			return false;
		}
		else if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v)
		{
			return false;
		}
		else if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v)
		{
			return false;
		}
		return true;
	}

	return localRuntime.Native.Show.values = {
		toString: toString
	};
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			_: {},
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a,b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a,b,c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a,b,c,d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a,b,c,d,e]);
	}



	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.String = {};
Elm.Native.String.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr,str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd;
		return (hd = str[0])
			? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
			: Maybe.Nothing;
	}
	function append(a,b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f,str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred,str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f,b,str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f,b,str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}

	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}

	function slice(start, end, str)
	{
		return str.slice(start,end);
	}
	function left(n, str)
	{
		return n < 1 ? "" : str.slice(0,n);
	}
	function right(n, str)
	{
		return n < 1 ? "" : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0,-n);
	}

	function pad(n,chr,str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
	}
	function padRight(n,chr,str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n,chr,str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.trimLeft();
	}
	function trimRight(str)
	{
		return str.trimRight();
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Task = {};
Elm.Native.Task.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			setTimeout(function() {
				if (workQueue.length > 0)
				{
					runTask(workQueue[0], onComplete);
				}
			}, 0);
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== "Nothing")
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);;
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas['href'];
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length == 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  "&#38;")
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  "&#39;")
			.replace(/</g,  "&#60;")
			.replace(/>/g,  "&#62;");
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length == 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] == ' ')
		{
			arr[0] = "&nbsp;"
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] == ' ' && arr[i-1] == ' ')
			{
				arr[i-1] = arr[i-1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] == ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length-1] === " ")
		{
			return arr.slice(0,-1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(localRuntime)
{

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input,state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value,initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2( NS.map, fst, NS.timestamp(ticker) );
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new window.Date(t); },
		read: read
	};

};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1,0,0,0,1,0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t,m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
					  m21*c + m22*s, -m21*s + m22*c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity:identity,
		matrix:F6(matrix),
		rotation:rotation,
		multiply:F2(multiply)
		/*
		transform:F7(transform),
		rotate:F2(rotate),
		move:F2(move),
		scale:F2(scale),
		scaleX:F2(scaleX),
		scaleY:F2(scaleY),
		reflectX:reflectX,
		reflectY:reflectY
		*/
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}

	function eq(l,r)
	{
		var stack = [{'x': l, 'y': r}]
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === "object")
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];

	function compare(x,y)
	{
		return {
			ctor: ord[cmp(x,y)+1]
		};
	}

	function cmp(x,y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === "::" || x.ctor === "[]")
		{
			while (true)
			{
				if (x.ctor === "[]" && y.ctor === "[]")
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0,6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	var Tuple0 = {
		ctor: "_Tuple0"
	};

	function Tuple2(x,y)
	{
		return {
			ctor: "_Tuple2",
			_0: x,
			_1: y
		};
	}

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}

	var count = 0;
	function guid(_)
	{
		return count++
	}

	function copy(oldRecord)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = key === '_'
				? copy(oldRecord._)
				: oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}

	function remove(key, oldRecord)
	{
		var record = copy(oldRecord);
		if (key in record._)
		{
			record[key] = record._[key][0];
			record._[key] = record._[key].slice(1);
			if (record._[key].length === 0)
			{
				delete record._[key];
			}
		}
		else
		{
			delete record[key];
		}
		return record;
	}

	function replace(keyValuePairs, oldRecord)
	{
		var record = copy(oldRecord);
		for (var i = keyValuePairs.length; i--; )
		{
			var pair = keyValuePairs[i];
			record[pair[0]] = pair[1];
		}
		return record;
	}

	function insert(key, value, oldRecord)
	{
		var newRecord = copy(oldRecord);
		if (key in newRecord)
		{
			var values = newRecord._[key];
			var copiedValues = values ? values.slice(0) : [];
			newRecord._[key] = [newRecord[key]].concat(copiedValues);
		}
		newRecord[key] = value;
		return newRecord;
	}

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor:'[]' };

	function Cons(hd,tl)
	{
		return {
			ctor: "::",
			_0: hd,
			_1: tl
		};
	}

	function append(xs,ys)
	{
		// append Strings
		if (typeof xs === "string")
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0,5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}



		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}

	//// RUNTIME ERRORS ////

	function indent(lines)
	{
		return '\n' + lines.join('\n');
	}

	function badCase(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in case-expression.',
			'Make sure your patterns cover every case!'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}

	function badIf(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in multi-way-if expression.',
			'It is best to use \'otherwise\' as the last branch of multi-way-if.'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}


	function badPort(expected, received)
	{
		var msg = indent([
			'Expecting ' + expected + ' but was given ',
			JSON.stringify(received)
		]);
		throw new Error('Runtime error when sending values through a port.' + msg);
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		copy: copy,
		remove: remove,
		replace: replace,
		insert: insert,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		append: F2(append),

		badCase: badCase,
		badIf: badIf,
		badPort: badPort
	};
};

(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports = createHash

function createHash(elem) {
    var attributes = elem.attributes
    var hash = {}

    if (attributes === null || attributes === undefined) {
        return hash
    }

    for (var i = 0; i < attributes.length; i++) {
        var attr = attributes[i]

        if (attr.name.substr(0,5) !== "data-") {
            continue
        }

        hash[attr.name.substr(5)] = attr.value
    }

    return hash
}

},{}],2:[function(require,module,exports){
var createStore = require("weakmap-shim/create-store")
var Individual = require("individual")

var createHash = require("./create-hash.js")

var hashStore = Individual("__DATA_SET_WEAKMAP@3", createStore())

module.exports = DataSet

function DataSet(elem) {
    var store = hashStore(elem)

    if (!store.hash) {
        store.hash = createHash(elem)
    }

    return store.hash
}

},{"./create-hash.js":1,"individual":3,"weakmap-shim/create-store":4}],3:[function(require,module,exports){
(function (global){
var root = typeof window !== 'undefined' ?
    window : typeof global !== 'undefined' ?
    global : {};

module.exports = Individual

function Individual(key, value) {
    if (root[key]) {
        return root[key]
    }

    Object.defineProperty(root, key, {
        value: value
        , configurable: true
    })

    return value
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],4:[function(require,module,exports){
var hiddenStore = require('./hidden-store.js');

module.exports = createStore;

function createStore() {
    var key = {};

    return function (obj) {
        if (typeof obj !== 'object' || obj === null) {
            throw new Error('Weakmap-shim: Key must be object')
        }

        var store = obj.valueOf(key);
        return store && store.identity === key ?
            store : hiddenStore(obj, key);
    };
}

},{"./hidden-store.js":5}],5:[function(require,module,exports){
module.exports = hiddenStore;

function hiddenStore(obj, key) {
    var store = { identity: key };
    var valueOf = obj.valueOf;

    Object.defineProperty(obj, "valueOf", {
        value: function (value) {
            return value !== key ?
                valueOf.apply(this, arguments) : store;
        },
        writable: true
    });

    return store;
}

},{}],6:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = addEvent

function addEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        ds[type] = handler
    } else if (Array.isArray(events)) {
        if (events.indexOf(handler) === -1) {
            events.push(handler)
        }
    } else if (events !== handler) {
        ds[type] = [events, handler]
    }
}

},{"data-set":2}],7:[function(require,module,exports){
var globalDocument = require("global/document")
var DataSet = require("data-set")
var createStore = require("weakmap-shim/create-store")

var addEvent = require("./add-event.js")
var removeEvent = require("./remove-event.js")
var ProxyEvent = require("./proxy-event.js")

var HANDLER_STORE = createStore()

module.exports = DOMDelegator

function DOMDelegator(document) {
    document = document || globalDocument

    this.target = document.documentElement
    this.events = {}
    this.rawEventListeners = {}
    this.globalListeners = {}
}

DOMDelegator.prototype.addEventListener = addEvent
DOMDelegator.prototype.removeEventListener = removeEvent

DOMDelegator.prototype.allocateHandle =
    function allocateHandle(func) {
        var handle = new Handle()

        HANDLER_STORE(handle).func = func;

        return handle
    }

DOMDelegator.prototype.transformHandle =
    function transformHandle(handle, lambda) {
        var func = HANDLER_STORE(handle).func

        return this.allocateHandle(function (ev) {
            var result = lambda(ev)
            if (result) {
                func(result)
            }
        })
    }

DOMDelegator.prototype.addGlobalEventListener =
    function addGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];
        if (listeners.indexOf(fn) === -1) {
            listeners.push(fn)
        }

        this.globalListeners[eventName] = listeners;
    }

DOMDelegator.prototype.removeGlobalEventListener =
    function removeGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];

        var index = listeners.indexOf(fn)
        if (index !== -1) {
            listeners.splice(index, 1)
        }
    }

DOMDelegator.prototype.listenTo = function listenTo(eventName) {
    if (this.events[eventName]) {
        return
    }

    this.events[eventName] = true

    var listener = this.rawEventListeners[eventName]
    if (!listener) {
        listener = this.rawEventListeners[eventName] =
            createHandler(eventName, this)
    }

    this.target.addEventListener(eventName, listener, true)
}

DOMDelegator.prototype.unlistenTo = function unlistenTo(eventName) {
    if (!this.events[eventName]) {
        return
    }

    this.events[eventName] = false
    var listener = this.rawEventListeners[eventName]

    if (!listener) {
        throw new Error("dom-delegator#unlistenTo: cannot " +
            "unlisten to " + eventName)
    }

    this.target.removeEventListener(eventName, listener, true)
}

function createHandler(eventName, delegator) {
    var globalListeners = delegator.globalListeners;
    var delegatorTarget = delegator.target;

    return handler

    function handler(ev) {
        var globalHandlers = globalListeners[eventName] || []

        if (globalHandlers.length > 0) {
            var globalEvent = new ProxyEvent(ev);
            globalEvent.currentTarget = delegatorTarget;
            callListeners(globalHandlers, globalEvent)
        }

        findAndInvokeListeners(ev.target, ev, eventName)
    }
}

function findAndInvokeListeners(elem, ev, eventName) {
    var listener = getListener(elem, eventName)

    if (listener && listener.handlers.length > 0) {
        var listenerEvent = new ProxyEvent(ev);
        listenerEvent.currentTarget = listener.currentTarget
        callListeners(listener.handlers, listenerEvent)

        if (listenerEvent._bubbles) {
            var nextTarget = listener.currentTarget.parentNode
            findAndInvokeListeners(nextTarget, ev, eventName)
        }
    }
}

function getListener(target, type) {
    // terminate recursion if parent is `null`
    if (target === null) {
        return null
    }

    var ds = DataSet(target)
    // fetch list of handler fns for this event
    var handler = ds[type]
    var allHandler = ds.event

    if (!handler && !allHandler) {
        return getListener(target.parentNode, type)
    }

    var handlers = [].concat(handler || [], allHandler || [])
    return new Listener(target, handlers)
}

function callListeners(handlers, ev) {
    handlers.forEach(function (handler) {
        if (typeof handler === "function") {
            handler(ev)
        } else if (typeof handler.handleEvent === "function") {
            handler.handleEvent(ev)
        } else if (handler.type === "dom-delegator-handle") {
            HANDLER_STORE(handler).func(ev)
        } else {
            throw new Error("dom-delegator: unknown handler " +
                "found: " + JSON.stringify(handlers));
        }
    })
}

function Listener(target, handlers) {
    this.currentTarget = target
    this.handlers = handlers
}

function Handle() {
    this.type = "dom-delegator-handle"
}

},{"./add-event.js":6,"./proxy-event.js":15,"./remove-event.js":16,"data-set":2,"global/document":10,"weakmap-shim/create-store":13}],8:[function(require,module,exports){
var Individual = require("individual")
var cuid = require("cuid")
var globalDocument = require("global/document")

var DOMDelegator = require("./dom-delegator.js")

var delegatorCache = Individual("__DOM_DELEGATOR_CACHE@9", {
    delegators: {}
})
var commonEvents = [
    "blur", "change", "click",  "contextmenu", "dblclick",
    "error","focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "load", "mousedown", "mouseup",
    "resize", "scroll", "select", "submit", "touchcancel",
    "touchend", "touchstart", "unload"
]

/*  Delegator is a thin wrapper around a singleton `DOMDelegator`
        instance.

    Only one DOMDelegator should exist because we do not want
        duplicate event listeners bound to the DOM.

    `Delegator` will also `listenTo()` all events unless 
        every caller opts out of it
*/
module.exports = Delegator

function Delegator(opts) {
    opts = opts || {}
    var document = opts.document || globalDocument

    var cacheKey = document["__DOM_DELEGATOR_CACHE_TOKEN@9"]

    if (!cacheKey) {
        cacheKey =
            document["__DOM_DELEGATOR_CACHE_TOKEN@9"] = cuid()
    }

    var delegator = delegatorCache.delegators[cacheKey]

    if (!delegator) {
        delegator = delegatorCache.delegators[cacheKey] =
            new DOMDelegator(document)
    }

    if (opts.defaultEvents !== false) {
        for (var i = 0; i < commonEvents.length; i++) {
            delegator.listenTo(commonEvents[i])
        }
    }

    return delegator
}



},{"./dom-delegator.js":7,"cuid":9,"global/document":10,"individual":11}],9:[function(require,module,exports){
/**
 * cuid.js
 * Collision-resistant UID generator for browsers and node.
 * Sequential for fast db lookups and recency sorting.
 * Safe for element IDs and server-side lookups.
 *
 * Extracted from CLCTR
 * 
 * Copyright (c) Eric Elliott 2012
 * MIT License
 */

/*global window, navigator, document, require, process, module */
(function (app) {
  'use strict';
  var namespace = 'cuid',
    c = 0,
    blockSize = 4,
    base = 36,
    discreteValues = Math.pow(base, blockSize),

    pad = function pad(num, size) {
      var s = "000000000" + num;
      return s.substr(s.length-size);
    },

    randomBlock = function randomBlock() {
      return pad((Math.random() *
            discreteValues << 0)
            .toString(base), blockSize);
    },

    safeCounter = function () {
      c = (c < discreteValues) ? c : 0;
      c++; // this is not subliminal
      return c - 1;
    },

    api = function cuid() {
      // Starting with a lowercase letter makes
      // it HTML element ID friendly.
      var letter = 'c', // hard-coded allows for sequential access

        // timestamp
        // warning: this exposes the exact date and time
        // that the uid was created.
        timestamp = (new Date().getTime()).toString(base),

        // Prevent same-machine collisions.
        counter,

        // A few chars to generate distinct ids for different
        // clients (so different computers are far less
        // likely to generate the same id)
        fingerprint = api.fingerprint(),

        // Grab some more chars from Math.random()
        random = randomBlock() + randomBlock();

        counter = pad(safeCounter().toString(base), blockSize);

      return  (letter + timestamp + counter + fingerprint + random);
    };

  api.slug = function slug() {
    var date = new Date().getTime().toString(36),
      counter,
      print = api.fingerprint().slice(0,1) +
        api.fingerprint().slice(-1),
      random = randomBlock().slice(-2);

      counter = safeCounter().toString(36).slice(-4);

    return date.slice(-2) + 
      counter + print + random;
  };

  api.globalCount = function globalCount() {
    // We want to cache the results of this
    var cache = (function calc() {
        var i,
          count = 0;

        for (i in window) {
          count++;
        }

        return count;
      }());

    api.globalCount = function () { return cache; };
    return cache;
  };

  api.fingerprint = function browserPrint() {
    return pad((navigator.mimeTypes.length +
      navigator.userAgent.length).toString(36) +
      api.globalCount().toString(36), 4);
  };

  // don't change anything from here down.
  if (app.register) {
    app.register(namespace, api);
  } else if (typeof module !== 'undefined') {
    module.exports = api;
  } else {
    app[namespace] = api;
  }

}(this.applitude || this));

},{}],10:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":40}],11:[function(require,module,exports){
module.exports=require(3)
},{}],12:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],13:[function(require,module,exports){
module.exports=require(4)
},{"./hidden-store.js":14}],14:[function(require,module,exports){
module.exports=require(5)
},{}],15:[function(require,module,exports){
var inherits = require("inherits")

var ALL_PROPS = [
    "altKey", "bubbles", "cancelable", "ctrlKey",
    "eventPhase", "metaKey", "relatedTarget", "shiftKey",
    "target", "timeStamp", "type", "view", "which"
]
var KEY_PROPS = ["char", "charCode", "key", "keyCode"]
var MOUSE_PROPS = [
    "button", "buttons", "clientX", "clientY", "layerX",
    "layerY", "offsetX", "offsetY", "pageX", "pageY",
    "screenX", "screenY", "toElement"
]

var rkeyEvent = /^key|input/
var rmouseEvent = /^(?:mouse|pointer|contextmenu)|click/

module.exports = ProxyEvent

function ProxyEvent(ev) {
    if (!(this instanceof ProxyEvent)) {
        return new ProxyEvent(ev)
    }

    if (rkeyEvent.test(ev.type)) {
        return new KeyEvent(ev)
    } else if (rmouseEvent.test(ev.type)) {
        return new MouseEvent(ev)
    }

    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    this._rawEvent = ev
    this._bubbles = false;
}

ProxyEvent.prototype.preventDefault = function () {
    this._rawEvent.preventDefault()
}

ProxyEvent.prototype.startPropagation = function () {
    this._bubbles = true;
}

function MouseEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < MOUSE_PROPS.length; j++) {
        var mousePropKey = MOUSE_PROPS[j]
        this[mousePropKey] = ev[mousePropKey]
    }

    this._rawEvent = ev
}

inherits(MouseEvent, ProxyEvent)

function KeyEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < KEY_PROPS.length; j++) {
        var keyPropKey = KEY_PROPS[j]
        this[keyPropKey] = ev[keyPropKey]
    }

    this._rawEvent = ev
}

inherits(KeyEvent, ProxyEvent)

},{"inherits":12}],16:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = removeEvent

function removeEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        return
    } else if (Array.isArray(events)) {
        var index = events.indexOf(handler)
        if (index !== -1) {
            events.splice(index, 1)
        }
    } else if (events === handler) {
        ds[type] = null
    }
}

},{"data-set":2}],17:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("vtree/is-vhook")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, props, previous, propName);
        } else if (isHook(propValue)) {
            propValue.hook(node,
                propName,
                previous ? previous[propName] : undefined)
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else if (propValue !== undefined) {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, props, previous, propName) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"is-object":21,"vtree/is-vhook":29}],18:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("vtree/is-vnode")
var isVText = require("vtree/is-vtext")
var isWidget = require("vtree/is-widget")
var handleThunk = require("vtree/handle-thunk")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"./apply-properties":17,"global/document":20,"vtree/handle-thunk":27,"vtree/is-vnode":30,"vtree/is-vtext":31,"vtree/is-widget":32}],19:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],20:[function(require,module,exports){
module.exports=require(10)
},{"min-document":40}],21:[function(require,module,exports){
module.exports = isObject

function isObject(x) {
    return typeof x === "object" && x !== null
}

},{}],22:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],23:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("vtree/is-widget")
var VPatch = require("vtree/vpatch")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    if (updateWidget(leftVNode, widget)) {
        return widget.update(leftVNode, domNode) || domNode
    }

    var parentNode = domNode.parentNode
    var newWidget = render(widget, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newWidget, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newWidget
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, bIndex) {
    var children = []
    var childNodes = domNode.childNodes
    var len = childNodes.length
    var i
    var reverseIndex = bIndex.reverse

    for (i = 0; i < len; i++) {
        children.push(domNode.childNodes[i])
    }

    var insertOffset = 0
    var move
    var node
    var insertNode
    for (i = 0; i < len; i++) {
        move = bIndex[i]
        if (move !== undefined && move !== i) {
            // the element currently at this index will be moved later so increase the insert offset
            if (reverseIndex[i] > i) {
                insertOffset++
            }

            node = children[move]
            insertNode = childNodes[i + insertOffset] || null
            if (node !== insertNode) {
                domNode.insertBefore(node, insertNode)
            }

            // the moved element came from the front of the array so reduce the insert offset
            if (move < i) {
                insertOffset--
            }
        }

        // element at this index is scheduled to be removed so increase insert offset
        if (i in bIndex.removes) {
            insertOffset++
        }
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        console.log(oldRoot)
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"./apply-properties":17,"./create-element":18,"./update-widget":25,"vtree/is-widget":32,"vtree/vpatch":37}],24:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":19,"./patch-op":23,"global/document":20,"x-is-array":22}],25:[function(require,module,exports){
var isWidget = require("vtree/is-widget")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"vtree/is-widget":32}],26:[function(require,module,exports){
var isArray = require("x-is-array")
var isObject = require("is-object")

var VPatch = require("./vpatch")
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var handleThunk = require("./handle-thunk")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        if (isThunk(a) || isThunk(b)) {
            thunks(a, b, patch, index)
        } else {
            hooks(b, patch, index)
        }
        return
    }

    var apply = patch[index]

    if (b == null) {
        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
        destroyWidgets(a, patch, index)
    } else if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties, b.hooks)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                destroyWidgets(a, patch, index)
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            destroyWidgets(a, patch, index)
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            destroyWidgets(a, patch, index)
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))

        if (!isWidget(a)) {
            destroyWidgets(a, patch, index)
        }
    }

    if (apply) {
        patch[index] = apply
    }
}

function diffProps(a, b, hooks) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (hooks && aKey in hooks) {
            diff = diff || {}
            diff[aKey] = bValue
        } else {
            if (isObject(aValue) && isObject(bValue)) {
                if (getPrototype(bValue) !== getPrototype(aValue)) {
                    diff = diff || {}
                    diff[aKey] = bValue
                } else {
                    var objectDiff = diffProps(aValue, bValue)
                    if (objectDiff) {
                        diff = diff || {}
                        diff[aKey] = objectDiff
                    }
                }
            } else if (aValue !== bValue) {
                diff = diff || {}
                diff[aKey] = bValue
            }
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var bChildren = reorder(aChildren, b.children)

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else if (!rightNode) {
            if (leftNode) {
                // Excess nodes in a need to be removed
                patch[index] = new VPatch(VPatch.REMOVE, leftNode, null)
                destroyWidgets(leftNode, patch, index)
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (bChildren.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(VPatch.ORDER, a, bChildren.moves))
    }

    return apply
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = new VPatch(VPatch.REMOVE, vNode, null)
        }
    } else if (isVNode(vNode) && vNode.hasWidgets) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b);
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true;
        }
    }

    return false;
}

// Execute hooks when two nodes are identical
function hooks(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = new VPatch(VPatch.PROPS, vNode.hooks, vNode.hooks)
        }

        if (vNode.descendantHooks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                hooks(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    }
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {

    var bKeys = keyIndex(bChildren)

    if (!bKeys) {
        return bChildren
    }

    var aKeys = keyIndex(aChildren)

    if (!aKeys) {
        return bChildren
    }

    var bMatch = {}, aMatch = {}

    for (var key in bKeys) {
        bMatch[bKeys[key]] = aKeys[key]
    }

    for (var key in aKeys) {
        aMatch[aKeys[key]] = bKeys[key]
    }

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen
    var shuffle = []
    var freeIndex = 0
    var i = 0
    var moveIndex = 0
    var moves = {}
    var removes = moves.removes = {}
    var reverse = moves.reverse = {}
    var hasMoves = false

    while (freeIndex < len) {
        var move = aMatch[i]
        if (move !== undefined) {
            shuffle[i] = bChildren[move]
            if (move !== moveIndex) {
                moves[move] = moveIndex
                reverse[moveIndex] = move
                hasMoves = true
            }
            moveIndex++
        } else if (i in aMatch) {
            shuffle[i] = undefined
            removes[i] = moveIndex++
            hasMoves = true
        } else {
            while (bMatch[freeIndex] !== undefined) {
                freeIndex++
            }

            if (freeIndex < len) {
                var freeChild = bChildren[freeIndex]
                if (freeChild) {
                    shuffle[i] = freeChild
                    if (freeIndex !== moveIndex) {
                        hasMoves = true
                        moves[freeIndex] = moveIndex
                        reverse[moveIndex] = freeIndex
                    }
                    moveIndex++
                }
                freeIndex++
            }
        }
        i++
    }

    if (hasMoves) {
        shuffle.moves = moves
    }

    return shuffle
}

function keyIndex(children) {
    var i, keys

    for (i = 0; i < children.length; i++) {
        var child = children[i]

        if (child.key !== undefined) {
            keys = keys || {}
            keys[child.key] = i
        }
    }

    return keys
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"./handle-thunk":27,"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32,"./vpatch":37,"is-object":33,"x-is-array":34}],27:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32}],28:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],29:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook && typeof hook.hook === "function" &&
        !hook.hasOwnProperty("hook")
}

},{}],30:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":35}],31:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":35}],32:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],33:[function(require,module,exports){
module.exports=require(21)
},{}],34:[function(require,module,exports){
module.exports=require(22)
},{}],35:[function(require,module,exports){
module.exports = "1"

},{}],36:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property)) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-vhook":29,"./is-vnode":30,"./is-widget":32,"./version":35}],37:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":35}],38:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":35}],39:[function(require,module,exports){

var VNode = require('vtree/vnode');
var VText = require('vtree/vtext');
var diff = require('vtree/diff');
var patch = require('vdom/patch');
var createElement = require('vdom/create-element');
var DataSet = require("data-set");
var Delegator = require("dom-delegator");
var isHook = require("vtree/is-vhook");

Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	// This manages event listeners. Somehow...
	// Save a reference for use in on(...)
	var delegator = Delegator();

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}

	function node(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			name === 'input'
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}

	function on(name, decoder, createMessage)
	{
		// Ensure we're listening for this type of event
		delegator.listenTo(name);
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property(name, DataSetHook(eventHandler));
	}

	function DataSetHook(value)
	{
		if (!(this instanceof DataSetHook))
		{
			return new DataSetHook(value);
		}

		this.value = value;
	}

	DataSetHook.prototype.hook = function (node, propertyName) {
		var ds = DataSet(node);
		ds[propertyName] = this.value;
	};


	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};

	function text(string)
	{
		return new VText(string);
	}

	function fromElement(element)
	{
		return {
			type: "Widget",

			element: element,

			init: function () {
				return Element.render(element);
			},

			update: function (previous, node) {
				return Element.update(node, previous.element, element);
			}
		};
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}

	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}

	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		this.fn = fn;
		this.args = args;
		this.vnode = null;
		this.key = undefined;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.update = updateThunk;
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function updateThunk(previous, domNode)
	{
		if (!shouldUpdate(this, previous))
		{
			this.vnode = previous.vnode;
			return;
		}

		if (!this.vnode)
		{
			this.vnode = this.thunk();
		}

		var patches = diff(previous.vnode, this.vnode);
		patch(domNode, patches);
	}

	function renderThunk()
	{
		return this.thunk();
	}

	return Elm.Native.VirtualDom.values = {
		node: F3(node),
		text: text,
		on: F3(on),

		property: F2(property),
		attribute: F2(attribute),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"data-set":2,"dom-delegator":8,"vdom/create-element":18,"vdom/patch":24,"vtree/diff":26,"vtree/is-vhook":29,"vtree/vnode":36,"vtree/vtext":38}],40:[function(require,module,exports){

},{}]},{},[39]);

Elm.OSC = Elm.OSC || {};
Elm.OSC.make = function (_elm) {
   "use strict";
   _elm.OSC = _elm.OSC || {};
   if (_elm.OSC.values)
   return _elm.OSC.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "OSC",
   $List = Elm.List.make(_elm);
   var toOsc = function (m) {
      return function () {
         switch (m.ctor)
         {case "PlayTokens":
            return {ctor: "_Tuple2"
                   ,_0: "/tokens/play"
                   ,_1: A2($List.map,
                   function (_v4) {
                      return function () {
                         switch (_v4.ctor)
                         {case "_Tuple2":
                            return {ctor: "_Tuple2"
                                   ,_0: _L.fromArray([_v4._0])
                                   ,_1: _L.fromArray([_v4._1])};}
                         _U.badCase($moduleName,
                         "on line 28, column 47 to 55");
                      }();
                   },
                   m._0)};
            case "PlayTrack":
            return {ctor: "_Tuple2"
                   ,_0: "/track/play"
                   ,_1: _L.fromArray([{ctor: "_Tuple2"
                                      ,_0: _L.fromArray([m._0.track])
                                      ,_1: _L.fromArray([m._0.startPos])}])};
            case "SeekTrack":
            return {ctor: "_Tuple2"
                   ,_0: "/track/seek"
                   ,_1: _L.fromArray([{ctor: "_Tuple2"
                                      ,_0: _L.fromArray([])
                                      ,_1: _L.fromArray([m._0.pos])}])};
            case "StopTokens":
            return {ctor: "_Tuple2"
                   ,_0: "/tokens/stop"
                   ,_1: _L.fromArray([])};
            case "StopTrack":
            return {ctor: "_Tuple2"
                   ,_0: "/track/stop"
                   ,_1: _L.fromArray([])};}
         _U.badCase($moduleName,
         "between lines 20 and 59");
      }();
   };
   var StopTokens = {ctor: "StopTokens"};
   var PlayTokens = function (a) {
      return {ctor: "PlayTokens"
             ,_0: a};
   };
   var StopTrack = {ctor: "StopTrack"};
   var SeekTrack = function (a) {
      return {ctor: "SeekTrack"
             ,_0: a};
   };
   var PlayTrack = function (a) {
      return {ctor: "PlayTrack"
             ,_0: a};
   };
   _elm.OSC.values = {_op: _op
                     ,PlayTrack: PlayTrack
                     ,SeekTrack: SeekTrack
                     ,StopTrack: StopTrack
                     ,PlayTokens: PlayTokens
                     ,StopTokens: StopTokens
                     ,toOsc: toOsc};
   return _elm.OSC.values;
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 164 and 177");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 126 and 145");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 41 and 52");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 55 and 58");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 63 and 67");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 72 and 77");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 82 and 123");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 148 and 161");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 180 and 182");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe
                        ,formatError: formatError
                        ,Ok: Ok
                        ,Err: Err};
   return _elm.Result.values;
};
Elm.Router = Elm.Router || {};
Elm.Router.make = function (_elm) {
   "use strict";
   _elm.Router = _elm.Router || {};
   if (_elm.Router.values)
   return _elm.Router.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Router",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $String = Elm.String.make(_elm);
   _op[":->"] = F2(function (v0,
   v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   });
   var matchPrefix = F2(function (prefix,
   string) {
      return A2($String.startsWith,
      prefix,
      string) ? $Maybe.Just(A2($String.dropLeft,
      $String.length(prefix),
      string)) : $Maybe.Nothing;
   });
   var match = F3(function (routers,
   defaultRoute,
   url) {
      return function () {
         switch (routers.ctor)
         {case "::":
            switch (routers._0.ctor)
              {case "_Tuple2":
                 return _U.eq(routers._0._0,
                   "") || _U.eq(routers._0._0,
                   "/") ? _U.eq(url,
                   routers._0._0) ? routers._0._1(url) : A3(match,
                   routers._1,
                   defaultRoute,
                   url) : function () {
                      var _v5 = A2(matchPrefix,
                      routers._0._0,
                      url);
                      switch (_v5.ctor)
                      {case "Just":
                         return routers._0._1(_v5._0);
                         case "Nothing": return A3(match,
                           routers._1,
                           defaultRoute,
                           url);}
                      _U.badCase($moduleName,
                      "between lines 94 and 104");
                   }();}
              break;
            case "[]":
            return defaultRoute(url);}
         _U.badCase($moduleName,
         "between lines 81 and 104");
      }();
   });
   _elm.Router.values = {_op: _op
                        ,match: match
                        ,matchPrefix: matchPrefix};
   return _elm.Router.values;
};
Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values)
   return _elm.Set.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Set",
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm);
   var partition = F2(function (p,
   set) {
      return A2($Dict.partition,
      F2(function (k,_v0) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var filter = F2(function (p,
   set) {
      return A2($Dict.filter,
      F2(function (k,_v2) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var foldr = F3(function (f,
   b,
   s) {
      return A3($Dict.foldr,
      F3(function (k,_v4,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var foldl = F3(function (f,
   b,
   s) {
      return A3($Dict.foldl,
      F3(function (k,_v6,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var toList = $Dict.keys;
   var diff = $Dict.diff;
   var intersect = $Dict.intersect;
   var union = $Dict.union;
   var member = $Dict.member;
   var remove = $Dict.remove;
   var insert = function (k) {
      return A2($Dict.insert,
      k,
      {ctor: "_Tuple0"});
   };
   var singleton = function (k) {
      return A2($Dict.singleton,
      k,
      {ctor: "_Tuple0"});
   };
   var empty = $Dict.empty;
   var fromList = function (xs) {
      return A3($List.foldl,
      insert,
      empty,
      xs);
   };
   var map = F2(function (f,s) {
      return fromList(A2($List.map,
      f,
      toList(s)));
   });
   _elm.Set.values = {_op: _op
                     ,empty: empty
                     ,singleton: singleton
                     ,insert: insert
                     ,remove: remove
                     ,member: member
                     ,foldl: foldl
                     ,foldr: foldr
                     ,map: map
                     ,filter: filter
                     ,partition: partition
                     ,union: union
                     ,intersect: intersect
                     ,diff: diff
                     ,toList: toList
                     ,fromList: fromList};
   return _elm.Set.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var send = F2(function (_v0,
   value) {
      return function () {
         switch (_v0.ctor)
         {case "Address":
            return A2($Task.onError,
              _v0._0(value),
              function (_v3) {
                 return function () {
                    return $Task.succeed({ctor: "_Tuple0"});
                 }();
              });}
         _U.badCase($moduleName,
         "between lines 370 and 371");
      }();
   });
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var message = F2(function (_v5,
   value) {
      return function () {
         switch (_v5.ctor)
         {case "Address":
            return Message(_v5._0(value));}
         _U.badCase($moduleName,
         "on line 352, column 5 to 24");
      }();
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address"
             ,_0: a};
   };
   var forwardTo = F2(function (_v8,
   f) {
      return function () {
         switch (_v8.ctor)
         {case "Address":
            return Address(function (x) {
                 return _v8._0(f(x));
              });}
         _U.badCase($moduleName,
         "on line 339, column 5 to 29");
      }();
   });
   var Mailbox = F2(function (a,
   b) {
      return {_: {}
             ,address: a
             ,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,
   base,
   signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,
   right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      return function () {
         var _v11 = $List.reverse(signalList);
         switch (_v11.ctor)
         {case "::":
            return A3($List.foldl,
              merge,
              _v11._0,
              _v11._1);
            case "[]":
            return $Debug.crash("mergeMany was given an empty list!");}
         _U.badCase($moduleName,
         "between lines 177 and 197");
      }();
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   _op["~"] = F2(function (funcs,
   args) {
      return A3(map2,
      F2(function (f,v) {
         return f(v);
      }),
      funcs,
      args);
   });
   var map = $Native$Signal.map;
   _op["<~"] = map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,constant: constant
                        ,dropRepeats: dropRepeats
                        ,filter: filter
                        ,filterMap: filterMap
                        ,sampleOn: sampleOn
                        ,foldp: foldp
                        ,mailbox: mailbox
                        ,send: send
                        ,message: message
                        ,forwardTo: forwardTo
                        ,Mailbox: Mailbox};
   return _elm.Signal.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,length: length
                        ,reverse: reverse
                        ,repeat: repeat
                        ,cons: cons
                        ,uncons: uncons
                        ,fromChar: fromChar
                        ,append: append
                        ,concat: concat
                        ,split: split
                        ,join: join
                        ,words: words
                        ,lines: lines
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,map: map
                        ,filter: filter
                        ,foldl: foldl
                        ,foldr: foldr
                        ,any: any
                        ,all: all};
   return _elm.String.values;
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   if (_elm.Svg.values)
   return _elm.Svg.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Svg",
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var svgNamespace = A2($VirtualDom.property,
   "namespace",
   $Json$Encode.string("http://www.w3.org/2000/svg"));
   var svgNode = F3(function (name,
   attributes,
   children) {
      return A3($VirtualDom.node,
      name,
      A2($List._op["::"],
      svgNamespace,
      attributes),
      children);
   });
   var svg = F2(function (attributes,
   children) {
      return A3(svgNode,
      "svg",
      attributes,
      children);
   });
   var foreignObject = F2(function (attributes,
   children) {
      return A3(svgNode,
      "foreignObject",
      attributes,
      children);
   });
   var animate = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animate",
      attributes,
      children);
   });
   var animateColor = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateColor",
      attributes,
      children);
   });
   var animateMotion = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateMotion",
      attributes,
      children);
   });
   var animateTransform = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateTransform",
      attributes,
      children);
   });
   var mpath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "mpath",
      attributes,
      children);
   });
   var set = F2(function (attributes,
   children) {
      return A3(svgNode,
      "set",
      attributes,
      children);
   });
   var a = F2(function (attributes,
   children) {
      return A3(svgNode,
      "a",
      attributes,
      children);
   });
   var defs = F2(function (attributes,
   children) {
      return A3(svgNode,
      "defs",
      attributes,
      children);
   });
   var g = F2(function (attributes,
   children) {
      return A3(svgNode,
      "g",
      attributes,
      children);
   });
   var marker = F2(function (attributes,
   children) {
      return A3(svgNode,
      "marker",
      attributes,
      children);
   });
   var mask = F2(function (attributes,
   children) {
      return A3(svgNode,
      "mask",
      attributes,
      children);
   });
   var missingGlyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "missingGlyph",
      attributes,
      children);
   });
   var pattern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "pattern",
      attributes,
      children);
   });
   var $switch = F2(function (attributes,
   children) {
      return A3(svgNode,
      "switch",
      attributes,
      children);
   });
   var symbol = F2(function (attributes,
   children) {
      return A3(svgNode,
      "symbol",
      attributes,
      children);
   });
   var desc = F2(function (attributes,
   children) {
      return A3(svgNode,
      "desc",
      attributes,
      children);
   });
   var metadata = F2(function (attributes,
   children) {
      return A3(svgNode,
      "metadata",
      attributes,
      children);
   });
   var title = F2(function (attributes,
   children) {
      return A3(svgNode,
      "title",
      attributes,
      children);
   });
   var feBlend = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feBlend",
      attributes,
      children);
   });
   var feColorMatrix = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feColorMatrix",
      attributes,
      children);
   });
   var feComponentTransfer = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feComponentTransfer",
      attributes,
      children);
   });
   var feComposite = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feComposite",
      attributes,
      children);
   });
   var feConvolveMatrix = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feConvolveMatrix",
      attributes,
      children);
   });
   var feDiffuseLighting = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDiffuseLighting",
      attributes,
      children);
   });
   var feDisplacementMap = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDisplacementMap",
      attributes,
      children);
   });
   var feFlood = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFlood",
      attributes,
      children);
   });
   var feFuncA = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncA",
      attributes,
      children);
   });
   var feFuncB = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncB",
      attributes,
      children);
   });
   var feFuncG = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncG",
      attributes,
      children);
   });
   var feFuncR = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncR",
      attributes,
      children);
   });
   var feGaussianBlur = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feGaussianBlur",
      attributes,
      children);
   });
   var feImage = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feImage",
      attributes,
      children);
   });
   var feMerge = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMerge",
      attributes,
      children);
   });
   var feMergeNode = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMergeNode",
      attributes,
      children);
   });
   var feMorphology = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMorphology",
      attributes,
      children);
   });
   var feOffset = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feOffset",
      attributes,
      children);
   });
   var feSpecularLighting = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feSpecularLighting",
      attributes,
      children);
   });
   var feTile = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feTile",
      attributes,
      children);
   });
   var feTurbulence = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feTurbulence",
      attributes,
      children);
   });
   var font = F2(function (attributes,
   children) {
      return A3(svgNode,
      "font",
      attributes,
      children);
   });
   var fontFace = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFace",
      attributes,
      children);
   });
   var fontFaceFormat = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceFormat",
      attributes,
      children);
   });
   var fontFaceName = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceName",
      attributes,
      children);
   });
   var fontFaceSrc = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceSrc",
      attributes,
      children);
   });
   var fontFaceUri = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceUri",
      attributes,
      children);
   });
   var hkern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "hkern",
      attributes,
      children);
   });
   var vkern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "vkern",
      attributes,
      children);
   });
   var linearGradient = F2(function (attributes,
   children) {
      return A3(svgNode,
      "linearGradient",
      attributes,
      children);
   });
   var radialGradient = F2(function (attributes,
   children) {
      return A3(svgNode,
      "radialGradient",
      attributes,
      children);
   });
   var stop = F2(function (attributes,
   children) {
      return A3(svgNode,
      "stop",
      attributes,
      children);
   });
   var circle = F2(function (attributes,
   children) {
      return A3(svgNode,
      "circle",
      attributes,
      children);
   });
   var ellipse = F2(function (attributes,
   children) {
      return A3(svgNode,
      "ellipse",
      attributes,
      children);
   });
   var image = F2(function (attributes,
   children) {
      return A3(svgNode,
      "image",
      attributes,
      children);
   });
   var line = F2(function (attributes,
   children) {
      return A3(svgNode,
      "line",
      attributes,
      children);
   });
   var path = F2(function (attributes,
   children) {
      return A3(svgNode,
      "path",
      attributes,
      children);
   });
   var polygon = F2(function (attributes,
   children) {
      return A3(svgNode,
      "polygon",
      attributes,
      children);
   });
   var polyline = F2(function (attributes,
   children) {
      return A3(svgNode,
      "polyline",
      attributes,
      children);
   });
   var rect = F2(function (attributes,
   children) {
      return A3(svgNode,
      "rect",
      attributes,
      children);
   });
   var use = F2(function (attributes,
   children) {
      return A3(svgNode,
      "use",
      attributes,
      children);
   });
   var feDistantLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDistantLight",
      attributes,
      children);
   });
   var fePointLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fePointLight",
      attributes,
      children);
   });
   var feSpotLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feSpotLight",
      attributes,
      children);
   });
   var altGlyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyph",
      attributes,
      children);
   });
   var altGlyphDef = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyphDef",
      attributes,
      children);
   });
   var altGlyphItem = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyphItem",
      attributes,
      children);
   });
   var glyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "glyph",
      attributes,
      children);
   });
   var glyphRef = F2(function (attributes,
   children) {
      return A3(svgNode,
      "glyphRef",
      attributes,
      children);
   });
   var textPath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "textPath",
      attributes,
      children);
   });
   var text = F2(function (attributes,
   children) {
      return A3(svgNode,
      "text",
      attributes,
      children);
   });
   var tref = F2(function (attributes,
   children) {
      return A3(svgNode,
      "tref",
      attributes,
      children);
   });
   var tspan = F2(function (attributes,
   children) {
      return A3(svgNode,
      "tspan",
      attributes,
      children);
   });
   var clipPath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "clipPath",
      attributes,
      children);
   });
   var colorProfile = F2(function (attributes,
   children) {
      return A3(svgNode,
      "colorProfile",
      attributes,
      children);
   });
   var cursor = F2(function (attributes,
   children) {
      return A3(svgNode,
      "cursor",
      attributes,
      children);
   });
   var filter = F2(function (attributes,
   children) {
      return A3(svgNode,
      "filter",
      attributes,
      children);
   });
   var script = F2(function (attributes,
   children) {
      return A3(svgNode,
      "script",
      attributes,
      children);
   });
   var style = F2(function (attributes,
   children) {
      return A3(svgNode,
      "style",
      attributes,
      children);
   });
   var view = F2(function (attributes,
   children) {
      return A3(svgNode,
      "view",
      attributes,
      children);
   });
   _elm.Svg.values = {_op: _op
                     ,svgNamespace: svgNamespace
                     ,svgNode: svgNode
                     ,svg: svg
                     ,foreignObject: foreignObject
                     ,animate: animate
                     ,animateColor: animateColor
                     ,animateMotion: animateMotion
                     ,animateTransform: animateTransform
                     ,mpath: mpath
                     ,set: set
                     ,a: a
                     ,defs: defs
                     ,g: g
                     ,marker: marker
                     ,mask: mask
                     ,missingGlyph: missingGlyph
                     ,pattern: pattern
                     ,$switch: $switch
                     ,symbol: symbol
                     ,desc: desc
                     ,metadata: metadata
                     ,title: title
                     ,feBlend: feBlend
                     ,feColorMatrix: feColorMatrix
                     ,feComponentTransfer: feComponentTransfer
                     ,feComposite: feComposite
                     ,feConvolveMatrix: feConvolveMatrix
                     ,feDiffuseLighting: feDiffuseLighting
                     ,feDisplacementMap: feDisplacementMap
                     ,feFlood: feFlood
                     ,feFuncA: feFuncA
                     ,feFuncB: feFuncB
                     ,feFuncG: feFuncG
                     ,feFuncR: feFuncR
                     ,feGaussianBlur: feGaussianBlur
                     ,feImage: feImage
                     ,feMerge: feMerge
                     ,feMergeNode: feMergeNode
                     ,feMorphology: feMorphology
                     ,feOffset: feOffset
                     ,feSpecularLighting: feSpecularLighting
                     ,feTile: feTile
                     ,feTurbulence: feTurbulence
                     ,font: font
                     ,fontFace: fontFace
                     ,fontFaceFormat: fontFaceFormat
                     ,fontFaceName: fontFaceName
                     ,fontFaceSrc: fontFaceSrc
                     ,fontFaceUri: fontFaceUri
                     ,hkern: hkern
                     ,vkern: vkern
                     ,linearGradient: linearGradient
                     ,radialGradient: radialGradient
                     ,stop: stop
                     ,circle: circle
                     ,ellipse: ellipse
                     ,image: image
                     ,line: line
                     ,path: path
                     ,polygon: polygon
                     ,polyline: polyline
                     ,rect: rect
                     ,use: use
                     ,feDistantLight: feDistantLight
                     ,fePointLight: fePointLight
                     ,feSpotLight: feSpotLight
                     ,altGlyph: altGlyph
                     ,altGlyphDef: altGlyphDef
                     ,altGlyphItem: altGlyphItem
                     ,glyph: glyph
                     ,glyphRef: glyphRef
                     ,textPath: textPath
                     ,text: text
                     ,tref: tref
                     ,tspan: tspan
                     ,clipPath: clipPath
                     ,colorProfile: colorProfile
                     ,cursor: cursor
                     ,filter: filter
                     ,script: script
                     ,style: style
                     ,view: view};
   return _elm.Svg.values;
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.Attributes = Elm.Svg.Attributes || {};
Elm.Svg.Attributes.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   _elm.Svg.Attributes = _elm.Svg.Attributes || {};
   if (_elm.Svg.Attributes.values)
   return _elm.Svg.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Svg.Attributes",
   $Svg = Elm.Svg.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var writingMode = function (value) {
      return A2($VirtualDom.attribute,
      "writing-mode",
      value);
   };
   var wordSpacing = function (value) {
      return A2($VirtualDom.attribute,
      "word-spacing",
      value);
   };
   var visibility = function (value) {
      return A2($VirtualDom.attribute,
      "visibility",
      value);
   };
   var unicodeBidi = function (value) {
      return A2($VirtualDom.attribute,
      "unicode-bidi",
      value);
   };
   var textRendering = function (value) {
      return A2($VirtualDom.attribute,
      "text-rendering",
      value);
   };
   var textDecoration = function (value) {
      return A2($VirtualDom.attribute,
      "text-decoration",
      value);
   };
   var textAnchor = function (value) {
      return A2($VirtualDom.attribute,
      "text-anchor",
      value);
   };
   var stroke = function (value) {
      return A2($VirtualDom.attribute,
      "stroke",
      value);
   };
   var strokeWidth = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-width",
      value);
   };
   var strokeOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-opacity",
      value);
   };
   var strokeMiterlimit = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-miterlimit",
      value);
   };
   var strokeLinejoin = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-linejoin",
      value);
   };
   var strokeLinecap = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-linecap",
      value);
   };
   var strokeDashoffset = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-dashoffset",
      value);
   };
   var strokeDasharray = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-dasharray",
      value);
   };
   var stopOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "stop-opacity",
      value);
   };
   var stopColor = function (value) {
      return A2($VirtualDom.attribute,
      "stop-color",
      value);
   };
   var shapeRendering = function (value) {
      return A2($VirtualDom.attribute,
      "shape-rendering",
      value);
   };
   var pointerEvents = function (value) {
      return A2($VirtualDom.attribute,
      "pointer-events",
      value);
   };
   var overflow = function (value) {
      return A2($VirtualDom.attribute,
      "overflow",
      value);
   };
   var opacity = function (value) {
      return A2($VirtualDom.attribute,
      "opacity",
      value);
   };
   var mask = function (value) {
      return A2($VirtualDom.attribute,
      "mask",
      value);
   };
   var markerStart = function (value) {
      return A2($VirtualDom.attribute,
      "marker-start",
      value);
   };
   var markerMid = function (value) {
      return A2($VirtualDom.attribute,
      "marker-mid",
      value);
   };
   var markerEnd = function (value) {
      return A2($VirtualDom.attribute,
      "marker-end",
      value);
   };
   var lightingColor = function (value) {
      return A2($VirtualDom.attribute,
      "lighting-color",
      value);
   };
   var letterSpacing = function (value) {
      return A2($VirtualDom.attribute,
      "letter-spacing",
      value);
   };
   var kerning = function (value) {
      return A2($VirtualDom.attribute,
      "kerning",
      value);
   };
   var imageRendering = function (value) {
      return A2($VirtualDom.attribute,
      "image-rendering",
      value);
   };
   var glyphOrientationVertical = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-orientation-vertical",
      value);
   };
   var glyphOrientationHorizontal = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-orientation-horizontal",
      value);
   };
   var fontWeight = function (value) {
      return A2($VirtualDom.attribute,
      "font-weight",
      value);
   };
   var fontVariant = function (value) {
      return A2($VirtualDom.attribute,
      "font-variant",
      value);
   };
   var fontStyle = function (value) {
      return A2($VirtualDom.attribute,
      "font-style",
      value);
   };
   var fontStretch = function (value) {
      return A2($VirtualDom.attribute,
      "font-stretch",
      value);
   };
   var fontSize = function (value) {
      return A2($VirtualDom.attribute,
      "font-size",
      value);
   };
   var fontSizeAdjust = function (value) {
      return A2($VirtualDom.attribute,
      "font-size-adjust",
      value);
   };
   var fontFamily = function (value) {
      return A2($VirtualDom.attribute,
      "font-family",
      value);
   };
   var floodOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "flood-opacity",
      value);
   };
   var floodColor = function (value) {
      return A2($VirtualDom.attribute,
      "flood-color",
      value);
   };
   var filter = function (value) {
      return A2($VirtualDom.attribute,
      "filter",
      value);
   };
   var fill = function (value) {
      return A2($VirtualDom.attribute,
      "fill",
      value);
   };
   var fillRule = function (value) {
      return A2($VirtualDom.attribute,
      "fill-rule",
      value);
   };
   var fillOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "fill-opacity",
      value);
   };
   var enableBackground = function (value) {
      return A2($VirtualDom.attribute,
      "enable-background",
      value);
   };
   var dominantBaseline = function (value) {
      return A2($VirtualDom.attribute,
      "dominant-baseline",
      value);
   };
   var display = function (value) {
      return A2($VirtualDom.attribute,
      "display",
      value);
   };
   var direction = function (value) {
      return A2($VirtualDom.attribute,
      "direction",
      value);
   };
   var cursor = function (value) {
      return A2($VirtualDom.attribute,
      "cursor",
      value);
   };
   var color = function (value) {
      return A2($VirtualDom.attribute,
      "color",
      value);
   };
   var colorRendering = function (value) {
      return A2($VirtualDom.attribute,
      "color-rendering",
      value);
   };
   var colorProfile = function (value) {
      return A2($VirtualDom.attribute,
      "color-profile",
      value);
   };
   var colorInterpolation = function (value) {
      return A2($VirtualDom.attribute,
      "color-interpolation",
      value);
   };
   var colorInterpolationFilters = function (value) {
      return A2($VirtualDom.attribute,
      "color-interpolation-filters",
      value);
   };
   var clip = function (value) {
      return A2($VirtualDom.attribute,
      "clip",
      value);
   };
   var clipRule = function (value) {
      return A2($VirtualDom.attribute,
      "clip-rule",
      value);
   };
   var clipPath = function (value) {
      return A2($VirtualDom.attribute,
      "clip-path",
      value);
   };
   var baselineShift = function (value) {
      return A2($VirtualDom.attribute,
      "baseline-shift",
      value);
   };
   var alignmentBaseline = function (value) {
      return A2($VirtualDom.attribute,
      "alignment-baseline",
      value);
   };
   var zoomAndPan = function (value) {
      return A2($VirtualDom.attribute,
      "zoomAndPan",
      value);
   };
   var z = function (value) {
      return A2($VirtualDom.attribute,
      "z",
      value);
   };
   var yChannelSelector = function (value) {
      return A2($VirtualDom.attribute,
      "yChannelSelector",
      value);
   };
   var y2 = function (value) {
      return A2($VirtualDom.attribute,
      "y2",
      value);
   };
   var y1 = function (value) {
      return A2($VirtualDom.attribute,
      "y1",
      value);
   };
   var y = function (value) {
      return A2($VirtualDom.attribute,
      "y",
      value);
   };
   var xmlSpace = function (value) {
      return A2($VirtualDom.attribute,
      "xml:space",
      value);
   };
   var xmlLang = function (value) {
      return A2($VirtualDom.attribute,
      "xml:lang",
      value);
   };
   var xmlBase = function (value) {
      return A2($VirtualDom.attribute,
      "xml:base",
      value);
   };
   var xlinkType = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:type",
      value);
   };
   var xlinkTitle = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:title",
      value);
   };
   var xlinkShow = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:show",
      value);
   };
   var xlinkRole = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:role",
      value);
   };
   var xlinkHref = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:href",
      value);
   };
   var xlinkArcrole = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:arcrole",
      value);
   };
   var xlinkActuate = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:actuate",
      value);
   };
   var xChannelSelector = function (value) {
      return A2($VirtualDom.attribute,
      "xChannelSelector",
      value);
   };
   var x2 = function (value) {
      return A2($VirtualDom.attribute,
      "x2",
      value);
   };
   var x1 = function (value) {
      return A2($VirtualDom.attribute,
      "x1",
      value);
   };
   var xHeight = function (value) {
      return A2($VirtualDom.attribute,
      "x-height",
      value);
   };
   var x = function (value) {
      return A2($VirtualDom.attribute,
      "x",
      value);
   };
   var widths = function (value) {
      return A2($VirtualDom.attribute,
      "widths",
      value);
   };
   var width = function (value) {
      return A2($VirtualDom.attribute,
      "width",
      value);
   };
   var viewTarget = function (value) {
      return A2($VirtualDom.attribute,
      "viewTarget",
      value);
   };
   var viewBox = function (value) {
      return A2($VirtualDom.attribute,
      "viewBox",
      value);
   };
   var vertOriginY = function (value) {
      return A2($VirtualDom.attribute,
      "vert-origin-y",
      value);
   };
   var vertOriginX = function (value) {
      return A2($VirtualDom.attribute,
      "vert-origin-x",
      value);
   };
   var vertAdvY = function (value) {
      return A2($VirtualDom.attribute,
      "vert-adv-y",
      value);
   };
   var version = function (value) {
      return A2($VirtualDom.attribute,
      "version",
      value);
   };
   var values = function (value) {
      return A2($VirtualDom.attribute,
      "values",
      value);
   };
   var vMathematical = function (value) {
      return A2($VirtualDom.attribute,
      "v-mathematical",
      value);
   };
   var vIdeographic = function (value) {
      return A2($VirtualDom.attribute,
      "v-ideographic",
      value);
   };
   var vHanging = function (value) {
      return A2($VirtualDom.attribute,
      "v-hanging",
      value);
   };
   var vAlphabetic = function (value) {
      return A2($VirtualDom.attribute,
      "v-alphabetic",
      value);
   };
   var unitsPerEm = function (value) {
      return A2($VirtualDom.attribute,
      "units-per-em",
      value);
   };
   var unicodeRange = function (value) {
      return A2($VirtualDom.attribute,
      "unicode-range",
      value);
   };
   var unicode = function (value) {
      return A2($VirtualDom.attribute,
      "unicode",
      value);
   };
   var underlineThickness = function (value) {
      return A2($VirtualDom.attribute,
      "underline-thickness",
      value);
   };
   var underlinePosition = function (value) {
      return A2($VirtualDom.attribute,
      "underline-position",
      value);
   };
   var u2 = function (value) {
      return A2($VirtualDom.attribute,
      "u2",
      value);
   };
   var u1 = function (value) {
      return A2($VirtualDom.attribute,
      "u1",
      value);
   };
   var type$ = function (value) {
      return A2($VirtualDom.attribute,
      "type",
      value);
   };
   var transform = function (value) {
      return A2($VirtualDom.attribute,
      "transform",
      value);
   };
   var to = function (value) {
      return A2($VirtualDom.attribute,
      "to",
      value);
   };
   var title = function (value) {
      return A2($VirtualDom.attribute,
      "title",
      value);
   };
   var textLength = function (value) {
      return A2($VirtualDom.attribute,
      "textLength",
      value);
   };
   var targetY = function (value) {
      return A2($VirtualDom.attribute,
      "targetY",
      value);
   };
   var targetX = function (value) {
      return A2($VirtualDom.attribute,
      "targetX",
      value);
   };
   var target = function (value) {
      return A2($VirtualDom.attribute,
      "target",
      value);
   };
   var tableValues = function (value) {
      return A2($VirtualDom.attribute,
      "tableValues",
      value);
   };
   var systemLanguage = function (value) {
      return A2($VirtualDom.attribute,
      "systemLanguage",
      value);
   };
   var surfaceScale = function (value) {
      return A2($VirtualDom.attribute,
      "surfaceScale",
      value);
   };
   var style = function (value) {
      return A2($VirtualDom.attribute,
      "style",
      value);
   };
   var string = function (value) {
      return A2($VirtualDom.attribute,
      "string",
      value);
   };
   var strikethroughThickness = function (value) {
      return A2($VirtualDom.attribute,
      "strikethrough-thickness",
      value);
   };
   var strikethroughPosition = function (value) {
      return A2($VirtualDom.attribute,
      "strikethrough-position",
      value);
   };
   var stitchTiles = function (value) {
      return A2($VirtualDom.attribute,
      "stitchTiles",
      value);
   };
   var stemv = function (value) {
      return A2($VirtualDom.attribute,
      "stemv",
      value);
   };
   var stemh = function (value) {
      return A2($VirtualDom.attribute,
      "stemh",
      value);
   };
   var stdDeviation = function (value) {
      return A2($VirtualDom.attribute,
      "stdDeviation",
      value);
   };
   var startOffset = function (value) {
      return A2($VirtualDom.attribute,
      "startOffset",
      value);
   };
   var spreadMethod = function (value) {
      return A2($VirtualDom.attribute,
      "spreadMethod",
      value);
   };
   var speed = function (value) {
      return A2($VirtualDom.attribute,
      "speed",
      value);
   };
   var specularExponent = function (value) {
      return A2($VirtualDom.attribute,
      "specularExponent",
      value);
   };
   var specularConstant = function (value) {
      return A2($VirtualDom.attribute,
      "specularConstant",
      value);
   };
   var spacing = function (value) {
      return A2($VirtualDom.attribute,
      "spacing",
      value);
   };
   var slope = function (value) {
      return A2($VirtualDom.attribute,
      "slope",
      value);
   };
   var seed = function (value) {
      return A2($VirtualDom.attribute,
      "seed",
      value);
   };
   var scale = function (value) {
      return A2($VirtualDom.attribute,
      "scale",
      value);
   };
   var ry = function (value) {
      return A2($VirtualDom.attribute,
      "ry",
      value);
   };
   var rx = function (value) {
      return A2($VirtualDom.attribute,
      "rx",
      value);
   };
   var rotate = function (value) {
      return A2($VirtualDom.attribute,
      "rotate",
      value);
   };
   var result = function (value) {
      return A2($VirtualDom.attribute,
      "result",
      value);
   };
   var restart = function (value) {
      return A2($VirtualDom.attribute,
      "restart",
      value);
   };
   var requiredFeatures = function (value) {
      return A2($VirtualDom.attribute,
      "requiredFeatures",
      value);
   };
   var requiredExtensions = function (value) {
      return A2($VirtualDom.attribute,
      "requiredExtensions",
      value);
   };
   var repeatDur = function (value) {
      return A2($VirtualDom.attribute,
      "repeatDur",
      value);
   };
   var repeatCount = function (value) {
      return A2($VirtualDom.attribute,
      "repeatCount",
      value);
   };
   var renderingIntent = function (value) {
      return A2($VirtualDom.attribute,
      "rendering-intent",
      value);
   };
   var refY = function (value) {
      return A2($VirtualDom.attribute,
      "refY",
      value);
   };
   var refX = function (value) {
      return A2($VirtualDom.attribute,
      "refX",
      value);
   };
   var radius = function (value) {
      return A2($VirtualDom.attribute,
      "radius",
      value);
   };
   var r = function (value) {
      return A2($VirtualDom.attribute,
      "r",
      value);
   };
   var primitiveUnits = function (value) {
      return A2($VirtualDom.attribute,
      "primitiveUnits",
      value);
   };
   var preserveAspectRatio = function (value) {
      return A2($VirtualDom.attribute,
      "preserveAspectRatio",
      value);
   };
   var preserveAlpha = function (value) {
      return A2($VirtualDom.attribute,
      "preserveAlpha",
      value);
   };
   var pointsAtZ = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtZ",
      value);
   };
   var pointsAtY = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtY",
      value);
   };
   var pointsAtX = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtX",
      value);
   };
   var points = function (value) {
      return A2($VirtualDom.attribute,
      "points",
      value);
   };
   var pointOrder = function (value) {
      return A2($VirtualDom.attribute,
      "point-order",
      value);
   };
   var patternUnits = function (value) {
      return A2($VirtualDom.attribute,
      "patternUnits",
      value);
   };
   var patternTransform = function (value) {
      return A2($VirtualDom.attribute,
      "patternTransform",
      value);
   };
   var patternContentUnits = function (value) {
      return A2($VirtualDom.attribute,
      "patternContentUnits",
      value);
   };
   var pathLength = function (value) {
      return A2($VirtualDom.attribute,
      "pathLength",
      value);
   };
   var path = function (value) {
      return A2($VirtualDom.attribute,
      "path",
      value);
   };
   var panose1 = function (value) {
      return A2($VirtualDom.attribute,
      "panose-1",
      value);
   };
   var overlineThickness = function (value) {
      return A2($VirtualDom.attribute,
      "overline-thickness",
      value);
   };
   var overlinePosition = function (value) {
      return A2($VirtualDom.attribute,
      "overline-position",
      value);
   };
   var origin = function (value) {
      return A2($VirtualDom.attribute,
      "origin",
      value);
   };
   var orientation = function (value) {
      return A2($VirtualDom.attribute,
      "orientation",
      value);
   };
   var orient = function (value) {
      return A2($VirtualDom.attribute,
      "orient",
      value);
   };
   var order = function (value) {
      return A2($VirtualDom.attribute,
      "order",
      value);
   };
   var operator = function (value) {
      return A2($VirtualDom.attribute,
      "operator",
      value);
   };
   var offset = function (value) {
      return A2($VirtualDom.attribute,
      "offset",
      value);
   };
   var numOctaves = function (value) {
      return A2($VirtualDom.attribute,
      "numOctaves",
      value);
   };
   var name = function (value) {
      return A2($VirtualDom.attribute,
      "name",
      value);
   };
   var mode = function (value) {
      return A2($VirtualDom.attribute,
      "mode",
      value);
   };
   var min = function (value) {
      return A2($VirtualDom.attribute,
      "min",
      value);
   };
   var method = function (value) {
      return A2($VirtualDom.attribute,
      "method",
      value);
   };
   var media = function (value) {
      return A2($VirtualDom.attribute,
      "media",
      value);
   };
   var max = function (value) {
      return A2($VirtualDom.attribute,
      "max",
      value);
   };
   var mathematical = function (value) {
      return A2($VirtualDom.attribute,
      "mathematical",
      value);
   };
   var maskUnits = function (value) {
      return A2($VirtualDom.attribute,
      "maskUnits",
      value);
   };
   var maskContentUnits = function (value) {
      return A2($VirtualDom.attribute,
      "maskContentUnits",
      value);
   };
   var markerWidth = function (value) {
      return A2($VirtualDom.attribute,
      "markerWidth",
      value);
   };
   var markerUnits = function (value) {
      return A2($VirtualDom.attribute,
      "markerUnits",
      value);
   };
   var markerHeight = function (value) {
      return A2($VirtualDom.attribute,
      "markerHeight",
      value);
   };
   var local = function (value) {
      return A2($VirtualDom.attribute,
      "local",
      value);
   };
   var limitingConeAngle = function (value) {
      return A2($VirtualDom.attribute,
      "limitingConeAngle",
      value);
   };
   var lengthAdjust = function (value) {
      return A2($VirtualDom.attribute,
      "lengthAdjust",
      value);
   };
   var lang = function (value) {
      return A2($VirtualDom.attribute,
      "lang",
      value);
   };
   var keyTimes = function (value) {
      return A2($VirtualDom.attribute,
      "keyTimes",
      value);
   };
   var keySplines = function (value) {
      return A2($VirtualDom.attribute,
      "keySplines",
      value);
   };
   var keyPoints = function (value) {
      return A2($VirtualDom.attribute,
      "keyPoints",
      value);
   };
   var kernelUnitLength = function (value) {
      return A2($VirtualDom.attribute,
      "kernelUnitLength",
      value);
   };
   var kernelMatrix = function (value) {
      return A2($VirtualDom.attribute,
      "kernelMatrix",
      value);
   };
   var k4 = function (value) {
      return A2($VirtualDom.attribute,
      "k4",
      value);
   };
   var k3 = function (value) {
      return A2($VirtualDom.attribute,
      "k3",
      value);
   };
   var k2 = function (value) {
      return A2($VirtualDom.attribute,
      "k2",
      value);
   };
   var k1 = function (value) {
      return A2($VirtualDom.attribute,
      "k1",
      value);
   };
   var k = function (value) {
      return A2($VirtualDom.attribute,
      "k",
      value);
   };
   var intercept = function (value) {
      return A2($VirtualDom.attribute,
      "intercept",
      value);
   };
   var in2 = function (value) {
      return A2($VirtualDom.attribute,
      "in2",
      value);
   };
   var in$ = function (value) {
      return A2($VirtualDom.attribute,
      "in",
      value);
   };
   var ideographic = function (value) {
      return A2($VirtualDom.attribute,
      "ideographic",
      value);
   };
   var id = function (value) {
      return A2($VirtualDom.attribute,
      "id",
      value);
   };
   var horizOriginY = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-origin-y",
      value);
   };
   var horizOriginX = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-origin-x",
      value);
   };
   var horizAdvX = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-adv-x",
      value);
   };
   var height = function (value) {
      return A2($VirtualDom.attribute,
      "height",
      value);
   };
   var hanging = function (value) {
      return A2($VirtualDom.attribute,
      "hanging",
      value);
   };
   var gradientUnits = function (value) {
      return A2($VirtualDom.attribute,
      "gradientUnits",
      value);
   };
   var gradientTransform = function (value) {
      return A2($VirtualDom.attribute,
      "gradientTransform",
      value);
   };
   var glyphRef = function (value) {
      return A2($VirtualDom.attribute,
      "glyphRef",
      value);
   };
   var glyphName = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-name",
      value);
   };
   var g2 = function (value) {
      return A2($VirtualDom.attribute,
      "g2",
      value);
   };
   var g1 = function (value) {
      return A2($VirtualDom.attribute,
      "g1",
      value);
   };
   var fy = function (value) {
      return A2($VirtualDom.attribute,
      "fy",
      value);
   };
   var fx = function (value) {
      return A2($VirtualDom.attribute,
      "fx",
      value);
   };
   var from = function (value) {
      return A2($VirtualDom.attribute,
      "from",
      value);
   };
   var format = function (value) {
      return A2($VirtualDom.attribute,
      "format",
      value);
   };
   var filterUnits = function (value) {
      return A2($VirtualDom.attribute,
      "filterUnits",
      value);
   };
   var filterRes = function (value) {
      return A2($VirtualDom.attribute,
      "filterRes",
      value);
   };
   var externalResourcesRequired = function (value) {
      return A2($VirtualDom.attribute,
      "externalResourcesRequired",
      value);
   };
   var exponent = function (value) {
      return A2($VirtualDom.attribute,
      "exponent",
      value);
   };
   var end = function (value) {
      return A2($VirtualDom.attribute,
      "end",
      value);
   };
   var elevation = function (value) {
      return A2($VirtualDom.attribute,
      "elevation",
      value);
   };
   var edgeMode = function (value) {
      return A2($VirtualDom.attribute,
      "edgeMode",
      value);
   };
   var dy = function (value) {
      return A2($VirtualDom.attribute,
      "dy",
      value);
   };
   var dx = function (value) {
      return A2($VirtualDom.attribute,
      "dx",
      value);
   };
   var dur = function (value) {
      return A2($VirtualDom.attribute,
      "dur",
      value);
   };
   var divisor = function (value) {
      return A2($VirtualDom.attribute,
      "divisor",
      value);
   };
   var diffuseConstant = function (value) {
      return A2($VirtualDom.attribute,
      "diffuseConstant",
      value);
   };
   var descent = function (value) {
      return A2($VirtualDom.attribute,
      "descent",
      value);
   };
   var decelerate = function (value) {
      return A2($VirtualDom.attribute,
      "decelerate",
      value);
   };
   var d = function (value) {
      return A2($VirtualDom.attribute,
      "d",
      value);
   };
   var cy = function (value) {
      return A2($VirtualDom.attribute,
      "cy",
      value);
   };
   var cx = function (value) {
      return A2($VirtualDom.attribute,
      "cx",
      value);
   };
   var contentStyleType = function (value) {
      return A2($VirtualDom.attribute,
      "contentStyleType",
      value);
   };
   var contentScriptType = function (value) {
      return A2($VirtualDom.attribute,
      "contentScriptType",
      value);
   };
   var clipPathUnits = function (value) {
      return A2($VirtualDom.attribute,
      "clipPathUnits",
      value);
   };
   var $class = function (value) {
      return A2($VirtualDom.attribute,
      "class",
      value);
   };
   var capHeight = function (value) {
      return A2($VirtualDom.attribute,
      "cap-height",
      value);
   };
   var calcMode = function (value) {
      return A2($VirtualDom.attribute,
      "calcMode",
      value);
   };
   var by = function (value) {
      return A2($VirtualDom.attribute,
      "by",
      value);
   };
   var bias = function (value) {
      return A2($VirtualDom.attribute,
      "bias",
      value);
   };
   var begin = function (value) {
      return A2($VirtualDom.attribute,
      "begin",
      value);
   };
   var bbox = function (value) {
      return A2($VirtualDom.attribute,
      "bbox",
      value);
   };
   var baseProfile = function (value) {
      return A2($VirtualDom.attribute,
      "baseProfile",
      value);
   };
   var baseFrequency = function (value) {
      return A2($VirtualDom.attribute,
      "baseFrequency",
      value);
   };
   var azimuth = function (value) {
      return A2($VirtualDom.attribute,
      "azimuth",
      value);
   };
   var autoReverse = function (value) {
      return A2($VirtualDom.attribute,
      "autoReverse",
      value);
   };
   var attributeType = function (value) {
      return A2($VirtualDom.attribute,
      "attributeType",
      value);
   };
   var attributeName = function (value) {
      return A2($VirtualDom.attribute,
      "attributeName",
      value);
   };
   var ascent = function (value) {
      return A2($VirtualDom.attribute,
      "ascent",
      value);
   };
   var arabicForm = function (value) {
      return A2($VirtualDom.attribute,
      "arabic-form",
      value);
   };
   var amplitude = function (value) {
      return A2($VirtualDom.attribute,
      "amplitude",
      value);
   };
   var allowReorder = function (value) {
      return A2($VirtualDom.attribute,
      "allowReorder",
      value);
   };
   var alphabetic = function (value) {
      return A2($VirtualDom.attribute,
      "alphabetic",
      value);
   };
   var additive = function (value) {
      return A2($VirtualDom.attribute,
      "additive",
      value);
   };
   var accumulate = function (value) {
      return A2($VirtualDom.attribute,
      "accumulate",
      value);
   };
   var accelerate = function (value) {
      return A2($VirtualDom.attribute,
      "accelerate",
      value);
   };
   var accentHeight = function (value) {
      return A2($VirtualDom.attribute,
      "accent-height",
      value);
   };
   _elm.Svg.Attributes.values = {_op: _op
                                ,accentHeight: accentHeight
                                ,accelerate: accelerate
                                ,accumulate: accumulate
                                ,additive: additive
                                ,alphabetic: alphabetic
                                ,allowReorder: allowReorder
                                ,amplitude: amplitude
                                ,arabicForm: arabicForm
                                ,ascent: ascent
                                ,attributeName: attributeName
                                ,attributeType: attributeType
                                ,autoReverse: autoReverse
                                ,azimuth: azimuth
                                ,baseFrequency: baseFrequency
                                ,baseProfile: baseProfile
                                ,bbox: bbox
                                ,begin: begin
                                ,bias: bias
                                ,by: by
                                ,calcMode: calcMode
                                ,capHeight: capHeight
                                ,$class: $class
                                ,clipPathUnits: clipPathUnits
                                ,contentScriptType: contentScriptType
                                ,contentStyleType: contentStyleType
                                ,cx: cx
                                ,cy: cy
                                ,d: d
                                ,decelerate: decelerate
                                ,descent: descent
                                ,diffuseConstant: diffuseConstant
                                ,divisor: divisor
                                ,dur: dur
                                ,dx: dx
                                ,dy: dy
                                ,edgeMode: edgeMode
                                ,elevation: elevation
                                ,end: end
                                ,exponent: exponent
                                ,externalResourcesRequired: externalResourcesRequired
                                ,filterRes: filterRes
                                ,filterUnits: filterUnits
                                ,format: format
                                ,from: from
                                ,fx: fx
                                ,fy: fy
                                ,g1: g1
                                ,g2: g2
                                ,glyphName: glyphName
                                ,glyphRef: glyphRef
                                ,gradientTransform: gradientTransform
                                ,gradientUnits: gradientUnits
                                ,hanging: hanging
                                ,height: height
                                ,horizAdvX: horizAdvX
                                ,horizOriginX: horizOriginX
                                ,horizOriginY: horizOriginY
                                ,id: id
                                ,ideographic: ideographic
                                ,in$: in$
                                ,in2: in2
                                ,intercept: intercept
                                ,k: k
                                ,k1: k1
                                ,k2: k2
                                ,k3: k3
                                ,k4: k4
                                ,kernelMatrix: kernelMatrix
                                ,kernelUnitLength: kernelUnitLength
                                ,keyPoints: keyPoints
                                ,keySplines: keySplines
                                ,keyTimes: keyTimes
                                ,lang: lang
                                ,lengthAdjust: lengthAdjust
                                ,limitingConeAngle: limitingConeAngle
                                ,local: local
                                ,markerHeight: markerHeight
                                ,markerUnits: markerUnits
                                ,markerWidth: markerWidth
                                ,maskContentUnits: maskContentUnits
                                ,maskUnits: maskUnits
                                ,mathematical: mathematical
                                ,max: max
                                ,media: media
                                ,method: method
                                ,min: min
                                ,mode: mode
                                ,name: name
                                ,numOctaves: numOctaves
                                ,offset: offset
                                ,operator: operator
                                ,order: order
                                ,orient: orient
                                ,orientation: orientation
                                ,origin: origin
                                ,overlinePosition: overlinePosition
                                ,overlineThickness: overlineThickness
                                ,panose1: panose1
                                ,path: path
                                ,pathLength: pathLength
                                ,patternContentUnits: patternContentUnits
                                ,patternTransform: patternTransform
                                ,patternUnits: patternUnits
                                ,pointOrder: pointOrder
                                ,points: points
                                ,pointsAtX: pointsAtX
                                ,pointsAtY: pointsAtY
                                ,pointsAtZ: pointsAtZ
                                ,preserveAlpha: preserveAlpha
                                ,preserveAspectRatio: preserveAspectRatio
                                ,primitiveUnits: primitiveUnits
                                ,r: r
                                ,radius: radius
                                ,refX: refX
                                ,refY: refY
                                ,renderingIntent: renderingIntent
                                ,repeatCount: repeatCount
                                ,repeatDur: repeatDur
                                ,requiredExtensions: requiredExtensions
                                ,requiredFeatures: requiredFeatures
                                ,restart: restart
                                ,result: result
                                ,rotate: rotate
                                ,rx: rx
                                ,ry: ry
                                ,scale: scale
                                ,seed: seed
                                ,slope: slope
                                ,spacing: spacing
                                ,specularConstant: specularConstant
                                ,specularExponent: specularExponent
                                ,speed: speed
                                ,spreadMethod: spreadMethod
                                ,startOffset: startOffset
                                ,stdDeviation: stdDeviation
                                ,stemh: stemh
                                ,stemv: stemv
                                ,stitchTiles: stitchTiles
                                ,strikethroughPosition: strikethroughPosition
                                ,strikethroughThickness: strikethroughThickness
                                ,string: string
                                ,style: style
                                ,surfaceScale: surfaceScale
                                ,systemLanguage: systemLanguage
                                ,tableValues: tableValues
                                ,target: target
                                ,targetX: targetX
                                ,targetY: targetY
                                ,textLength: textLength
                                ,title: title
                                ,to: to
                                ,transform: transform
                                ,type$: type$
                                ,u1: u1
                                ,u2: u2
                                ,underlinePosition: underlinePosition
                                ,underlineThickness: underlineThickness
                                ,unicode: unicode
                                ,unicodeRange: unicodeRange
                                ,unitsPerEm: unitsPerEm
                                ,vAlphabetic: vAlphabetic
                                ,vHanging: vHanging
                                ,vIdeographic: vIdeographic
                                ,vMathematical: vMathematical
                                ,values: values
                                ,version: version
                                ,vertAdvY: vertAdvY
                                ,vertOriginX: vertOriginX
                                ,vertOriginY: vertOriginY
                                ,viewBox: viewBox
                                ,viewTarget: viewTarget
                                ,width: width
                                ,widths: widths
                                ,x: x
                                ,xHeight: xHeight
                                ,x1: x1
                                ,x2: x2
                                ,xChannelSelector: xChannelSelector
                                ,xlinkActuate: xlinkActuate
                                ,xlinkArcrole: xlinkArcrole
                                ,xlinkHref: xlinkHref
                                ,xlinkRole: xlinkRole
                                ,xlinkShow: xlinkShow
                                ,xlinkTitle: xlinkTitle
                                ,xlinkType: xlinkType
                                ,xmlBase: xmlBase
                                ,xmlLang: xmlLang
                                ,xmlSpace: xmlSpace
                                ,y: y
                                ,y1: y1
                                ,y2: y2
                                ,yChannelSelector: yChannelSelector
                                ,z: z
                                ,zoomAndPan: zoomAndPan
                                ,alignmentBaseline: alignmentBaseline
                                ,baselineShift: baselineShift
                                ,clipPath: clipPath
                                ,clipRule: clipRule
                                ,clip: clip
                                ,colorInterpolationFilters: colorInterpolationFilters
                                ,colorInterpolation: colorInterpolation
                                ,colorProfile: colorProfile
                                ,colorRendering: colorRendering
                                ,color: color
                                ,cursor: cursor
                                ,direction: direction
                                ,display: display
                                ,dominantBaseline: dominantBaseline
                                ,enableBackground: enableBackground
                                ,fillOpacity: fillOpacity
                                ,fillRule: fillRule
                                ,fill: fill
                                ,filter: filter
                                ,floodColor: floodColor
                                ,floodOpacity: floodOpacity
                                ,fontFamily: fontFamily
                                ,fontSizeAdjust: fontSizeAdjust
                                ,fontSize: fontSize
                                ,fontStretch: fontStretch
                                ,fontStyle: fontStyle
                                ,fontVariant: fontVariant
                                ,fontWeight: fontWeight
                                ,glyphOrientationHorizontal: glyphOrientationHorizontal
                                ,glyphOrientationVertical: glyphOrientationVertical
                                ,imageRendering: imageRendering
                                ,kerning: kerning
                                ,letterSpacing: letterSpacing
                                ,lightingColor: lightingColor
                                ,markerEnd: markerEnd
                                ,markerMid: markerMid
                                ,markerStart: markerStart
                                ,mask: mask
                                ,opacity: opacity
                                ,overflow: overflow
                                ,pointerEvents: pointerEvents
                                ,shapeRendering: shapeRendering
                                ,stopColor: stopColor
                                ,stopOpacity: stopOpacity
                                ,strokeDasharray: strokeDasharray
                                ,strokeDashoffset: strokeDashoffset
                                ,strokeLinecap: strokeLinecap
                                ,strokeLinejoin: strokeLinejoin
                                ,strokeMiterlimit: strokeMiterlimit
                                ,strokeOpacity: strokeOpacity
                                ,strokeWidth: strokeWidth
                                ,stroke: stroke
                                ,textAnchor: textAnchor
                                ,textDecoration: textDecoration
                                ,textRendering: textRendering
                                ,unicodeBidi: unicodeBidi
                                ,visibility: visibility
                                ,wordSpacing: wordSpacing
                                ,writingMode: writingMode};
   return _elm.Svg.Attributes.values;
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.Lazy = Elm.Svg.Lazy || {};
Elm.Svg.Lazy.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   _elm.Svg.Lazy = _elm.Svg.Lazy || {};
   if (_elm.Svg.Lazy.values)
   return _elm.Svg.Lazy.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Svg.Lazy",
   $Svg = Elm.Svg.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var lazy3 = $VirtualDom.lazy3;
   var lazy2 = $VirtualDom.lazy2;
   var lazy = $VirtualDom.lazy;
   _elm.Svg.Lazy.values = {_op: _op
                          ,lazy: lazy
                          ,lazy2: lazy2
                          ,lazy3: lazy3};
   return _elm.Svg.Lazy.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values)
   return _elm.Task.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID"
             ,_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,
   promise) {
      return A2(onError,
      promise,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,
   promiseA) {
      return A2(andThen,
      promiseA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,
   promiseA,
   promiseB) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,
   promiseA,
   promiseB,
   promiseC) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return succeed(A3(func,
               a,
               b,
               c));
            });
         });
      });
   });
   var map4 = F5(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return succeed(A4(func,
                  a,
                  b,
                  c,
                  d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD,
   promiseE) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return A2(andThen,
                  promiseE,
                  function (e) {
                     return succeed(A5(func,
                     a,
                     b,
                     c,
                     d,
                     e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (promiseFunc,
   promiseValue) {
      return A2(andThen,
      promiseFunc,
      function (func) {
         return A2(andThen,
         promiseValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (promises) {
      return function () {
         switch (promises.ctor)
         {case "::": return A3(map2,
              F2(function (x,y) {
                 return A2($List._op["::"],
                 x,
                 y);
              }),
              promises._0,
              sequence(promises._1));
            case "[]":
            return succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 101 and 106");
      }();
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_v3) {
         return function () {
            return succeed($Maybe.Nothing);
         }();
      });
   };
   var fromMaybe = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return succeed(maybe._0);
            case "Nothing":
            return fail($default);}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return fail(result._0);
            case "Ok":
            return succeed(result._0);}
         _U.badCase($moduleName,
         "between lines 151 and 153");
      }();
   };
   var Task = {ctor: "Task"};
   _elm.Task.values = {_op: _op
                      ,succeed: succeed
                      ,fail: fail
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,andMap: andMap
                      ,sequence: sequence
                      ,andThen: andThen
                      ,onError: onError
                      ,mapError: mapError
                      ,toMaybe: toMaybe
                      ,fromMaybe: fromMaybe
                      ,toResult: toResult
                      ,fromResult: fromResult
                      ,spawn: spawn
                      ,sleep: sleep};
   return _elm.Task.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,link: link
                      ,style: style
                      ,defaultStyle: defaultStyle
                      ,typeface: typeface
                      ,monospace: monospace
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,Style: Style
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Signal.delay;
   var since = F2(function (time,
   signal) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,time,signal));
         var start = A2($Signal.map,
         $Basics.always(1),
         signal);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,
      targetFrames,
      $Signal.constant(true));
   };
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,timestamp: timestamp
                      ,delay: delay
                      ,since: since};
   return _elm.Time.values;
};
Elm.TopicData = Elm.TopicData || {};
Elm.TopicData.make = function (_elm) {
   "use strict";
   _elm.TopicData = _elm.TopicData || {};
   if (_elm.TopicData.values)
   return _elm.TopicData.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "TopicData",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Common = Elm.Common.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Http = Elm.Http.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm);
   var topicData = $Signal.mailbox($Result.Err("Loading..."));
   var calcBestTopics = function (data) {
      return function () {
         var bestTopics = A2($Dict.map,
         F2(function (k,v) {
            return $Common.argmax(v);
         }),
         data.tokenTopics);
         return _U.replace([["tokenMaxLikelyTopic"
                            ,bestTopics]],
         data);
      }();
   };
   var receivedData = function (data) {
      return function () {
         var data$ = calcBestTopics(data);
         return A2($Signal.send,
         topicData.address,
         $Result.Ok(data$));
      }();
   };
   var prefix = "http://susurrant-audio.github.io/demo/data/";
   var addVocab = F2(function (a,
   data) {
      return _U.replace([["vocab"
                         ,a]],
      data);
   });
   var addDocMetadata = F2(function (a,
   data) {
      return _U.replace([["docMetadata"
                         ,a]],
      data);
   });
   var addTopicTokens = F2(function (a,
   data) {
      return _U.replace([["topicTokens"
                         ,a]],
      data);
   });
   var addTokenTopics = F2(function (a,
   data) {
      return _U.replace([["tokenTopics"
                         ,a]],
      data);
   });
   var addDocTopics = F2(function (a,
   data) {
      return _U.replace([["docTopics"
                         ,a]],
      data);
   });
   var addTopicPrevalence = F2(function (a,
   data) {
      return _U.replace([["topicPrevalence"
                         ,a]],
      data);
   });
   var updateOrFail = F4(function (update,
   dec,
   resp,
   data) {
      return function () {
         var decoded = A2($Result.andThen,
         resp,
         $Json$Decode.decodeString(dec));
         return function () {
            switch (decoded.ctor)
            {case "Err":
               return $Result.Err(decoded._0);
               case "Ok":
               return $Result.Ok(A2(update,
                 decoded._0,
                 data));}
            _U.badCase($moduleName,
            "between lines 185 and 187");
         }();
      }();
   });
   var topicPct = F2(function (i,
   data) {
      return function () {
         var amt = $Maybe.withDefault(0.0)(A2($Array.get,
         i,
         data.topicPrevalence));
         return $Common.roundPct(amt);
      }();
   });
   var trackInfo = F2(function (data,
   track) {
      return $Maybe.withDefault($Model.noInfo(track))(A2($Dict.get,
      track,
      data.docMetadata));
   });
   var segToTrackId = function (seg) {
      return function () {
         var parts = A2($String.split,
         ".",
         seg);
         return $Maybe.withDefault("")($List.head(parts));
      }();
   };
   var getVector = F2(function (data,
   _v3) {
      return function () {
         switch (_v3.ctor)
         {case "_Tuple2":
            return function () {
                 var f = function (v) {
                    return {_: {}
                           ,id: _v3._0
                           ,prob: _v3._1
                           ,tokenType: $Model.tokenTypeOf(_v3._0)
                           ,values: v};
                 };
                 var vec = A2($Dict.get,
                 _v3._0,
                 data.vocab);
                 return A2($Maybe.map,f,vec);
              }();}
         _U.badCase($moduleName,
         "between lines 152 and 154");
      }();
   });
   var getTokenVectors = F2(function (data,
   tokens) {
      return A2($List.filterMap,
      getVector(data),
      tokens);
   });
   var topWordsForTopic = F2(function (topic,
   data) {
      return function () {
         var topWords = $Maybe.withDefault(_L.fromArray([]))(A2($Dict.get,
         topic,
         data.topicTokens));
         var sorted = $List.reverse(A2($List.sortBy,
         $Basics.snd,
         topWords));
         var nonzero = A2($List.filter,
         function (_v7) {
            return function () {
               switch (_v7.ctor)
               {case "_Tuple2":
                  return _U.cmp(_v7._1,
                    1.0e-2) > 0;}
               _U.badCase($moduleName,
               "on line 147, column 43 to 51");
            }();
         },
         sorted);
         return A2($List.take,
         10,
         nonzero);
      }();
   });
   var topicTokens = F2(function (topic,
   data) {
      return getTokenVectors(data)(A2(topWordsForTopic,
      topic,
      data));
   });
   var toXY = $Array.indexedMap(F2(function (i,
   y) {
      return {_: {},x: i,y: y};
   }));
   var getTopicsForDoc = F2(function (data,
   doc) {
      return A2($Result.fromMaybe,
      A2($Basics._op["++"],
      "Doc ",
      A2($Basics._op["++"],
      doc,
      "not found")),
      A2($Dict.get,
      doc,
      data.docTopics));
   });
   var trackTokenDec = A4($Json$Decode.tuple3,
   F3(function (v0,v1,v2) {
      return {ctor: "_Tuple3"
             ,_0: v0
             ,_1: v1
             ,_2: v2};
   }),
   $Json$Decode.maybe($Json$Decode.$int),
   $Json$Decode.$int,
   $Json$Decode.$int);
   var trackDataDec = function (trackID) {
      return A2($Json$Decode.map,
      function (xs) {
         return {ctor: "_Tuple2"
                ,_0: trackID
                ,_1: xs};
      },
      $Json$Decode.array(trackTokenDec));
   };
   var incrementOrAdd = F2(function (x,
   d) {
      return function () {
         var f = function (maybeV) {
            return function () {
               switch (maybeV.ctor)
               {case "Just":
                  return $Maybe.Just(maybeV._0 + 1);
                  case "Nothing":
                  return $Maybe.Just(1);}
               _U.badCase($moduleName,
               "between lines 84 and 87");
            }();
         };
         return A3($Dict.update,x,f,d);
      }();
   });
   var count = A2($List.foldl,
   incrementOrAdd,
   $Dict.empty);
   var mkProbList = function (xs) {
      return function () {
         var counts = count(xs);
         var countLst = $Dict.toList(counts);
         var n = $List.length(xs);
         var f = function (_v13) {
            return function () {
               switch (_v13.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: _v13._0
                         ,_1: $Basics.toFloat(_v13._1) / $Basics.toFloat(n)};}
               _U.badCase($moduleName,
               "on line 97, column 20 to 44");
            }();
         };
         return _U.eq(n,
         0) ? _L.fromArray([]) : A2($List.map,
         f,
         countLst);
      }();
   };
   var tokensToProbDist = F2(function (dtype,
   byDtypes) {
      return function () {
         var tokenStr = function (i) {
            return A2($Basics._op["++"],
            dtype,
            $Basics.toString(i));
         };
         var tokens = $Maybe.withDefault($Array.empty)(A2($Dict.get,
         dtype,
         byDtypes));
         var tokenStrs = $Array.toList(A2($Array.map,
         tokenStr,
         tokens));
         return mkProbList(tokenStrs);
      }();
   });
   var tokensToTagged = function (tokens) {
      return A2($Array.map,
      function (_v17) {
         return function () {
            switch (_v17.ctor)
            {case "_Tuple3":
               return A3($Model.TaggedToken,
                 _v17._0,
                 _v17._1,
                 _v17._2);}
            _U.badCase($moduleName,
            "on line 79, column 30 to 47");
         }();
      },
      tokens);
   };
   var tokensByDtype = F3(function (data,
   track,
   allTokens) {
      return function () {
         var tagged = tokensToTagged(allTokens);
         var get = F2(function (dtype,
         getter) {
            return {ctor: "_Tuple2"
                   ,_0: dtype
                   ,_1: A2($Array.map,
                   getter,
                   tagged)};
         });
         return $Dict.fromList(_L.fromArray([A2(get,
                                            "gfccs",
                                            function (_) {
                                               return _.gfcc;
                                            })
                                            ,A2(get,
                                            "beat_coefs",
                                            function ($) {
                                               return $Maybe.withDefault(-1)(function (_) {
                                                  return _.beat_coef;
                                               }($));
                                            })
                                            ,A2(get,
                                            "chroma",
                                            function (_) {
                                               return _.chroma;
                                            })]));
      }();
   });
   var tokenTopic = F3(function (data,
   dtype,
   dnum) {
      return function () {
         var tokName = A2($Basics._op["++"],
         dtype,
         $Basics.toString(dnum));
         return $Maybe.withDefault(-1)(A2($Dict.get,
         tokName,
         data.tokenMaxLikelyTopic));
      }();
   });
   var tokensToTopics = function (data) {
      return $Dict.map(F2(function (dtype,
      xs) {
         return A2($Array.map,
         A2(tokenTopic,data,dtype),
         xs);
      }));
   };
   var trackToTokenTopics = F2(function (data,
   _v22) {
      return function () {
         switch (_v22.ctor)
         {case "_Tuple2":
            return function () {
                 var f = function (xs) {
                    return A2($Array.map,
                    function (i) {
                       return {_: {}
                              ,x: i
                              ,y: 1.0 / $Basics.toFloat($Array.length(xs))};
                    },
                    xs);
                 };
                 var info = A2(trackInfo,
                 data,
                 _v22._0);
                 var byDtype = A3(tokensByDtype,
                 data,
                 _v22._0,
                 _v22._1);
                 var topicDict = A2(tokensToTopics,
                 data,
                 byDtype);
                 return A2($Dict.map,
                 F2(function (_v26,v) {
                    return function () {
                       return {_: {}
                              ,topics: f(v)
                              ,track: info};
                    }();
                 }),
                 topicDict);
              }();}
         _U.badCase($moduleName,
         "between lines 66 and 70");
      }();
   });
   var thresh = F4(function (topic,
   min,
   _v28,
   arr) {
      return function () {
         return _U.cmp(A2($Common.nth,
         topic,
         arr),
         min) > 0;
      }();
   });
   var topDocsForTopic = F2(function (topic,
   data) {
      return function () {
         var getInfo = function (t) {
            return A2(trackInfo,
            data,
            segToTrackId(t));
         };
         var aboveThresh = $Dict.toList(A2($Dict.filter,
         A2(thresh,topic,0.1),
         data.docTopics));
         var docs = A2($List.map,
         function (_v30) {
            return function () {
               switch (_v30.ctor)
               {case "_Tuple2": return {_: {}
                                       ,topics: toXY(_v30._1)
                                       ,track: getInfo(_v30._0)};}
               _U.badCase($moduleName,
               "on line 139, column 37 to 67");
            }();
         },
         aboveThresh);
         var topDocs = $List.reverse(A2($List.sortBy,
         function ($) {
            return $Common.nth(topic)($Array.map(function (_) {
               return _.y;
            })(function (_) {
               return _.topics;
            }($)));
         },
         docs));
         return A2($List.take,
         10,
         topDocs);
      }();
   });
   var unsafeToInt = function (s) {
      return function () {
         var _v34 = $String.toInt(s);
         switch (_v34.ctor)
         {case "Ok": return _v34._0;}
         _U.badCase($moduleName,
         "between lines 50 and 51");
      }();
   };
   var toIntDict = function ($) {
      return $Dict.fromList($List.map(function (_v36) {
         return function () {
            switch (_v36.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: unsafeToInt(_v36._0)
                      ,_1: _v36._1};}
            _U.badCase($moduleName,
            "on line 55, column 38 to 54");
         }();
      })($Dict.toList($)));
   };
   var topicTokenDec = $Json$Decode.map(toIntDict)($Json$Decode.dict($Json$Decode.keyValuePairs($Json$Decode.$float)));
   var topicDist = $Json$Decode.dict($Json$Decode.array($Json$Decode.$float));
   var trackInfoDec = A5($Json$Decode.object4,
   $Model.TrackInfo,
   $Json$Decode.map($Basics.toString)(A2($Json$Decode._op[":="],
   "id",
   $Json$Decode.$int)),
   A2($Json$Decode._op[":="],
   "title",
   $Json$Decode.string),
   A2($Json$Decode.at,
   _L.fromArray(["user"
                ,"username"]),
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "permalink_url",
   $Json$Decode.string));
   var loadData = A2($Task.andMap,
   A2($Task.andMap,
   A2($Task.andMap,
   A2($Task.andMap,
   A2($Task.andMap,
   A2($Task.andMap,
   A2($Task.map,
   $Model.Data,
   A2($Http.get,
   $Json$Decode.array($Json$Decode.$float),
   A2($Basics._op["++"],
   prefix,
   "topics.json"))),
   A2($Http.get,
   topicDist,
   A2($Basics._op["++"],
   prefix,
   "doc_topics.json"))),
   A2($Http.get,
   topicDist,
   A2($Basics._op["++"],
   prefix,
   "token_topics.json"))),
   A2($Http.get,
   topicTokenDec,
   A2($Basics._op["++"],
   prefix,
   "topic_tokens.json"))),
   A2($Http.get,
   $Json$Decode.dict(trackInfoDec),
   A2($Basics._op["++"],
   prefix,
   "doc_metadata.json"))),
   A2($Http.get,
   $Json$Decode.dict($Json$Decode.list($Json$Decode.$float)),
   A2($Basics._op["++"],
   prefix,
   "vocab.json"))),
   $Task.succeed($Dict.empty));
   var emptyData = A7($Model.Data,
   $Array.empty,
   $Dict.empty,
   $Dict.empty,
   $Dict.empty,
   $Dict.empty,
   $Dict.empty,
   $Dict.empty);
   var fromResults = function (results) {
      return function () {
         var updates = _L.fromArray([A2(updateOrFail,
                                    addTopicPrevalence,
                                    $Json$Decode.array($Json$Decode.$float))
                                    ,A2(updateOrFail,
                                    addDocTopics,
                                    topicDist)
                                    ,A2(updateOrFail,
                                    addTokenTopics,
                                    topicDist)
                                    ,A2(updateOrFail,
                                    addTopicTokens,
                                    topicTokenDec)
                                    ,A2(updateOrFail,
                                    addDocMetadata,
                                    $Json$Decode.dict(trackInfoDec))
                                    ,A2(updateOrFail,
                                    addVocab,
                                    $Json$Decode.dict($Json$Decode.list($Json$Decode.$float)))]);
         var updates$ = A3($List.map2,
         F2(function (x,y) {
            return y(x);
         }),
         results,
         updates);
         return A3($List.foldl,
         $Basics.flip($Result.andThen),
         $Result.Ok(emptyData),
         updates$);
      }();
   };
   var numTopics = function ($) {
      return $Array.length(function (_) {
         return _.topicPrevalence;
      }($));
   };
   var topicOrder = function (data) {
      return function () {
         var f = function (a) {
            return $Maybe.withDefault(0.0)(A2($Array.get,
            a,
            data.topicPrevalence));
         };
         return $List.reverse(A2($List.sortBy,
         f,
         _L.range(0,
         numTopics(data) - 1)));
      }();
   };
   _elm.TopicData.values = {_op: _op
                           ,numTopics: numTopics
                           ,emptyData: emptyData
                           ,trackInfoDec: trackInfoDec
                           ,topicDist: topicDist
                           ,unsafeToInt: unsafeToInt
                           ,toIntDict: toIntDict
                           ,topicTokenDec: topicTokenDec
                           ,thresh: thresh
                           ,trackToTokenTopics: trackToTokenTopics
                           ,tokenTopic: tokenTopic
                           ,tokensToTagged: tokensToTagged
                           ,incrementOrAdd: incrementOrAdd
                           ,count: count
                           ,mkProbList: mkProbList
                           ,tokensToProbDist: tokensToProbDist
                           ,trackTokenDec: trackTokenDec
                           ,trackDataDec: trackDataDec
                           ,tokensByDtype: tokensByDtype
                           ,tokensToTopics: tokensToTopics
                           ,getTopicsForDoc: getTopicsForDoc
                           ,toXY: toXY
                           ,topDocsForTopic: topDocsForTopic
                           ,topWordsForTopic: topWordsForTopic
                           ,getVector: getVector
                           ,getTokenVectors: getTokenVectors
                           ,topicTokens: topicTokens
                           ,segToTrackId: segToTrackId
                           ,trackInfo: trackInfo
                           ,topicPct: topicPct
                           ,topicOrder: topicOrder
                           ,updateOrFail: updateOrFail
                           ,addTopicPrevalence: addTopicPrevalence
                           ,addDocTopics: addDocTopics
                           ,addTokenTopics: addTokenTopics
                           ,addTopicTokens: addTopicTokens
                           ,addDocMetadata: addDocMetadata
                           ,addVocab: addVocab
                           ,fromResults: fromResults
                           ,prefix: prefix
                           ,calcBestTopics: calcBestTopics
                           ,loadData: loadData
                           ,topicData: topicData
                           ,receivedData: receivedData};
   return _elm.TopicData.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
Elm.Updates = Elm.Updates || {};
Elm.Updates.make = function (_elm) {
   "use strict";
   _elm.Updates = _elm.Updates || {};
   if (_elm.Updates.values)
   return _elm.Updates.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Updates",
   $History = Elm.History.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $OSC = Elm.OSC.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var doSoundUpdate = F2(function (up,
   set) {
      return function () {
         switch (up.ctor)
         {case "Noop": return set;
            case "Play":
            return A2($Set.insert,
              up._0,
              set);
            case "Stop":
            return A2($Set.remove,
              up._0,
              set);}
         _U.badCase($moduleName,
         "between lines 21 and 24");
      }();
   });
   var Noop = {ctor: "Noop"};
   var soundUpdates = $Signal.mailbox(Noop);
   var nowPlaying = A3($Signal.foldp,
   doSoundUpdate,
   $Set.empty,
   soundUpdates.signal);
   var oscMessages = function () {
      var f = function (x) {
         return function () {
            switch (x.ctor)
            {case "Noop":
               return $Maybe.Nothing;
               case "Play":
               return $Maybe.Just($OSC.toOsc(x._1));
               case "Stop":
               return $Maybe.Just($OSC.toOsc(x._1));}
            _U.badCase($moduleName,
            "between lines 31 and 35");
         }();
      };
      return A2($Signal.map,
      f,
      soundUpdates.signal);
   }();
   var Stop = F2(function (a,b) {
      return {ctor: "Stop"
             ,_0: a
             ,_1: b};
   });
   var Play = F2(function (a,b) {
      return {ctor: "Play"
             ,_0: a
             ,_1: b};
   });
   var soundUpdate = F3(function (soundID,
   playing,
   msg) {
      return playing ? A2(Play,
      soundID,
      msg) : A2(Stop,soundID,msg);
   });
   var actions = $Signal.mailbox($Task.succeed({ctor: "_Tuple0"}));
   var toPath = function (x) {
      return A2($Signal.send,
      actions.address,
      $History.setPath(x));
   };
   _elm.Updates.values = {_op: _op
                         ,actions: actions
                         ,toPath: toPath
                         ,Play: Play
                         ,Stop: Stop
                         ,Noop: Noop
                         ,soundUpdates: soundUpdates
                         ,doSoundUpdate: doSoundUpdate
                         ,nowPlaying: nowPlaying
                         ,oscMessages: oscMessages
                         ,soundUpdate: soundUpdate};
   return _elm.Updates.values;
};
Elm.View = Elm.View || {};
Elm.View.make = function (_elm) {
   "use strict";
   _elm.View = _elm.View || {};
   if (_elm.View.values)
   return _elm.View.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "View",
   $Array = Elm.Array.make(_elm),
   $Audio = Elm.Audio.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Bootstrap$Html = Elm.Bootstrap.Html.make(_elm),
   $Common = Elm.Common.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $OSC = Elm.OSC.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $TopicData = Elm.TopicData.make(_elm),
   $Updates = Elm.Updates.make(_elm),
   $Viz$Bars = Elm.Viz.Bars.make(_elm),
   $Viz$Common = Elm.Viz.Common.make(_elm),
   $Viz$Graph = Elm.Viz.Graph.make(_elm),
   $Viz$Ordinal = Elm.Viz.Ordinal.make(_elm),
   $Viz$Stars = Elm.Viz.Stars.make(_elm);
   var viewGraph = _L.fromArray([$Viz$Graph.graphView]);
   var playIcon = F4(function (soundId,
   playMsg,
   stopMsg,
   state) {
      return function () {
         var stopAct = A3($Updates.soundUpdate,
         soundId,
         false,
         stopMsg);
         var playAct = A3($Updates.soundUpdate,
         soundId,
         true,
         playMsg);
         var isPlaying = A2($Set.member,
         soundId,
         state.playing);
         var icon = isPlaying ? $Bootstrap$Html.glyphiconPause_ : $Bootstrap$Html.glyphiconPlay_;
         var action = isPlaying ? A2($Html$Events.onClick,
         $Updates.soundUpdates.address,
         playAct) : A2($Html$Events.onClick,
         $Updates.soundUpdates.address,
         stopAct);
         return A2($Html.div,
         _L.fromArray([action]),
         _L.fromArray([icon]));
      }();
   });
   var mkPlayIcon = F4(function (dtype,
   info,
   byDtypes,
   state) {
      return function () {
         var stopMsg = $OSC.StopTokens;
         var tokenProbs = A2($TopicData.tokensToProbDist,
         dtype,
         byDtypes);
         var playMsg = $OSC.PlayTokens(tokenProbs);
         var soundId = A2($Basics._op["++"],
         info.trackID,
         A2($Basics._op["++"],
         "/",
         dtype));
         return A4(playIcon,
         soundId,
         playMsg,
         stopMsg,
         state);
      }();
   });
   var viewDocTopicBar = F5(function (state,
   info,
   dtype,
   byDtypes,
   topicDict) {
      return function () {
         var playIcon$ = A4(mkPlayIcon,
         dtype,
         info,
         byDtypes,
         state);
         var mkTopics = function (xs) {
            return A2($Array.map,
            function (x) {
               return {_: {},x: x,y: 1.0};
            },
            xs);
         };
         var topics = A2($Array.slice,
         0,
         50)($Array.filter(function (x) {
            return !_U.eq(x,-1);
         })($Maybe.withDefault($Array.empty)(A2($Dict.get,
         dtype,
         topicDict))));
         var trackTopics = {_: {}
                           ,topics: mkTopics(topics)
                           ,track: info};
         return A2($Html.div,
         _L.fromArray([]),
         _L.fromArray([A2($Bootstrap$Html.colXs_,
                      2,
                      _L.fromArray([$Html.text(dtype)]))
                      ,A2($Bootstrap$Html.colXs_,
                      1,
                      _L.fromArray([playIcon$]))
                      ,A2($Bootstrap$Html.colXs_,
                      9,
                      _L.fromArray([A5($Viz$Bars.barDisplay,
                      _L.fromArray([]),
                      $Viz$Common.noMargin,
                      500,
                      36,
                      trackTopics)]))]));
      }();
   });
   var alertBase = F2(function (b,
   xs) {
      return function () {
         var icon = b ? $Bootstrap$Html.glyphiconExclamationSign_ : $Bootstrap$Html.glyphiconWarningSign_;
         return A2($Html.div,
         _L.fromArray([$Html$Attributes.classList(_L.fromArray([{ctor: "_Tuple2"
                                                                ,_0: "alert"
                                                                ,_1: true}
                                                               ,{ctor: "_Tuple2"
                                                                ,_0: "alert-danger"
                                                                ,_1: b}
                                                               ,{ctor: "_Tuple2"
                                                                ,_0: "alert-warning"
                                                                ,_1: $Basics.not(b)}]))]),
         A2($List._op["::"],
         icon,
         A2($List._op["::"],
         $Html.text(" "),
         xs)));
      }();
   });
   var alert = alertBase(true);
   var warning = alertBase(false);
   var showTrack = F2(function (data,
   mtd) {
      return function () {
         var trackViz = A2($Maybe.andThen,
         A2($Maybe.andThen,
         A2($Maybe.andThen,
         mtd,
         function ($) {
            return $Maybe.Just($TopicData.trackToTokenTopics(data)($));
         }),
         $Dict.get("gfccs")),
         function ($) {
            return $Maybe.Just($Html.text($Basics.toString($)));
         });
         return A2($Maybe.withDefault,
         $Html.text("Display failed"),
         trackViz);
      }();
   });
   var trackInfoFmt = function (inf) {
      return $Html.text(A2($Basics._op["++"],
      inf.username,
      A2($Basics._op["++"],
      " | ",
      inf.title)));
   };
   var showBar = function (trackTopics) {
      return function () {
         var trackID = trackTopics.track.trackID;
         return A2($Html.div,
         _L.fromArray([$Html$Attributes.$class("row track-row")
                      ,A2($Html$Events.onClick,
                      $Updates.actions.address,
                      $Updates.toPath(A2($Basics._op["++"],
                      "/track/",
                      trackID)))]),
         _L.fromArray([A2($Bootstrap$Html.colXs_,
                      9,
                      _L.fromArray([trackInfoFmt(trackTopics.track)]))
                      ,A2($Bootstrap$Html.colXs_,
                      3,
                      _L.fromArray([A5($Viz$Bars.verticalBarDisplay,
                      _L.fromArray([]),
                      $Viz$Common.noMargin,
                      100,
                      24,
                      trackTopics)]))]));
      }();
   };
   var viewDocData = F4(function (state,
   data,
   info,
   tokens) {
      return function () {
         var byDtypes = A3($TopicData.tokensByDtype,
         data,
         info.trackID,
         tokens);
         var topicDict = A2($TopicData.tokensToTopics,
         data,
         byDtypes);
         var info$ = trackInfoFmt(info);
         return _L.fromArray([A2($Html.h3,
                             _L.fromArray([]),
                             _L.fromArray([A2($Html.a,
                             _L.fromArray([$Html$Attributes.href(info.url)]),
                             _L.fromArray([info$]))]))
                             ,A5(viewDocTopicBar,
                             state,
                             info,
                             "gfccs",
                             byDtypes,
                             topicDict)
                             ,A5(viewDocTopicBar,
                             state,
                             info,
                             "beat_coefs",
                             byDtypes,
                             topicDict)
                             ,A5(viewDocTopicBar,
                             state,
                             info,
                             "chroma",
                             byDtypes,
                             topicDict)]);
      }();
   });
   var viewDoc = F4(function (doc,
   data,
   maybeTrack,
   state) {
      return function () {
         switch (maybeTrack.ctor)
         {case "Just":
            switch (maybeTrack._0.ctor)
              {case "_Tuple2":
                 return !_U.eq(maybeTrack._0._0,
                   doc) ? _L.fromArray([alert(_L.fromArray([$Html.text(A2($Basics._op["++"],
                   "Fetched ",
                   A2($Basics._op["++"],
                   maybeTrack._0._0,
                   A2($Basics._op["++"],
                   "; doesn\'t match ",
                   "doc"))))]))]) : A4(viewDocData,
                   state,
                   data,
                   A2($TopicData.trackInfo,
                   data,
                   maybeTrack._0._0),
                   maybeTrack._0._1);}
              break;
            case "Nothing":
            return _L.fromArray([$Html.text("Loading...")]);}
         _U.badCase($moduleName,
         "between lines 203 and 209");
      }();
   });
   var colorFor = function (i) {
      return $Viz$Ordinal.cat10(i);
   };
   var colorAttrFor = function (i) {
      return $Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                  ,_0: "color"
                                                  ,_1: colorFor(i)}]));
   };
   var viewTopicDocOverview = F3(function (data,
   state,
   topic) {
      return function () {
         var tokenDomains = $Viz$Stars.getTokenDomains(data);
         var starPlot = A4($Viz$Stars.mediumStar,
         colorFor(topic),
         _L.fromArray([A2($Html$Attributes.attribute,
         "class",
         "center-block")]),
         $Maybe.Just(tokenDomains),
         A2($TopicData.topicTokens,
         topic,
         data));
         return _L.fromArray([$Bootstrap$Html.row_(_L.fromArray([A2($Html.div,
                                                                _L.fromArray([A2($Html$Events.onClick,
                                                                             $Updates.actions.address,
                                                                             $Updates.toPath(A2($Basics._op["++"],
                                                                             "/topic/",
                                                                             $Basics.toString(topic))))
                                                                             ,A2($Html$Events.onMouseEnter,
                                                                             $Updates.actions.address,
                                                                             A2($Audio.playTopic,
                                                                             topic,
                                                                             data))
                                                                             ,A2($Html$Events.onMouseLeave,
                                                                             $Updates.actions.address,
                                                                             $Audio.stopTopic(topic))
                                                                             ,$Html$Attributes.$class("col-xs-3 topic-overview")]),
                                                                _L.fromArray([A2($Html.h2,
                                                                             _L.fromArray([colorAttrFor(topic)]),
                                                                             _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                                                          "Topic ",
                                                                                          A2($Basics._op["++"],
                                                                                          $Basics.toString(topic),
                                                                                          " ")))
                                                                                          ,A2($Html.br,
                                                                                          _L.fromArray([]),
                                                                                          _L.fromArray([]))
                                                                                          ,A2($Html.small,
                                                                                          _L.fromArray([]),
                                                                                          _L.fromArray([$Html.text(A2($TopicData.topicPct,
                                                                                          topic,
                                                                                          data))]))]))
                                                                             ,starPlot]))
                                                                ,A2($Bootstrap$Html.colXs_,
                                                                9,
                                                                A2($List.map,
                                                                showBar,
                                                                A2($TopicData.topDocsForTopic,
                                                                topic,
                                                                data)))]))
                             ,$Bootstrap$Html.row_(_L.fromArray([A2($Html.hr,
                             _L.fromArray([]),
                             _L.fromArray([]))]))]);
      }();
   });
   var viewTopicTokens = F2(function (data,
   topic) {
      return function () {
         var playPause = function (x) {
            return _L.fromArray([A2($Html$Events.onMouseEnter,
                                $Updates.actions.address,
                                A2($Audio.playToken,x,data))
                                ,A2($Html$Events.onMouseLeave,
                                $Updates.actions.address,
                                $Audio.stopToken(x))]);
         };
         var tokens = A2($TopicData.topicTokens,
         topic,
         data);
         var tokenDomains = $Viz$Stars.getDomain(tokens);
         var f = function (x) {
            return A2($Html.div,
            A2($Basics._op["++"],
            _L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                               ,_0: "float"
                                                               ,_1: "left"}
                                                              ,{ctor: "_Tuple2"
                                                               ,_0: "margin"
                                                               ,_1: "4px"}]))]),
            playPause(x)),
            _L.fromArray([A4($Viz$Stars.smallStar,
                         colorFor(topic),
                         _L.fromArray([]),
                         $Maybe.Just(tokenDomains),
                         _L.fromArray([x]))
                         ,A2($Html.br,
                         _L.fromArray([]),
                         _L.fromArray([]))
                         ,$Html.text(x.id)
                         ,A2($Html.div,
                         _L.fromArray([$Html$Attributes.$class("small")]),
                         _L.fromArray([$Html.text($Common.roundPct(x.prob))]))]));
         };
         return A2($List.map,f,tokens);
      }();
   });
   var viewTopic = F3(function (data,
   state,
   topic) {
      return A2($Basics._op["++"],
      A3(viewTopicDocOverview,
      data,
      state,
      topic),
      A2($Basics._op["++"],
      A2(viewTopicTokens,data,topic),
      _L.fromArray([A2($Html.br,
                   _L.fromArray([$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                                      ,_0: "clear"
                                                                      ,_1: "both"}]))]),
                   _L.fromArray([]))
                   ,alert(_L.fromArray([]))])));
   });
   var viewOverview = F2(function (model,
   state) {
      return function () {
         var f = function (data) {
            return A2($List.concatMap,
            A2(viewTopicDocOverview,
            data,
            state),
            $TopicData.topicOrder(data));
         };
         var output = A2($Result.map,
         f,
         model.data);
         return function () {
            switch (output.ctor)
            {case "Err":
               return _L.fromArray([$Html.text(output._0)]);
               case "Ok": return output._0;}
            _U.badCase($moduleName,
            "between lines 111 and 113");
         }();
      }();
   });
   var navbrand = A2($Html.a,
   _L.fromArray([$Html$Attributes.$class("navbar-brand")
                ,$Html$Attributes.href("/demo/index.html")]),
   _L.fromArray([$Html.text("Susurrant")]));
   var navheader = A2($Html.div,
   _L.fromArray([$Html$Attributes.$class("navbar-header")]),
   _L.fromArray([A2($Html.button,
                _L.fromArray([$Html$Attributes.$class("navbar-toggle collapsed")
                             ,A2($Html$Attributes.attribute,
                             "data-toggle",
                             "collapse")
                             ,A2($Html$Attributes.attribute,
                             "data-target",
                             "#collapsed")]),
                A2($Basics._op["++"],
                _L.fromArray([A2($Html.span,
                _L.fromArray([$Html$Attributes.$class("sr-only")]),
                _L.fromArray([$Html.text("Toggle navigation ")]))]),
                A2($List.repeat,
                3,
                A2($Html.span,
                _L.fromArray([$Html$Attributes.$class("icon-bar")]),
                _L.fromArray([])))))
                ,navbrand]));
   var aLink = F2(function (current,
   _v7) {
      return function () {
         return A2($Html.li,
         _L.fromArray([$Html$Attributes.classList(_L.fromArray([{ctor: "_Tuple2"
                                                                ,_0: "active"
                                                                ,_1: _U.eq(current,
                                                                _v7.path)}]))]),
         _L.fromArray([A2($Html.a,
         _L.fromArray([A2($Html$Events.onClick,
                      $Updates.actions.address,
                      $Updates.toPath(_v7.path))
                      ,$Html$Attributes.title(_v7.titleText)]),
         _L.fromArray([$Html.text(_v7.name)]))]));
      }();
   });
   var HeaderLink = F3(function (a,
   b,
   c) {
      return {_: {}
             ,name: a
             ,path: c
             ,titleText: b};
   });
   var navLinks = _L.fromArray([A3(HeaderLink,
                               "Overview",
                               "Topics and Top Tracks",
                               "/demo/index.html")
                               ,A3(HeaderLink,
                               "Social Graph",
                               "Tracks in social context",
                               "/graph")]);
   var navbar = function (currentPath) {
      return function () {
         var links = A2($List.map,
         aLink(currentPath),
         navLinks);
         return A2($Html.nav,
         _L.fromArray([$Html$Attributes.$class("navbar navbar-default navbar-fixed-top")]),
         _L.fromArray([$Bootstrap$Html.containerFluid_(_L.fromArray([navheader
                                                                    ,A2($Html.div,
                                                                    _L.fromArray([$Html$Attributes.$class("collapse navbar-collapse")
                                                                                 ,$Html$Attributes.id("collapsed")]),
                                                                    _L.fromArray([A2($Html.ul,
                                                                    _L.fromArray([$Html$Attributes.$class("nav navbar-nav")]),
                                                                    links)]))]))]));
      }();
   };
   var wrap = F2(function (state,
   xs) {
      return function () {
         var alerts = state.oscConnected ? _L.fromArray([]) : _L.fromArray([warning(_L.fromArray([$Html.text("OSC not connected")]))]);
         return $Bootstrap$Html.container_(A2($Basics._op["++"],
         _L.fromArray([navbar(state.currentPath)]),
         A2($Basics._op["++"],
         alerts,
         xs)));
      }();
   });
   _elm.View.values = {_op: _op
                      ,HeaderLink: HeaderLink
                      ,aLink: aLink
                      ,navLinks: navLinks
                      ,navbrand: navbrand
                      ,navheader: navheader
                      ,navbar: navbar
                      ,wrap: wrap
                      ,viewOverview: viewOverview
                      ,colorFor: colorFor
                      ,colorAttrFor: colorAttrFor
                      ,viewTopicDocOverview: viewTopicDocOverview
                      ,trackInfoFmt: trackInfoFmt
                      ,showBar: showBar
                      ,showTrack: showTrack
                      ,warning: warning
                      ,alert: alert
                      ,alertBase: alertBase
                      ,viewTopicTokens: viewTopicTokens
                      ,viewTopic: viewTopic
                      ,viewDoc: viewDoc
                      ,viewDocData: viewDocData
                      ,viewDocTopicBar: viewDocTopicBar
                      ,mkPlayIcon: mkPlayIcon
                      ,playIcon: playIcon
                      ,viewGraph: viewGraph};
   return _elm.View.values;
};
Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values)
   return _elm.VirtualDom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "VirtualDom",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var on = $Native$VirtualDom.on;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   _elm.VirtualDom.values = {_op: _op
                            ,Node: Node
                            ,node: node
                            ,text: text
                            ,toElement: toElement
                            ,fromElement: fromElement
                            ,Property: Property
                            ,property: property
                            ,attribute: attribute
                            ,on: on
                            ,lazy: lazy
                            ,lazy2: lazy2
                            ,lazy3: lazy3};
   return _elm.VirtualDom.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Bars = Elm.Viz.Bars || {};
Elm.Viz.Bars.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Bars = _elm.Viz.Bars || {};
   if (_elm.Viz.Bars.values)
   return _elm.Viz.Bars.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Bars",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Common = Elm.Common.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $Viz$Common = Elm.Viz.Common.make(_elm),
   $Viz$Ordinal = Elm.Viz.Ordinal.make(_elm),
   $Viz$Scale = Elm.Viz.Scale.make(_elm);
   var exampleData = function () {
      var mkTopics = function (xs) {
         return A2($Array.indexedMap,
         F2(function (i,x) {
            return {_: {},x: i,y: x};
         }),
         xs);
      };
      var f = function (xs) {
         return {_: {}
                ,topics: mkTopics($Array.fromList(xs))
                ,track: $Model.noInfo("")};
      };
      return f(_L.range(0,9));
   }();
   var verticalBars = F2(function (_v0,
   data) {
      return function () {
         return function () {
            var yMax = $Maybe.withDefault(0)($List.head($List.reverse(_v0.yS.range)));
            var bar = function (d) {
               return A2($Svg.rect,
               _L.fromArray([$Svg$Attributes.width($Basics.toString(A2($Viz$Scale.convert,
                            _v0.xS,
                            1.0)))
                            ,$Svg$Attributes.height($Basics.toString(A2($Viz$Scale.convert,
                            _v0.yS,
                            d.y)))
                            ,$Svg$Attributes.fill(_v0.cS(d.x))
                            ,$Svg$Attributes.x($Basics.toString(A2($Viz$Scale.convert,
                            _v0.xS,
                            $Basics.toFloat(d.x))))
                            ,$Svg$Attributes.y($Basics.toString(yMax - A2($Viz$Scale.convert,
                            _v0.yS,
                            d.y)))]),
               _L.fromArray([]));
            };
            return $Array.toList(A2($Array.map,
            bar,
            data));
         }();
      }();
   });
   var bars = F2(function (_v2,
   data) {
      return function () {
         return function () {
            var bar = function (d) {
               return A2($Svg.rect,
               _L.fromArray([$Svg$Attributes.width($Basics.toString(A2($Viz$Scale.convert,
                            _v2.xS,
                            d.y)))
                            ,$Svg$Attributes.height($Basics.toString(A2($Viz$Scale.convert,
                            _v2.yS,
                            1.0)))
                            ,$Svg$Attributes.fill(_v2.cS(d.x))
                            ,$Svg$Attributes.x($Basics.toString(A2($Viz$Scale.convert,
                            _v2.xS,
                            d.y0)))]),
               _L.fromArray([]));
            };
            return $Array.toList(A2($Array.map,
            bar,
            data));
         }();
      }();
   });
   var toXYT = function (_v4) {
      return function () {
         return A2($Array.map,
         function (d) {
            return _U.insert("track",
            _v4.track,
            d);
         },
         _v4.topics);
      }();
   };
   var toData = function (trackTopics) {
      return function () {
         var cumul = F2(function (_v6,
         acc) {
            return function () {
               return function () {
                  var y0 = $Maybe.withDefault(0.0)($Maybe.map(function (_) {
                     return _.y1;
                  })($Common.last(acc)));
                  return A2($Array.push,
                  {_: {}
                  ,trackInfo: _v6.track
                  ,x: _v6.x
                  ,y: _v6.y
                  ,y0: y0
                  ,y1: y0 + _v6.y},
                  acc);
               }();
            }();
         });
         return A3($Array.foldl,
         cumul,
         $Array.empty,
         toXYT(trackTopics));
      }();
   };
   var baseBarDisplay = F7(function (getDomains,
   v,
   attrs,
   margin,
   w,
   h,
   data) {
      return function () {
         var data$ = toData(data);
         var ds = A3($Viz$Common.dims,
         margin,
         w,
         h);
         var domains = getDomains(data.topics);
         return A4($Viz$Common.svgWithMargin,
         attrs,
         ds,
         margin,
         A2(v,
         A2($Viz$Common.scales,
         domains,
         ds),
         data$));
      }();
   });
   var colorScale = $Viz$Ordinal.color$($Viz$Ordinal.category10(_L.range(0,
   9)));
   var verticalDomains = function (arr) {
      return function () {
         var _ = $Viz$Common.extent($Array.toList(A2($Array.map,
         function (_) {
            return _.y;
         },
         arr)));
         var a = function () {
            switch (_.ctor)
            {case "::": switch (_._1.ctor)
                 {case "::":
                    switch (_._1._1.ctor)
                      {case "[]": return _._0;}
                      break;}
                 break;}
            _U.badCase($moduleName,
            "on line 37, column 18 to 58");
         }();
         var b = function () {
            switch (_.ctor)
            {case "::": switch (_._1.ctor)
                 {case "::":
                    switch (_._1._1.ctor)
                      {case "[]": return _._1._0;}
                      break;}
                 break;}
            _U.badCase($moduleName,
            "on line 37, column 18 to 58");
         }();
         var ys = _L.fromArray([a - 0.1
                               ,b]);
         return {_: {}
                ,cDomain: _L.range(0,9)
                ,xDomain: _L.fromArray([0.0
                                       ,9.0])
                ,yDomain: ys};
      }();
   };
   var verticalBarDisplay = A2(baseBarDisplay,
   verticalDomains,
   verticalBars);
   var main = A5(verticalBarDisplay,
   _L.fromArray([]),
   $Viz$Common.noMargin,
   64,
   100,
   exampleData);
   var horizontalDomains = function (arr) {
      return {_: {}
             ,cDomain: _L.range(0,9)
             ,xDomain: _L.fromArray([0
                                    ,$List.sum($Array.toList(A2($Array.map,
                                    function (_) {
                                       return _.y;
                                    },
                                    arr)))])
             ,yDomain: _L.fromArray([0.0
                                    ,1.0])};
   };
   var barDisplay = A2(baseBarDisplay,
   horizontalDomains,
   bars);
   var Datum = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,trackInfo: e
             ,x: a
             ,y: b
             ,y0: c
             ,y1: d};
   });
   _elm.Viz.Bars.values = {_op: _op
                          ,Datum: Datum
                          ,horizontalDomains: horizontalDomains
                          ,verticalDomains: verticalDomains
                          ,colorScale: colorScale
                          ,toXYT: toXYT
                          ,toData: toData
                          ,bars: bars
                          ,verticalBars: verticalBars
                          ,baseBarDisplay: baseBarDisplay
                          ,barDisplay: barDisplay
                          ,verticalBarDisplay: verticalBarDisplay
                          ,exampleData: exampleData
                          ,main: main};
   return _elm.Viz.Bars.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Common = Elm.Viz.Common || {};
Elm.Viz.Common.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Common = _elm.Viz.Common || {};
   if (_elm.Viz.Common.values)
   return _elm.Viz.Common.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Common",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Common = Elm.Common.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $Viz$Ordinal = Elm.Viz.Ordinal.make(_elm),
   $Viz$Scale = Elm.Viz.Scale.make(_elm);
   var htmlDims = F2(function (ds,
   ms) {
      return _L.fromArray([A2($Html$Attributes.attribute,
                          "height",
                          $Basics.toString($Basics.floor(ds.height + ms.top + ms.bottom)))
                          ,A2($Html$Attributes.attribute,
                          "width",
                          $Basics.toString($Basics.floor(ds.width + ms.left + ms.right)))]);
   });
   var colorStr = function (c) {
      return function () {
         var $ = $Color.toRgb(c),
         red = $.red,
         green = $.green,
         blue = $.blue,
         alpha = $.alpha;
         var rgb = A2($List.map,
         $Basics.toString,
         _L.fromArray([red,green,blue]));
         var val = $String.join(",")(A2($Basics._op["++"],
         rgb,
         _L.fromArray([$Basics.toString(alpha)])));
         return A2($Basics._op["++"],
         "rgba(",
         A2($Basics._op["++"],val,")"));
      }();
   };
   var translate = F2(function (x,
   y) {
      return A2($Basics._op["++"],
      "translate(",
      A2($Basics._op["++"],
      $Basics.toString(x),
      A2($Basics._op["++"],
      ",",
      A2($Basics._op["++"],
      $Basics.toString(y),
      ")"))));
   });
   var svgWithMargin = F4(function (attrs,
   ds,
   ms,
   xs) {
      return A2($Svg.svg,
      A2($Basics._op["++"],
      attrs,
      A2(htmlDims,ds,ms)),
      _L.fromArray([A2($Svg.g,
      _L.fromArray([$Svg$Attributes.transform(A2(translate,
      ms.left,
      ms.top))]),
      xs)]));
   });
   var center = F3(function (w,
   h,
   xs) {
      return _L.fromArray([A2($Svg.g,
      _L.fromArray([$Svg$Attributes.transform(A2(translate,
      w / 2.0,
      h / 2.0))]),
      xs)]);
   });
   var extent = function (lst) {
      return function () {
         var max = $List.maximum(lst);
         var min = $List.minimum(lst);
         return A2($Basics._op["++"],
         $Common.toList(min),
         $Common.toList(max));
      }();
   };
   var defaultDomains = {_: {}
                        ,cDomain: _L.range(0,9)
                        ,xDomain: _L.fromArray([0.0
                                               ,1.0])
                        ,yDomain: _L.fromArray([0.0
                                               ,1.0])};
   var scales = F2(function (_v0,
   _v1) {
      return function () {
         return function () {
            return function () {
               var cS = $Viz$Ordinal.color$($Viz$Ordinal.category10(_v0.cDomain));
               var yS = _U.replace([["domain"
                                    ,_v0.yDomain]
                                   ,["range"
                                    ,_L.fromArray([0,_v1.height])]],
               $Viz$Scale.linear);
               var xS = _U.replace([["domain"
                                    ,_v0.xDomain]
                                   ,["range"
                                    ,_L.fromArray([0,_v1.width])]],
               $Viz$Scale.linear);
               return {_: {}
                      ,cS: cS
                      ,xS: xS
                      ,yS: yS};
            }();
         }();
      }();
   });
   var dims = F3(function (margin,
   w,
   h) {
      return {_: {}
             ,height: h - margin.top - margin.bottom
             ,width: w - margin.left - margin.right};
   });
   var noMargin = {_: {}
                  ,bottom: 0
                  ,left: 0
                  ,right: 0
                  ,top: 0};
   var margin = {_: {}
                ,bottom: 10
                ,left: 10
                ,right: 10
                ,top: 10};
   var size = 500;
   var Scales = F3(function (a,
   b,
   c) {
      return {_: {}
             ,cS: c
             ,xS: a
             ,yS: b};
   });
   var Domains = F3(function (a,
   b,
   c) {
      return {_: {}
             ,cDomain: c
             ,xDomain: a
             ,yDomain: b};
   });
   var Margins = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: b
             ,right: c
             ,top: a};
   });
   var Dimensions = F2(function (a,
   b) {
      return {_: {}
             ,height: a
             ,width: b};
   });
   _elm.Viz.Common.values = {_op: _op
                            ,Dimensions: Dimensions
                            ,Margins: Margins
                            ,Domains: Domains
                            ,Scales: Scales
                            ,size: size
                            ,margin: margin
                            ,noMargin: noMargin
                            ,dims: dims
                            ,scales: scales
                            ,defaultDomains: defaultDomains
                            ,extent: extent
                            ,translate: translate
                            ,colorStr: colorStr
                            ,htmlDims: htmlDims
                            ,svgWithMargin: svgWithMargin
                            ,center: center};
   return _elm.Viz.Common.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Graph = Elm.Viz.Graph || {};
Elm.Viz.Graph.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Graph = _elm.Viz.Graph || {};
   if (_elm.Viz.Graph.values)
   return _elm.Viz.Graph.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Graph",
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm);
   var graphView = A2($Html.div,
   _L.fromArray([$Html$Attributes.id("graph")
                ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                      ,_0: "width"
                                                      ,_1: "960px"}
                                                     ,{ctor: "_Tuple2"
                                                      ,_0: "height"
                                                      ,_1: "500px"}]))]),
   _L.fromArray([]));
   _elm.Viz.Graph.values = {_op: _op
                           ,graphView: graphView};
   return _elm.Viz.Graph.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Ordinal = Elm.Viz.Ordinal || {};
Elm.Viz.Ordinal.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Ordinal = _elm.Viz.Ordinal || {};
   if (_elm.Viz.Ordinal.values)
   return _elm.Viz.Ordinal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Ordinal",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var category10colors = _L.fromArray(["#1f77b4"
                                       ,"#ff7f0e"
                                       ,"#2ca02c"
                                       ,"#d62728"
                                       ,"#9467bd"
                                       ,"#8c564b"
                                       ,"#e377c2"
                                       ,"#7f7f7f"
                                       ,"#bcbd22"
                                       ,"#17becf"]);
   var color$ = F2(function (cDict,
   c) {
      return $Maybe.withDefault("#000")(A2($Dict.get,
      c,
      cDict));
   });
   var colorDict = F2(function (xs,
   cs) {
      return $Dict.fromList(A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      xs,
      cs));
   });
   var category10 = function (domain) {
      return A2(colorDict,
      domain,
      category10colors);
   };
   var cat10 = color$(category10(_L.range(0,
   9)));
   _elm.Viz.Ordinal.values = {_op: _op
                             ,colorDict: colorDict
                             ,color$: color$
                             ,category10colors: category10colors
                             ,category10: category10
                             ,cat10: cat10};
   return _elm.Viz.Ordinal.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Scale = Elm.Viz.Scale || {};
Elm.Viz.Scale.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Scale = _elm.Viz.Scale || {};
   if (_elm.Viz.Scale.values)
   return _elm.Viz.Scale.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Scale",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm);
   var lerp = F2(function (range,
   x) {
      return function () {
         switch (range.ctor)
         {case "::":
            switch (range._1.ctor)
              {case "::":
                 switch (range._1._1.ctor)
                   {case "[]": return function () {
                           var dist = range._1._0 - range._0;
                           return range._0 + dist * x;
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 32 and 35");
      }();
   });
   var toUnitInterval = F2(function (domain,
   x) {
      return function () {
         switch (domain.ctor)
         {case "::":
            switch (domain._1.ctor)
              {case "::":
                 switch (domain._1._1.ctor)
                   {case "[]": return function () {
                           var dist = domain._1._0 - domain._0;
                           return (x - domain._0) / dist;
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 25 and 28");
      }();
   });
   var Scale = F3(function (a,
   b,
   c) {
      return {_: {}
             ,domain: a
             ,range: b
             ,scaleType: c};
   });
   var Log = function (a) {
      return {ctor: "Log",_0: a};
   };
   var logScale = {_: {}
                  ,domain: _L.fromArray([1.0e-4
                                        ,1.0])
                  ,range: _L.fromArray([0.0,1.0])
                  ,scaleType: Log(2.0)};
   var Linear = {ctor: "Linear"};
   var linear = {_: {}
                ,domain: _L.fromArray([0.0,1.0])
                ,range: _L.fromArray([0.0,1.0])
                ,scaleType: Linear};
   var logToLinearScale = F3(function (domain,
   range,
   base) {
      return {_: {}
             ,domain: A2($List.map,
             $Basics.logBase(base),
             domain)
             ,range: range
             ,scaleType: Linear};
   });
   var convert = F2(function (_v10,
   d) {
      return function () {
         return function () {
            var _v12 = _v10.scaleType;
            switch (_v12.ctor)
            {case "Linear":
               return function () {
                    var x = A2(toUnitInterval,
                    _v10.domain,
                    d);
                    return A2(lerp,_v10.range,x);
                 }();
               case "Log": return function () {
                    var lin = A3(logToLinearScale,
                    _v10.domain,
                    _v10.range,
                    _v12._0);
                    return A2(convert,
                    lin,
                    A2($Basics.logBase,_v12._0,d));
                 }();}
            _U.badCase($moduleName,
            "between lines 46 and 52");
         }();
      }();
   });
   _elm.Viz.Scale.values = {_op: _op
                           ,Linear: Linear
                           ,Log: Log
                           ,Scale: Scale
                           ,linear: linear
                           ,logScale: logScale
                           ,toUnitInterval: toUnitInterval
                           ,lerp: lerp
                           ,logToLinearScale: logToLinearScale
                           ,convert: convert};
   return _elm.Viz.Scale.values;
};
Elm.Viz = Elm.Viz || {};
Elm.Viz.Stars = Elm.Viz.Stars || {};
Elm.Viz.Stars.make = function (_elm) {
   "use strict";
   _elm.Viz = _elm.Viz || {};
   _elm.Viz.Stars = _elm.Viz.Stars || {};
   if (_elm.Viz.Stars.values)
   return _elm.Viz.Stars.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Viz.Stars",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $Viz$Common = Elm.Viz.Common.make(_elm),
   $Viz$Scale = Elm.Viz.Scale.make(_elm);
   var toData = $List.indexedMap(F2(function (i,
   xs) {
      return {_: {}
             ,id: $Basics.toString(i)
             ,prob: 1.0
             ,tokenType: $Model.Gfcc
             ,values: xs};
   }));
   var exampleData = toData(_L.fromArray([_L.fromArray([3
                                                       ,1
                                                       ,2
                                                       ,3
                                                       ,4
                                                       ,2])
                                         ,_L.fromArray([2,3,5,0,1,2])]));
   var defaultOpacity = _U.replace([["domain"
                                    ,_L.fromArray([1.0e-2,1.0])]
                                   ,["range"
                                    ,_L.fromArray([0.0,0.8])]],
   $Viz$Scale.logScale);
   var getTokenDomains = function (data) {
      return $Viz$Common.extent($List.concat($Dict.values(data.vocab)));
   };
   var getDomain = function ($) {
      return $Viz$Common.extent($List.concatMap(function (_) {
         return _.values;
      })($));
   };
   var floatToStr = function (x) {
      return _U.cmp($Basics.abs(x),
      1.0e-10) < 0 ? "0" : $Basics.toString(x);
   };
   var twoPi = $Basics.pi * 2.0;
   var addAngles = function (xs) {
      return function () {
         var l = $List.length(xs);
         var angle = twoPi / $Basics.toFloat(l);
         return A3($List.map2,
         F2(function (a,b) {
            return {ctor: "_Tuple2"
                   ,_0: a
                   ,_1: $Basics.toFloat(b) * angle};
         }),
         xs,
         _L.range(0,l - 1));
      }();
   };
   var halfPi = $Basics.pi * 0.5;
   var lineRadial0 = function (xs) {
      return function () {
         var point = function (_v0) {
            return function () {
               switch (_v0.ctor)
               {case "_Tuple2":
                  return function () {
                       var a$ = _v0._1 - halfPi;
                       return {ctor: "_Tuple2"
                              ,_0: _v0._0 * $Basics.cos(a$)
                              ,_1: _v0._0 * $Basics.sin(a$)};
                    }();}
               _U.badCase($moduleName,
               "between lines 26 and 27");
            }();
         };
         return A2($List.map,point,xs);
      }();
   };
   var lineRadial = function (xs) {
      return function () {
         var points = lineRadial0(xs);
         var pointsL = $String.join("L")(A2($List.map,
         function (_v4) {
            return function () {
               switch (_v4.ctor)
               {case "_Tuple2":
                  return A2($Basics._op["++"],
                    floatToStr(_v4._0),
                    A2($Basics._op["++"],
                    ",",
                    floatToStr(_v4._1)));}
               _U.badCase($moduleName,
               "on line 36, column 40 to 75");
            }();
         },
         points));
         return A2($Basics._op["++"],
         "M",
         A2($Basics._op["++"],
         pointsL,
         "Z"));
      }();
   };
   var star = F2(function (_v8,
   _v9) {
      return function () {
         return function () {
            return function () {
               var pathStr = lineRadial(addAngles($List.map($Viz$Scale.convert(_v8.rS))(_v9.values)));
               return A2($Svg.path,
               _L.fromArray([$Svg$Attributes.d(pathStr)
                            ,$Svg$Attributes.fill("none")
                            ,$Svg$Attributes.stroke(_v8.color)
                            ,$Svg$Attributes.strokeOpacity($Basics.toString(A2($Viz$Scale.convert,
                            _v8.opacity,
                            _v9.prob)))]),
               _L.fromArray([]));
            }();
         }();
      }();
   });
   var stars = F2(function (scales,
   lst) {
      return A2($List.map,
      star(scales),
      lst);
   });
   var starDisplay = F7(function (color,
   attrs,
   domain,
   margin,
   w,
   h,
   data) {
      return function () {
         var ds = A3($Viz$Common.dims,
         margin,
         w,
         h);
         var dataDomain = A2($Maybe.withDefault,
         getDomain(data),
         domain);
         var rS = _U.replace([["domain"
                              ,dataDomain]
                             ,["range"
                              ,_L.fromArray([0.0,w / 2.0])]],
         $Viz$Scale.linear);
         var stars$ = stars({_: {}
                            ,color: color
                            ,opacity: defaultOpacity
                            ,rS: rS});
         return A4($Viz$Common.svgWithMargin,
         attrs,
         ds,
         margin,
         A3($Viz$Common.center,
         w,
         h,
         stars$(data)));
      }();
   });
   var smallStar = F3(function (color,
   attrs,
   domain) {
      return A6(starDisplay,
      color,
      attrs,
      domain,
      {_: {}
      ,bottom: 4
      ,left: 4
      ,right: 4
      ,top: 4},
      64,
      64);
   });
   var main = A4(smallStar,
   "#000",
   _L.fromArray([]),
   $Maybe.Nothing,
   exampleData);
   var mediumStar = F3(function (color,
   attrs,
   domain) {
      return A6(starDisplay,
      color,
      attrs,
      $Maybe.Nothing,
      {_: {}
      ,bottom: 4
      ,left: 4
      ,right: 4
      ,top: 4},
      256,
      256);
   });
   var Scales = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: b
             ,opacity: c
             ,rS: a};
   });
   _elm.Viz.Stars.values = {_op: _op
                           ,Scales: Scales
                           ,halfPi: halfPi
                           ,twoPi: twoPi
                           ,lineRadial0: lineRadial0
                           ,floatToStr: floatToStr
                           ,lineRadial: lineRadial
                           ,addAngles: addAngles
                           ,star: star
                           ,stars: stars
                           ,getDomain: getDomain
                           ,getTokenDomains: getTokenDomains
                           ,defaultOpacity: defaultOpacity
                           ,starDisplay: starDisplay
                           ,smallStar: smallStar
                           ,mediumStar: mediumStar
                           ,toData: toData
                           ,exampleData: exampleData
                           ,main: main};
   return _elm.Viz.Stars.values;
};
