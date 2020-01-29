\w*$
[ \t]+$
/[a-zA-Z]/
[^A-Za-z0-9]
^[^\\\/\?\*\"\>\<\:\|]*$
&( )
[0-9]+
[A-Za-z]+
^[\w]{1,}$
^[0-9]*[02468]$
^[a-zA-Z0-9\040]+$
^[a-zA-Z0-9\s.\-_']+$
^[ a - z, 0 - 9 , ?   -   ?   ,?   -   ? , ?    -  ?   ,?   -  ? , . ]
^[0-9]{2}
([^\\"]|\\.)*
^3[234689][0-9]$
^[http://www.|www.][\S]+$
^((([A-PR-UWYZ])([0-9][0-9A-HJKS-UW]?))|(([A-PR-UWYZ][A-HK-Y])([0-9][0-9ABEHMNPRV-Y]?))\s{0,2}(([0-9])([ABD-HJLNP-UW-Z])([ABD-HJLNP-UW-Z])))|(((GI)(R))\s{0,2}((0)(A)(A)))$
^"[^"]+"$
(^\d*\.\d{2}$)
^\d*((\.\d+)?)*$
^(0?[1-9]|1[012])$
([0-9]|[0-9][0-9])\.\s
^[a-zA-Z]{1}[\w\sa-zA-Z\d_]*[^\s]$
\d+(/\d+)?
^-?\d*(\.\d+)?$
/\/\*[\d\D]*?\*\//
<([^\s>]*)(\s[^<]*)>
^\d[0-9]*[-/]\d[0-9]*$
^[$]?[0-9]*(\.)?[0-9]?[0-9]?$
^(([0-9]{5})|([0-9]{3}[ ]{0,1}[0-9]{2}))$
^(([a-zA-Z][a-zA-Z_$0-9]*(\.[a-zA-Z][a-zA-Z_$0-9]*)*)\.)?([a-zA-Z][a-zA-Z_$0-9]*)$
^[0-9]{6}$
((\\")|[^"(\\")])+
^[-+]?\d+(\.\d{2})?$
^(\d|-)?(\d|,)*\.?\d*$
^\$[0-9]+(\.[0-9][0-9])?$
^([1-9]\d*|0)(([.,]\d*[1-9])?)$
(^\*\.[a-zA-Z][a-zA-Z][a-zA-Z]$)|(^\*\.\*$)
\d+,?\d+\$?
url=\"([^\[\]\"]*)\"
((0[1-9])|(1[02]))/\d{2}
^#?(([a-fA-F0-9]{3}){1,2})$
^[D-d][K-k]-[1-9]{1}[0-9]{3}$
/#([1-9]){2}([1-9]){2}([1-9]){2}/
^[A-Za-z]\d[A-Za-z][ -]?\d[A-Za-z]\d$
^~/[0-9a-zA-Z_][0-9a-zA-Z/_-]*\.[0-9a-zA-Z_-]+$
(0|(\+)?([1-9]{1}[0-9]{0,1}|[1]{1}[0-9]{0,2}|[2]{1}([0-4]{1}[0-9]{1}|[5]{1}[0-5]{1})))
\w+@\w+\.\w+
^[SC]{2}[0-9]{6}$
^\d{1,5}(\.\d{1,2})?$
(")([0-9]*)(",")([0-9]*)("\))
^((0?[1-9])|((1|2)[0-9])|30|31)$
^(\d{1,2})(\s?(H|h)?)(:([0-5]\d))?$
(^\d*\.?\d*[0-9]+\d*$)|(^[0-9]+\d*\.\d*$)
^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$
ICON="[^"]+"
^[A-Z]{1,3}\d{6}$
/(<\/?)(\w+)([^>]*>)/e
^\s*[a-zA-Z\s]+\,[0-9\s]+\s*$
^([0-9]|[1-9]\d|[1-7]\d{2}|800)$
^(((0[0-9])|(1[0-9])|(2[0-3])):[0-5][0-9])$
^(([0-9])|([0-1][0-9])|([2][0-3])):(([0-9])|([0-5][0-9]))$
\d{3}-\d{6}
^[2-9]{2}[0-9]{8}$
<\/?(tag1|tag2)[^>]*\/?>
^\d{4}\/\d{1,2}\/\d{1,2}$
^[+-]?\d+(\,\d{3})*\.?\d*\%?$
^([9]{1})([234789]{1})([0-9]{8})$
^([1-9]|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])$
^([0-1]?[0-9]|[2][0-3]):([0-5][0-9]):([0-5][0-9])$
(^(((\d)|(\d\d)|(\d\d\d))(\xA0|\x20))*((\d)|(\d\d)|(\d\d\d))([,.]\d*)?$)
^07[789]-\d{7}$
[0-9]{5}\s?[0-9]{5}
^\d{5}((\-|\s)?\d{4})?$
^[0-9]{6}-[0-9pPtTfF][0-9]{3}$
^[a-zA-Z0-9._\-]+@[a-z0-9\-]+(\.[a-z]+){1,}$
^(100([\.\,]0{1,2})?)|(\d{1,2}[\.\,]\d{1,2})|(\d{0,2})$
^(([1-9]\d{0,2}(\,\d{3})*|([1-9]\d*))(\.\d{2})?)|([0]\.(([0][1-9])|([1-9]\d)))$
(CY-?)?[0-9]{8}[A-Z]
^[2-9]\d{2}-\d{3}-\d{4}$
^\$\d{1,3}(,?\d{3})*(\.\d{2})?$
^(1[0-2]|0?[1-9]):([0-5]?[0-9])( AM| PM)$
^((1[01])|(\d)):[0-5]\d(:[0-5]\d)?\s?([apAP][Mm])?$
^[+]447\d{9}$
[0-9]{3}P[A-Z][0-9]{7}[0-9X]
(\d{1,3}[\.]\d*)[, ]+-?(\d{1,3}[\.]\d*)
^(-?)(,?)(\d{1,3}(\.\d{3})*|(\d+))(\,\d{2})?$
^(\$)?((\d{1,5})|(\d{1,3})(\,\d{3})*)(\.\d{1,2})?$
^(^N[BLSTU]$)|(^[AMN]B$)|(^[BQ]C$)|(^ON$)|(^PE$)|(^SK$)$
^(0{0,1}[1-9]|[12][0-9]|3[01])[- /.](0{0,1}[1-9]|1[012])[- /.](\d{2}|\d{4})$
/^[0-9]\d{2,4}-\d{6,8}$/
^([(][1-9]{2}[)] )?[0-9]{4}[-]?[0-9]{4}$
^(([\w][\w\-\.]*)\.)?([\w][\w\-]+)(\.([\w][\w\.]*))?$
^(([1-9])|(0[1-9])|(1[0-2]))\/((0[1-9])|([1-31]))\/((\d{2})|(\d{4}))$
(NL-?)?[0-9]{9}B[0-9]{2}
^[0-9+]{5}-[0-9+]{7}-[0-9]{1}$
^(\d)(\.)(\d)+\s(x)\s(10)(e|E|\^)(-)?(\d)+$
([0-9]+:)?[0-9]+\s*(am|pm)|[0-9]+:[0-9]+\s*(am|pm)?
^(0?[1-9]|1[0-2])(\:)([0-5][0-9])(\:)([0-5][0-9]) (AM|PM)$
