(defparameter *maestro-codes*
  '(
    33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
       56 57 58 59 60 61 62 63 64 65 66 67 69 70 71 72 73 74 75 76 77 78 79
       80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 101 102
       103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
       120 121 122 123 124 125 126 161 162 163 164 165 167 168 169 170 171
       172 174 175 176 177 180 181 182 183 186 187 191 192 193 194 197 198
       199 200 201 202 203 204 205 206 207 209 210 211 212 214 216 217 218
       219 220 223 224 225 226 227 228 229 230 231 233 234 235 236 237 241
       247 248 252 255))

(with-open-file (*standard-output*  "maestro.rtf"
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
  (loop
   :for code :in *maestro-codes*
   :initially
   (write-string "{\\rtf1\\ansi\\ansicpg1252\\cocoartf1187\\cocoasubrtf400
{\\fonttbl\\f0\\fnil\\fcharset0 AndaleMono;\\f1\\fnil\\fcharset0 Maestro;}
{\\colortbl;\\red255\\green255\\blue255;}
\\paperw11900\\paperh16840\\margl1440\\margr1440\\vieww13580\\viewh12780\\viewkind0
\\pard\\tx566\\tx1133\\tx1700\\tx2267\\tx2834\\tx3401\\tx3968\\tx4535\\tx5102\\tx5669\\tx6236\\tx6803\\pardirnatural

\\f0\\fs72 \\cf0 ")
   :do (format t "\\f0 ~3D: ~%\\f1 ~A\\~%"
               code
               (if (< code 127)
                 (string (code-char code))
                 (format nil "~(\\'~2,'0x~)" code))) 
   :finally
   (write-line "}")))





