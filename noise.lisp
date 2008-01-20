
(defparameter *noise* (make-array (512) :element-type '(unsigned-byte 8)))

(let ((permutation '(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 
                     142 8 99 37 240 21 10 23 190  6 148 247 120 234 75 0 26 197 62 94 252 219 
                     203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168  68 175 
                     74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 
                     105 92 41 55 46 245 40 244 102 143 54  65 25 63 161  1 216 80 73 209 76 132 
                     187 208  89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186  
                     3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 
                     227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152  2 44 154 163  
                     70 221 153 101 155 167  43 172 9 129 22 39 253  19 98 108 110 79 113 224 
                     232 178 185  112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 
                     162 241  81 51 145 235 249 14 239 107 49 192 214  31 181 199 106 157 184  
                     84 204 176 115 121 50 45 127  4 150 254 138 236 205 93 222 114 67 29 
                     24 72 243 141 128 195 78 66 215 61 156 180))
      (loop 
         for index from 0 below 256
         (do
          (setf (aref *noise* index) (nth index permutation))
          (setf (aref *noise* (+ 256 index)) (nth index permutation))))))

(defun fade (t)
  (*t  (* t (* t  (+  (*t  (-  (* t 6) 15)) 10)))))

(defun lerp (t a b)
  (+ a (* t (- b a))))

(defun grad (hash x y z)
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14) x z)))))
    (+ (if (zerop  (logand h 1))
           u -u)
       (if (zerop (logand h 2))
           v -v))))

(defun noise (x y z)
  (let* ((xint (logand (floor x) #X255))
         (yint (logand (floor y) #X255))
         (zint (logand (floor z) #X255))
         (xr (- x (floor x)))
         (yr (- y (floor y)))
         (zr (- z (floor z)))
         (u (fade xr))
         (w (fade yr))
         (v (fade zr))
         (A (+ (aref *noise* X) Y))
         (AA (+ (aref *noise* A) Z))
         (AB (+ (aref *noise* (1+ A)) Z))
         (B (+ (aref *noise* (1+ X)) Y))
         (BA (+ (aref *noise* B) Z))
         (BB (+ (aref *noise* (1+ B)) Z)))
    (lerp w 
          (lerp v 
                (lerp u
                      (grad (aref *noise* AA) xr yr zr)
                      (grad (aref *noise* BA) (1 + xr) yr zr))
                (lerp u
                      (grad (aref *noise* AB) xr (1- yr) zr)
                      (grad (aref *noise* BB) (1- xr) (1- yr) zr))) 
          (lerp v 
                (lerp u
                      (grad (aref *noise* (1+ AA)) xr yr (1- zr))
                      (grad (aref *noise* (1+ BA) (1- xr) (1- yr) (1- zr))))
                
                (lerp u
                      (grad (aref *noise* (1+ AB)) xr (1- yr) (1- zr))
                      (grad (aref *noise* (1+ BB) (1- xr) (1- yr) (1- zr))))))))