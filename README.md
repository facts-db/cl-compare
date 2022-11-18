cl-compare
==========

Compare anything
----------------

(compare 'a 'b)
=> -1

(compare 'a "a")
=> -1

(compare '(a b 1) '(a b 2))
=> -1

Your own order
--------------

(defstruct fruit weight price)

(defmethod compare ((a fruit) (b fruit))
  (compare (/ (fruit-price a) (fruit-weight a))
  	       (/ (fruit-price b) (fruit-weight b))))

(let ((apple (make-fruit :price 0.3 :weight 100))
      (orange (make-fruit :price 4.5 :weight 1000)))
  (compare apple orange))

=> -1

Contribute
----------

This project welcomes COMPARE methods for types that have an
[atomic type specifier][1], with an emphasis on speed and functional
correctness.

[1]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm

To submit contributions, just fork the project and submit a pull
request.

Licence
-------

cl-compare - generic comparison functions

Copyright 2022 Thomas de Grivel <thodg@kmx.io>

Permission is hereby granted to use this software granted
the above copyright notice and this permission paragraph
are included in all copies and substantial portions of this
software.

THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
THIS SOFTWARE.
