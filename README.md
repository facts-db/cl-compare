cl-compare
==========

Compare anything
----------------

```common-lisp
(compare 'a 'b)
=> -1

(compare 'a "a")
=> -1

(compare '(a b 1) '(a b 2))
=> -1
```

---

Your own order
--------------

```common-lisp
(defstruct fruit weight price)

(defmethod compare ((a fruit) (b fruit))
  (compare (* (fruit-price a) (fruit-weight a))
           (* (fruit-price b) (fruit-weight b))))

(let ((apple (make-fruit :price 0.3 :weight 100))
      (orange (make-fruit :price 4.5 :weight 1000)))
  (compare apple orange))

=> -1
```

---

Contribute
----------

This project welcomes COMPARE methods for types that have an
[atomic type specifier][1], with an emphasis on speed and functional
correctness.

[1]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm

To submit contributions, just fork the project and submit a pull
request.
