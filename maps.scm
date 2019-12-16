(require-extension persistent-hash-map)

(define merge-strategy
  (make-parameter
   (lambda (a b) b)))

(define (merge m1 m2)
  (map-reduce
   (lambda (key value m1)
       (if (map-contains? m1 key)
	   (map-add m1 key
		    ((merge-strategy)
		     (map-ref m1 key)
		     value))
	   (map-add m1 key value)))
   m1
   m2))

(define ((merge-maps default-merge) a b)
  (if (and (map? a) (map? b))
      (merge a b)
      (default-merge a b)))

(define (throw-together a b)
  (cond ((and (list? a) (list? b))
	 `(,@b ,@a))
	((list? a)
	 `(,b . ,a))
	((list? b)
	 `(,@b ,a))
	(else
	 `(,b ,a))))

(define (join a b)
 (parameterize ((merge-strategy
		  (merge-maps
		   throw-together)))
    (merge a b)))

(define (map-ref* m . keys)
  (map-ref-in m keys))

(define (map-update* m f . keys)
  (map-update-in m keys f))

(define (map-add* m x . keys)
  (map-update-in m keys (lambda _ x)))

(define ~ persistent-map)
