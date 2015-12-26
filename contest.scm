;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <"CompSci ain't for trees">
;;;
;;; Description:
;;;   <'Twas a bear and tree
;;;    fought they did in outerspace
;;;    Go Bears master race!>

; ===== Turtle Movement ======
(define (move-right x)
	(setheading 90)
	(forward x))

; ==== Utilities =====
(define (at_index s i)
	(if (= i 0)
		(car s)
		(at_index (cdr s) (- i 1))
	)
)

(define (len s)
	(define (helper s i)
		(if (null? s) 
			i
			(helper (cdr s) (+ i 1))
		)
	)
	(helper s 0)
)


; ==== Variables ====
(define window_width (screen_width)) ;so they can be treated as variables instead of calling the functions everytime
(define window_height (screen_height))


(define (escape_depth px py)
	(define x_normalization 1.8)
	(define y_normalization 1)

	(define x_scaled (- (/
			(*	(- 1 (- x_normalization))
				(- px (/ (- window_width) 2)))
			(-	(/ window_width 2)
				(/ (- window_width) 2))
		) x_normalization)) ; x coordinated normalized to mandelbrot domain
	(define y_scaled (- (/
			(*	(- 1 (- y_normalization))
				(- py (/ (- window_height) 2)))
			(-	(/ window_height 2)
				(/ (- window_height) 2))
		) y_normalization)) ; y coordinated normalized to mandelbrot domain
	

	(define (for_iter depth max_depth x y x_temp)
		(cond
			((and (< depth max_depth) 
			(< [+ (* x x) (* y y)] 4))
				(define x_temp (+ (- (* x x) (* y y)) x_scaled))
				(define y (+ (* 2 x y) y_scaled))
				(define x x_temp)
				(for_iter (+ depth 1) max_depth x y x_temp)
			)
			(else depth)
		)
	)

	(for_iter 0 5000 0 0 0)
)

(define (depth_row x y)
	(define (iter i window_width row x)
		(cond ((< i window_width)
				(define row (append row (list (escape_depth x y))))
				(iter (+ i 1) window_width row (+ x 1))
			)
			(else row)
		)
	)

	(iter 0 window_width '() x)
)

(define (render_mandelbrot)
	(define x_0 (/ (- window_width) 2))
	(define y_0 (/ window_height 2))
	(define x x_0)
	(define y y_0)

	(define (iter y y_0)
		(cond
			((> y (- y_0))
				(define row (depth_row x y))

				(define (iter_2 i pallet)
					(cond ((< i (len row))
							(define j i)
							(define (iter_3 j)
								(if (and [< j (len row)] [= (at_index row j) (at_index row i)])
									(iter_3 (+ j 1))
									j
								)
							)
							(define j (iter_3 j))
							(define pallet (append pallet [list (list (at_index row i) (- j i))]))
							(if (> j i)
								(define i j)
								(define i (+ i 1))
							)
							(iter_2 i pallet)
						)
						(else pallet)
					)
				)
				(define pallet (iter_2 0 '()))
				(define (for_iter i pallet)
					(cond
						((< i (len pallet))
							(define offset 
								(* 255
									(- 1
										(/
											(log (+ [* (/ [at_index (at_index pallet i) 0] 1000) 255] 1))
											(log 256)
										)
									)
								)
							)
							(define offset (quotient offset 1)) ; truncation
							(define r (- 255 offset))
							(cond ((> r 255)
									(define r 230)
									(define g 230)
									(define b 230)
								)
								(else
									(define r 0)
									(define g 0)
									(define b (- 255 offset))
								)
							)
							(color (rgb (/ r 255) (/ g 255) (/ b 255)))
							(pendown)
							(move-right (at_index (at_index pallet i) 1))
							(penup)
							(for_iter (+ i 1) pallet)
						)
						(else nil)
					)
				)

				(for_iter 0 pallet)

				(define y (- y 1))
				(setposition x y)
				(iter y y_0)
			)
		)
		(else nil)
	)

	(iter y y_0)
)

(speed 0)
(penup)
(setposition (/ (- window_width) 2) (/ window_height 2))
(pendown)

(define (draw)
	(render_mandelbrot)
	(setposition (/ window_width 13) (/ window_height 45))
	(addshape)
	(shape) 
	(stamp)
  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)