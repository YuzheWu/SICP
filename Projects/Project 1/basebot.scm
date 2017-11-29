;;; Project 1, 6.001, Spring 2005
;;; Solution by Yuzhe Wu, 28 Nov 2017

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    YOUR-CODE-HERE))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2

(define root1
  (lambda (a b c)
    (let ((delta (- (square b) (* 4 a c))))
      (if (or (= a 0) (< delta 0))
	  #f
	  (/ (+ (- b) (sqrt delta)) (* 2 a))))))

(define root2
  (lambda (a b c)
    (let ((delta (- (square b) (* 4 a c))))
      (if (or (= a 0) (< delta 0))
	  #f
	  (/ (- (- b) (sqrt delta)) (* 2 a))))))

;; complete these procedures and show some test cases

(root1 5 3 6)  ; -> #f
(root2 5 3 6)  ; -> #f
(root1 0 1 0)  ; -> #f
(root2 0 1 0)  ; -> #f
(root1 1 2 1)  ; -> -1
(root2 1 2 1)  ; -> -1
(root1 1 0 -9) ; -> 3
(root2 1 0 -9) ; -> -3
(root1 1 1 -1) ; -> 0.618
(root2 1 1 -1) ; -> -1.618


;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (- (/ gravity 2)) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

;; test cases

(time-to-impact 0 0)      ; -> 0
(time-to-impact 0 4.9)    ; -> 1
(time-to-impact 9.8 0)    ; -> 2
(time-to-impact -6.3 1)   ; -> 1/7
(time-to-height 0 2 2)    ; -> 0
(time-to-height -6.3 2 1) ; -> 1/7


;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (* velocity
	 (cos alpha)
	 (time-to-impact (* velocity (sin alpha))
			 elevation)))))

;; let's try this out for some example values.  Note that we are going to
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

(time-to-impact (* 45 (sin 0)) 1)         ; angle = 0 -> 0.452
(time-to-impact (* 45 (sin (/ pi 2))) 1)  ; angle = (/ pi 2) radians -> 9.21
(time-to-impact (* 45 (sin (/ pi 4))) 1)  ; angle = (/ pi 4) radians -> 6.53

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0)   ; angle = 0 -> 20.33 meters
(travel-distance-simple 1 45 90)  ; angle = 90 degrees -> 0 meters
(travel-distance-simple 1 45 45)  ; angle = 45 degrees -> 207.63 meters
(meters-to-feet (travel-distance-simple 1 45 0))   ; angle = 0 -> 67.09 feet
(meters-to-feet (travel-distance-simple 1 45 90))  ; angle = 90 degrees -> 0 feet
(meters-to-feet (travel-distance-simple 1 45 45))  ; angle = 45 degrees -> 685.17 feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)
(define degree-increment 1)

(define find-best-angle
  (lambda (velocity elevation)
    (define (distance angle)
      (travel-distance-simple elevation velocity angle))
    (define (iter angle best)
      (cond ((> angle 90) best)
	    ((> (distance angle) (distance best))
	     (iter (+ angle degree-increment) angle))
	    (else
	     (iter (+ angle degree-increment) best))))
    (iter 0 0)))

;; find best angle
;; try for other velocities
;; try for other heights

;; elevation = 1(m), velocity = 45(m/s)
(find-best-angle 45 1)   ; -> 45 degrees
;; elevation = 2(m), velocity = 45(m/s)
(find-best-angle 45 2)   ; -> 45 degrees
;; elevation = 2(m), velocity = 30(m/s)
(find-best-angle 30 2)   ; -> 44 degrees
;; elevation = 2(m), velocity = 10(m/s)
(find-best-angle 10 2)   ; -> 40 degrees
;; elevation = 2(m), velocity = 2(m/s)
(find-best-angle 2 2)    ; -> 17 degrees
;; elevation = 1(m), velocity = 1(m/s)
(find-best-angle 1 1)    ; -> 12 degrees
;; elevation = 100(m), velocity = 10(m/s)
(find-best-angle 10 100) ; -> 12 degrees
;; elevation = 1(m), velocity = 0.01(m/s)
(find-best-angle 0.01 1) ; -> 0

;; From the above test results, we find that the optimal angle of hitting
;; depends on the ratio of velocity squared to elevation. If this ratio is
;; above certain threshold value, the optimal angle equals 45 degrees. As
;; this ration drops below the threshold towards 0, the corresponding
;; optimal angle also decreases and reaches 0 eventually.


;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor
;; that can be computed

;; We would like to again compute distance , but taking into account
;; drag.
;; Basically we can rework the equations to get four coupled linear
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0)
	x0
	(let ((speed (sqrt (+ (square u0) (square v0)))))
	  (let ((a_u (* -1 (/ 1 m) beta speed u0))
		(a_v (- (+ (* (/ 1 m) speed v0 beta) g))))
	    (integrate (+ x0 (* u0 dt))
		       (+ y0 (* v0 dt))
		       (+ u0 (* a_u dt))
		       (+ v0 (* a_v dt))
		       dt
		       g
		       m
		       beta))))))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (let ((u0 (* velocity (cos alpha)))
	    (v0 (* velocity (sin alpha))))
	(integrate 0 elevation u0 v0 0.01 gravity mass beta)))))


;; RUN SOME TEST CASES

;; angle = 45 degrees, elevation = 1 m
(travel-distance 1 45 45)   ; v = 45 m/sec -> 92.51 m
(travel-distance 1 40 45)   ; v = 40 m/sec -> 82.78 m
(travel-distance 1 35 45)   ; v = 35 m/sec -> 71.58 m

;; v = 45 m/sec
(feet-to-meters 300)        ; 300 feet = 90.91 m
(travel-distance 1 45 49)   ; angle = 49 degrees -> 89.54 m
(travel-distance 1 45 48)   ; angle = 48 degrees -> 90.33 m
(travel-distance 1 45 47)   ; angle = 47 degrees -> 91.06 m
(travel-distance 1 45 45)   ; angle = 45 degrees -> 92.23 m
(travel-distance 1 45 40)   ; angle = 40 degrees -> 93.76 m
(travel-distance 1 45 35)   ; angle = 35 degrees -> 93.44 m
(travel-distance 1 45 31)   ; angle = 40 degrees -> 91.52 m
(travel-distance 1 45 30)   ; angle = 30 degrees -> 90.74 m
(travel-distance 1 45 29)   ; angle = 26 degrees -> 90.06 m

;; We observe from the above test results that the distance drops faster as
;; the hitting angle moves further away from the optimal, which lies between
;; 35 and 40 degrees with an initial swing of 45 m/sec at height 1 m. With
;; an angle between 30 and 47 will the ball land over the fence. At the edge
;; of this range the distance drops at roughly 0.7 m per degree of deviation.

;; what about Denver?

(define density 1.06)  ; air density at denver
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

;; we run the same tests and compare the results with those of Boston
(travel-distance 1 45 56)   ; angle = 49 degrees -> 89.16 m
(travel-distance 1 45 55)   ; angle = 48 degrees -> 90.49 m
(travel-distance 1 45 54)   ; angle = 48 degrees -> 91.85 m
(travel-distance 1 45 50)   ; angle = 47 degrees -> 96.22 m
(travel-distance 1 45 45)   ; angle = 45 degrees -> 99.83 m
(travel-distance 1 45 40)   ; angle = 40 degrees -> 101.33 m
(travel-distance 1 45 35)   ; angle = 35 degrees -> 100.53 m
(travel-distance 1 45 30)   ; angle = 30 degrees -> 97.57 m
(travel-distance 1 45 25)   ; angle = 26 degrees -> 91.81 m
(travel-distance 1 45 24)   ; angle = 48 degrees -> 90.29 m
(travel-distance 1 45 23)   ; angle = 48 degrees -> 88.89 m

;; In Denver, thanks to lower air density, the baseball could travel about
;; 10 meters more in distance compared to Boston with the same initial
;; conditions. The optimal hitting angle remains the same, but now a player
;; has more freedom in choosing the angle for the ball to land over the
;; fence, ranging from 25 to 55 degrees. Also, at the edge, the rate of
;; drop in distance with respect to change in angle becomes twice as large
;; as that in Boston.


;; Problem 7

;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to
;; use, given a velocity, in order to reach a given height (receiver) at a
;; given distance

;; time-traveled: given initial velocity, throwing angle and elevation,
;; computes the amount of time spent in the air while accounting for drag.

(define time-traveled
  (lambda (elevation velocity angle g m beta)
    (define dt 0.01)
    (define (iter t x0 y0 u0 v0 g m beta)
      (if (< y0 0)
	  t
	  (let ((speed (sqrt (+ (square u0) (square v0)))))
	    (let ((a_u (* -1 (/ 1 m) beta speed u0))
		  (a_v (- (+ (* (/ 1 m) speed v0 beta) g))))
	      (iter (+ t dt)
		    (+ x0 (* u0 dt))
		    (+ y0 (* v0 dt))
		    (+ u0 (* a_u dt))
		    (+ v0 (* a_v dt))
		    g
		    m
		    beta)))))
    (let ((alpha (degree2radian angle)))
      (let ((u0 (* velocity (cos alpha)))
	    (v0 (* velocity (sin alpha))))
	(iter 0 0 elevation u0 v0 g m beta)))))

;; optimal-angle: given initial velocity, elevation and desired distance,
;; computes the optimal angle for reaching the target the fastest possible;
;; assume typical air conditions in Boston.

(define optimal-angle
  (lambda (elevation velocity distance)
    (define inc-angle .1)
    (define (close-enough? value target)
      (< (abs (- value target)) 1))
    (define (iter angle best-angle best-time)
      (if (> angle 90)
	  best-angle
	  (let ((current-distance
		 (travel-distance elevation velocity angle))
		(current-time
		 (time-traveled elevation velocity angle gravity mass beta)))
	    (if (and (close-enough? current-distance distance)
		     (or (= best-time 0)
		       (< current-time best-time)))
		(iter (+ angle inc-angle) angle current-time)
		(iter (+ angle inc-angle) best-angle best-time)))))
    (iter -90 #f 0)))

;; shortest-time: travel time corresponding to the optimal angle

(define shortest-time
  (lambda (elevation velocity distance)
    (let ((best-angle
	   (optimal-angle elevation velocity distance)))
      (if best-angle
	  (time-traveled elevation velocity best-angle gravity mass beta)
	  0))))

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

(shortest-time 1 55 36)  ; v = 55 m/s -> 0.76 s
(shortest-time 1 45 36)  ; v = 55 m/s -> 0.93 s
(shortest-time 1 35 36)  ; v = 55 m/s -> 1.2 s

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft)
;; using 45m/s

(shortest-time 1 45 30)  ; d = 30 m -> 0.74 s
(shortest-time 1 45 60)  ; v = 60 m -> 1.79 s
(shortest-time 1 45 90)  ; v = 90 m -> 3.58 s

;; what about a weaker outfielder at 35m/sec?

(shortest-time 1 35 30)  ; d = 30 m -> 0.96 s
(shortest-time 1 35 60)  ; v = 60 m -> 2.44 s
(shortest-time 1 35 90)  ; v = 90 m -> 0 s (distance unattainable)


;; Problem 8

;; Notice in the last problem that a weaker outfielder cannot not make it to 90m
;; and may need to bounce the ball. We could model the bounce effect by assuming
;; that at every bounce, the reflected angle is the same as it was initially thown
;; and the velocity halves.

;; bounced-distance: given the initial velocity, elevation, throwing angle and
;; number of bounces, computes the distance traveled upon completing last bounce.

(define bounced-distance-simple
  (lambda (elevation velocity angle nb-bounces)
    (define (iter count velocity result)
      (if (> count nb-bounces)
	  result
	  (iter (+ count 1)
		(/ velocity 2)
		(+ result
		   (travel-distance 0 velocity angle)))))
    (iter 1
	  (/ velocity 2)
	  (travel-distance elevation velocity angle))))

;; bounced-distance-total: total distance traveled when allowing arbitrary number
;; of bounces until the ball stops

(define bounced-distance-total-simple
  (lambda (elevation velocity angle)
    (define (small-enough? delta) (< delta 0.1))
    (define (iter velocity result)
      (let ((delta (travel-distance 0 velocity angle)))
	(if (small-enough? delta)
	    result
	    (iter (/ velocity 2) (+ result delta)))))
    (iter (/ velocity 2)
	  (travel-distance elevation velocity angle))))

;; show how far a fielder can throw a ball on one bounce, on two bounces, on an
;; arbitrary number of bounces until it stops moving, with an initial velocity of
;; 35m/s and a throwing angle of 30 degrees at height 1m.

(bounced-distance-simple 1 35 30 1)     ; 1 bounce -> 90.91 m
(bounced-distance-simple 1 35 30 2)     ; 2 boucnes -> 97.52 m
(bounced-distance-total-simple 1 35 30) ; limit case -> 99.82 m

;; try a different initial velocity of 55m/s

(bounced-distance-simple 1 55 30 1)     ; 1 bounce -> 159.26 m
(bounced-distance-simple 1 55 30 2)     ; 2 boucnes -> 174.50 m
(bounced-distance-total-simple 1 55 30) ; limit case -> 180.03 m

;; try a different angle of 45

(bounced-distance 1 35 45 1)-simple     ; 1 bounce -> 96.08 m
(bounced-distance 1 35 45 2)-simple     ; 2 boucnes -> 103.57 m
(bounced-distance-total 1 35 45)-simple ; limit case -> 106.17 m


;; Problem 9

;; The approximations made in the previous problem, although not unrealistic
;; altogether, is a rather rough one. Note that from the integration process, we
;; actually know the velocity of the ball when it bounces, which enables us to
;; refine our computation.

;; a refined version of the bouncing model using the velocity at bounce

(define bounced-distance
  (lambda (elevation velocity angle nb-bounces)
    (define dt 0.01)
    (let ((alpha (degree2radian angle)))
      (define (integrate count x0 y0 u0 v0)
	(let ((speed (sqrt (+ (square u0) (square v0)))))
	  (if (< y0 0)
	      (if (= count nb-bounces)
		  x0
		  (let ((u1 (* (/ speed 2) (cos alpha)))
			(v1 (* (/ speed 2) (sin alpha))))
		    (integrate (+ count 1) x0 0 u1 v1)))
	      (Let ((a_u (* -1 (/ 1 mass) beta speed u0))
		    (a_v (- (+ (* (/ 1 mass) speed v0 beta) gravity))))
		(integrate count
			   (+ x0 (* u0 dt))
			   (+ y0 (* v0 dt))
			   (+ u0 (* a_u dt))
			   (+ v0 (* a_v dt)))))))
      (let ((u0 (* velocity (cos alpha)))
	    (v0 (* velocity (sin alpha))))
	(integrate 0 0 elevation u0 v0)))))

(define bounced-distance-total
  (lambda (elevation velocity angle)
    (define dt 0.01)
    (define (small-enough? delta) (< delta 0.1))
    (let ((alpha (degree2radian angle)))
      (define (integrate x0 y0 u0 v0)
	(let ((speed (sqrt (+ (square u0) (square v0)))))
	  (if (< y0 0)
	      (if (small-enough? (travel-distance 0 (/ speed 2) angle))
		  x0
		  (let ((u1 (* (/ speed 2) (cos alpha)))
			(v1 (* (/ speed 2) (sin alpha))))
		    (integrate x0 0 u1 v1)))
	      (let ((a_u (* -1 (/ 1 mass) beta speed u0))
		    (a_v (- (+ (* (/ 1 mass) speed v0 beta) gravity))))
		(integrate (+ x0 (* u0 dt))
			   (+ y0 (* v0 dt))
			   (+ u0 (* a_u dt))
			 (+ v0 (* a_v dt)))))))
      (Let ((u0 (* velocity (cos alpha)))
	    (v0 (* velocity (sin alpha))))
	(integrate 0  elevation u0 v0)))))

;; rerun test cases in the problem 8

;; show how far a fielder can throw a ball on one bounce, on two bounces, on an
;; arbitrary number of bounces until it stops moving, with an initial velocity of
;; 35m/s and a throwing angle of 30 degrees at height 1m.

(bounced-distance 1 35 30 1)     ; 1 bounce -> 76.87 m
(bounced-distance 1 35 30 2)     ; 2 boucnes -> 79.02 m
(bounced-distance-total 1 35 30) ; limit case -> 79.72 m

;; try a different initial velocity of 55m/s

(bounced-distance 1 55 30 1)     ; 1 bounce -> 123.16 m
(bounced-distance 1 55 30 2)     ; 2 boucnes -> 125.81 m
(bounced-distance-total 1 55 30) ; limit case -> 126.68 m

;; try a different angle of 45

(bounced-distance 1 35 45 1)     ; 1 bounce -> 81.51 m
(bounced-distance 1 35 45 2)     ; 2 boucnes -> 84.13 m
(bounced-distance-total 1 35 45) ; limit case -> 84.99 m

;; Compared to the previous simplistic model, the refined version computes shorter
;; distances due to bounces. This is because now we take lag into account.
