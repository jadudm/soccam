;;TWO INSTRUCTIONS

(add-instruction ldtimer
  ;;Comes back as a fixnum (30 bits w/ 2 for sign)
  (stack! (current-milliseconds) a b))
      
(add-instruction tin
  ;;This is interruptable on the transputer. We don't have this. Yet.
  ;; Ever. Possibly. Probably.
  (let ([current-time (current-milliseconds)]
	[reschedule-time (get 'areg)])

    ;;If our reschedule is in the past, we've already timed out
    (if (AFTER? current-time reschedule-time)
	;;We go on. Don't do stuff now.
	(void)
	;;Otherwise, we need to do stuff.
	(begin
	  ;;we need to insert ourselves in the proper place.
	  ;; This is an ordered queue.
	  ;;Store our reschedule time 
	  (workspace! wptr timeout reschedule-time)
	  ;;Store the iptr
	  (workspace! wptr iptr (get 'iptr))
	  
	  (traverse-and-insert (get 'tptr) (get 'tptr))
	  (run-next-on-queue) ))
    (stack! undef undef undef)))
