open Base

type t =
  { top_left : Filled_square.t
  ; top_right : Filled_square.t
  ; bottom_left : Filled_square.t
  ; bottom_right : Filled_square.t
  }

let create () =
  { top_left = Filled_square.create (Color.random ())
  ; top_right = Filled_square.create (Color.random ())
  ; bottom_left = Filled_square.create (Color.random ())
  ; bottom_right = Filled_square.create (Color.random ())
  }
;; 

let rotate_left t =
  (* TODO : rotate the piece to the left *)
  { top_left = t.top_right
  ; top_right = t.bottom_right
  ; bottom_right = t.bottom_left
  ; bottom_left = t.top_left }
;;

let rotate_right t =
  (* TODO : rotate the piece to the right *)
  { top_left = t.bottom_left
  ; top_right = t.top_left
  ; bottom_right = t.top_right
  ; bottom_left = t.bottom_right }
;;

let coords ~bottom_left:{ Point.col; row } : Point.t list =
  [ { col; row }
  ; { col = col + 1; row }
  ; { col; row = row + 1 }
  ; { col = col + 1; row = row + 1 }
  ]
;;

let equal t1 t2 = 
  Filled_square.equal t1.top_left t2.top_left
  && Filled_square.equal t1.top_right t2.top_right
  && Filled_square.equal t1.bottom_left t2.bottom_left
  && Filled_square.equal t1.bottom_right t2.bottom_right
;;

(* Tests *)
let%test "Testing Rotate Right..." = 
  let piece = 
    { top_left = Filled_square.create (Color.Orange)
    ; top_right = Filled_square.create (Color.White)
    ; bottom_left = Filled_square.create (Color.White)
    ; bottom_right = Filled_square.create (Color.White)
    }
  in
  let rotated = 
    { top_left = Filled_square.create (Color.White)
    ; top_right = Filled_square.create (Color.Orange)
    ; bottom_left = Filled_square.create (Color.White)
    ; bottom_right = Filled_square.create (Color.White)
    }
  in
  equal (rotate_right piece) rotated
;;

let%test "Testing Rotate Left..." = 
  let piece = 
    { top_left = Filled_square.create (Color.Orange)
    ; top_right = Filled_square.create (Color.White)
    ; bottom_left = Filled_square.create (Color.White)
    ; bottom_right = Filled_square.create (Color.White)
    }
  in
  let rotated = 
    { top_left = Filled_square.create (Color.White)
    ; top_right = Filled_square.create (Color.White)
    ; bottom_left = Filled_square.create (Color.Orange)
    ; bottom_right = Filled_square.create (Color.White)
    }
  in
  equal (rotate_left piece) rotated
;;
