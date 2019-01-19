open Base

(* REMEMBER BOTTOM LEFT!!! *)

type t =
  { board : Filled_square.t option array array
  ; height : int
  ; width : int
  }

let create ~height ~width =
  { board = Array.make_matrix ~dimx:width ~dimy:height None; height; width }
;;

let get t { Point.col; row } = t.board.(col).(row)
let set t { Point.col; row } value = t.board.(col).(row) <- value

let mark_squares_that_are_sweepable t =
  (* TODO: at the end of this function the all filled_squares that are
     part of completed squares (anything that is in a single color
     square of 4 parts which includes combined groups) should be in
     sweeper state [To_sweep] and all other squares should be
     [Unmarked] *)
  for i = 0 to t.height - 2 do
    for j = 0 to t.width - 2 do
      let bottom_left = get t {col = j; row = i} in
      let bottom_right = get t {col = j + 1; row = i} in
      let top_left = get t {col = j; row = i + 1} in
      let top_right = get t {col = j + 1; row = i + 1} in
      match bottom_left, bottom_right, top_right, top_left with 
      | None, _, _, _
      | _, None, _, _
      | _, _, None, _
      | _, _, _, None -> ()
      | Some bottom_left, Some bottom_right, Some top_right, Some top_left -> (
        if (Color.equal bottom_right.color bottom_left.color && Color.equal bottom_right.color top_right.color && Color.equal bottom_right.color top_left.color) then (
          Filled_square.to_sweep bottom_right;
          Filled_square.to_sweep bottom_left;
          Filled_square.to_sweep top_right;
          Filled_square.to_sweep top_left
        )
        else if not (Filled_square.Sweeper_state.equal bottom_left.sweeper_state Filled_square.Sweeper_state.To_sweep) then Filled_square.unmark bottom_left
        else ()
      )
    done
  done
;;

let rec find_empty_row t col row =
  if row = t.height then -1 else (
    let space = get t {col = col; row = row} in
    match space with 
    | None -> row
    | _ -> find_empty_row t col (row + 1)
  )
;;

let move_square_down t row col = 
  let r = find_empty_row t col 0 in
  if r > -1 && not (r = row) then (
    set t {col = col; row = r} (get t {col = col; row = row});
    set t {col = col; row = row} None
  )
  else ()
;;

let remove_squares t =
  (* TODO: remove any squares marked as [Swept] from the board.
     Gravity should be applied appropriately.

     At the end of this function, we should call
     [mark_squares_that_are_sweepable] so that we ensure that we leave
     the board in a valid state.  *)
  for i = 0 to t.height - 1 do
    for j = 0 to t.width - 1 do
      let space = get t {col = j; row = i} in
      match space with 
      | None -> ()
      | Some square -> (
        if Filled_square.Sweeper_state.equal square.sweeper_state Filled_square.Sweeper_state.Swept then set t {col = j; row = i} None
        else move_square_down t i j
      )
    done;
  done;
  mark_squares_that_are_sweepable t
;;

let add_piece t ~(moving_piece:Moving_piece.t) ~col =
  (* TODO: insert the moving piece into the board, applying gravity
     appropriately. Make sure to leave the board in a valid state. *)
  let row = find_empty_row t col 0 in
  if row > -1 then (
    set t {col = col; row = row} (Some moving_piece.bottom_left);
    set t {col = col; row = row + 1} (Some moving_piece.top_left);
    set t {col = col + 1; row = row} (Some moving_piece.bottom_right);
    set t {col = col + 1; row = row + 1} (Some moving_piece.top_right);
    true
  )
  else false
;;

let is_empty t point =
  match get t point with
  | None -> true
  | Some _ -> false
;;

(* Tests *)
let is_filled_with_color t ~row ~col color = 
  match get t { Point. row; col} with
  | None -> false
  | Some square -> Color.equal color square.color
;;

let is_marked t ~row ~col = 
  match get t { Point. row; col} with
  | None -> false
  | Some square ->
    Filled_square.Sweeper_state.equal
      square.Filled_square.sweeper_state
      Filled_square.Sweeper_state.To_sweep
;;

let test_piece = 
  { Moving_piece. top_left = Filled_square.create (Color.Orange)
  ; top_right = Filled_square.create (Color.White)
  ; bottom_left = Filled_square.create (Color.White)
  ; bottom_right = Filled_square.create (Color.White)
  }
;;

let%test "Testing Add_piece add one..." = 
  let t = create ~height:4 ~width:4 in
  add_piece t ~moving_piece:test_piece ~col:0
  && is_filled_with_color t ~row:0 ~col:0 Color.White
  && is_filled_with_color t ~row:0 ~col:1 Color.White
  && is_filled_with_color t ~row:1 ~col:0 Color.Orange
  && is_filled_with_color t ~row:1 ~col:1 Color.White
;;

let%test "Testing Add_piece add many..." =
  let t = create ~height:4 ~width:4 in
  (add_piece t ~moving_piece:test_piece ~col:0)
  && (add_piece t ~moving_piece:test_piece ~col:0)
  && (not (add_piece t ~moving_piece:test_piece ~col:0))
;;

let test_removable_piece = 
  { Moving_piece. top_left = Filled_square.create (Color.White)
  ; top_right = Filled_square.create (Color.White)
  ; bottom_left = Filled_square.create (Color.White)
  ; bottom_right = Filled_square.create (Color.White)
  }
;;

let%test "Testing mark_squares_that_are_sweepable..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece  t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece  t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  is_marked t ~row:0 ~col:0 
  && is_marked t ~row:0 ~col:1
  && is_marked t ~row:1 ~col:0
  && is_marked t ~row:1 ~col:1
  && is_marked t ~row:2 ~col:0
  && is_marked t ~row:2 ~col:1
  && not (is_marked t ~row:3 ~col:0)
  && not (is_marked t ~row:3 ~col:1)
;;

let sweep_board t = 
  Array.iter t.board
    ~f:(fun row ->
        Array.iter row ~f:(fun square -> 
            Option.iter square
              ~f:(fun square -> ignore (Filled_square.sweep square))))
;;

let%test "Testing Remove_squares..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece  t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece  t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  sweep_board t;
  remove_squares t;
  is_filled_with_color t ~row:0 ~col:0 Color.Orange
  && is_filled_with_color t ~row:0 ~col:1 Color.White
;;
