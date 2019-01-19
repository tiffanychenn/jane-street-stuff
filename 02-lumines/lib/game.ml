open Base

type t =
  { board : Board.t
  ; height : int
  ; width : int
  ; mutable moving_piece :
      Moving_piece.t
  (* we will choose the bottom left corner to be the block we refer to the piece by *)
  ; mutable moving_piece_col : int
  ; mutable moving_piece_row : int
  ; game_over : bool ref
  ; sweeper : Sweeper.t
  }

let create ~height ~width ~seconds_per_sweep =
  let board = Board.create ~height ~width in
  { board
  ; height
  ; width
  ; moving_piece = Moving_piece.create ()
  ; moving_piece_col = (width - 1) / 2
  ; moving_piece_row = height
  ; game_over = ref false
  ; sweeper = Sweeper.create board ~seconds_per_sweep
  }
;;

let new_moving_piece t =
  t.moving_piece <- Moving_piece.create ();
  t.moving_piece_col <- (t.width - 1) / 2;
  t.moving_piece_row <- t.height
;;

let can_move t ~row ~col =
  (* TODO: Check if moving the [moving_piece] so that the bottom left
     corner is at [row] [col] will cause the board to be invalid
     either because the piece will collide with a filled-in square on
     the board or because it runs off the board *)
  if (row < 0 || col < 0 || row >= t.board.height + 1 || col + 1 >= t.board.width) then false
  else if row = t.board.height then true
  else (
    let space = Board.get t.board {col = col; row = row} in 
    match space with 
    | None -> true
    | _ -> false
  )
;;

let move_left t =
  if can_move t ~row:t.moving_piece_row ~col:(t.moving_piece_col - 1) then t.moving_piece_col <- t.moving_piece_col - 1
  else ()
;;

let move_right t =
  (* TODO: Move the active piece right one square *)
  if can_move t ~row:t.moving_piece_row ~col:(t.moving_piece_col + 1) then t.moving_piece_col <- t.moving_piece_col + 1
  else ()
;;

let rotate_right t = t.moving_piece <- Moving_piece.rotate_right t.moving_piece
let rotate_left t = t.moving_piece <- Moving_piece.rotate_left t.moving_piece

let drop t =
  (* TODO: drop the active piece all the way to to bottom and add it
     to the board. We should also check if the game is over. If the
     game is not over, make sure to generate a new moving piece. *)
  if Board.add_piece t.board ~moving_piece:t.moving_piece ~col:t.moving_piece_col then (
    if not game_over then new_moving_piece t
    else t.moving_piece <- None
  ) 
  else ()
;;

let tick t =
  (* TODO: handle a single 1 second clock tick. The moving piece
     should try to move down one square. If it can't, we should try to
     add it to the board, checking if the game is over and marking
     squares to sweep if appropriate. *)
  if t.moving_piece_row > 0 then (
    let space = Board.get t.board {col = t.moving_piece_col; row = t.moving_piece_row - 1} in
    match space with
    | None -> t.moving_piece_row <- t.moving_piece_row - 1
    | _ -> (
      if Board.add_piece t.board ~moving_piece:t.moving_piece ~col:t.moving_piece_col then () else ()
    )
  )
  else ()
;;


(* Tests *)

let test_piece = 
  { Moving_piece. top_left = Filled_square.create (Color.Orange)
  ; top_right = Filled_square.create (Color.White)
  ; bottom_left = Filled_square.create (Color.White)
  ; bottom_right = Filled_square.create (Color.White)
  }
;;

let%test "Test can_move edges..." = 
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  (can_move t ~row:4 ~col:0)
  && can_move t ~row:4 ~col:1
  && can_move t ~row:4 ~col:2
  && not (can_move t ~row:4 ~col:3)
  && can_move t ~row:3 ~col:0
  && can_move t ~row:3 ~col:1
  && can_move t ~row:3 ~col:2
  && not (can_move t ~row:3 ~col:3)
  && can_move t ~row:2 ~col:0
  && can_move t ~row:2 ~col:1
  && can_move t ~row:2 ~col:2
  && not (can_move t ~row:2 ~col:3)
  && can_move t ~row:1 ~col:0
  && can_move t ~row:1 ~col:1
  && can_move t ~row:1 ~col:2
  && not (can_move t ~row:1 ~col:3)
  && can_move t ~row:0 ~col:0
  && can_move t ~row:0 ~col:1
  && can_move t ~row:0 ~col:2
  && not (can_move t ~row:0 ~col:3)
  && not (can_move t ~row:(-1) ~col:0)
  && not (can_move t ~row:(-1) ~col:1)
  && not (can_move t ~row:(-1) ~col:2)
  && not (can_move t ~row:(-1) ~col:3)
;;

let%test "Test can_move collisions..." = 
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  ignore (Board.add_piece t.board ~moving_piece:test_piece ~col:0);
  can_move t ~row:4 ~col:0
  && can_move t ~row:4 ~col:1
  && can_move t ~row:4 ~col:2
  && not (can_move t ~row:4 ~col:3)
  && can_move t ~row:3 ~col:0
  && can_move t ~row:3 ~col:1
  && can_move t ~row:3 ~col:2
  && not (can_move t ~row:3 ~col:3)
  && can_move t ~row:2 ~col:0
  && can_move t ~row:2 ~col:1
  && can_move t ~row:2 ~col:2
  && not (can_move t ~row:2 ~col:3)
  && not (can_move t ~row:1 ~col:0)
  && not (can_move t ~row:1 ~col:1)
  && can_move t ~row:1 ~col:2
  && not (can_move t ~row:1 ~col:3)
  && not (can_move t ~row:0 ~col:0)
  && not (can_move t ~row:0 ~col:1)
  && can_move t ~row:0 ~col:2
  && not (can_move t ~row:0 ~col:3)
  && not (can_move t ~row:(-1) ~col:0)
  && not (can_move t ~row:(-1) ~col:1)
  && not (can_move t ~row:(-1) ~col:2)
  && not (can_move t ~row:(-1) ~col:3)
;;

let%test "Test move_left..." =
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  move_left t;
  assert (t.moving_piece_col = 0);
  move_left t; 
  (t.moving_piece_col = 0)
;;

let%test "Test move_right..." =
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  move_right t;
  assert (t.moving_piece_col = 2);
  move_right t;
  (t.moving_piece_col = 2)
;;

let%test "Test drop..." =
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  drop t;
  not (Board.is_empty t.board { Point. row = 1; col =1})
  && not (Board.is_empty t.board { Point. row = 1; col =2})
  && not (Board.is_empty t.board { Point. row = 0; col =1})
  && not (Board.is_empty t.board { Point. row = 0; col =2})
  && t.moving_piece_col = 1
  && t.moving_piece_row = 4
;;

let%test "Test tick freeze 1..." =
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  tick t;
  assert (t.moving_piece_row = 3);
  tick t;
  assert (t.moving_piece_row = 2);
  tick t;
  assert (t.moving_piece_row = 1);
  tick t;
  assert (t.moving_piece_row = 0);
  tick t;
  not (Board.is_empty t.board { Point. row = 1; col =1})
  && not (Board.is_empty t.board { Point. row = 1; col =2})
  && not (Board.is_empty t.board { Point. row = 0; col =1})
  && not (Board.is_empty t.board { Point. row = 0; col =2})
  && (t.moving_piece_row = 4);
;;

let%test "Test tick game over..." =
  let t = create ~height:4 ~width:4 ~seconds_per_sweep:4. in
  List.range ~start:`inclusive ~stop:`exclusive 0 9
  |> List.iter ~f:(fun _ -> 
      assert (not !(t.game_over));
      tick t);
  !(t.game_over)
;;
