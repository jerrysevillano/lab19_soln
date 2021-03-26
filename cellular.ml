(*
                             CS51 Lab 19
                   2-dimensional cellular automata
 *)
(* 
                               SOLUTION
 *)

module G = Graphics ;;

let cFONT = "-adobe-times-bold-r-normal--34-240-100-100-p-177-iso8859-9" ;;

(***********************************************************************
    Do not change either of the two module type signatures in this
    file. Doing so will likely cause your code to not to compile
    against our unit tests. 
***********************************************************************)
     
(*......................................................................
  Specifying automata 

  A cellular automaton (CA) is a square grid of cells each in a
  specific state. The grid is destructively updated according to an
  update rule. The evolving state of the CA can be visualized by
  displaying the grid in a graphics window.

  In this implementation, a particular kind of automaton, with its
  state space and update function, is specified by a module satisfying
  the `AUT_SPEC` signature. *)
   
module type AUT_SPEC =
  sig
    (* Automaton parameters *)
    type state                 (* the space of possible cell states *)
    val initial : state        (* the initial (default) cell state *)
    val grid_size : int        (* width and height of grid in cells *)
    val update : (state array array) -> int -> int -> state
                               (* update grid i j -- returns the new
                                  state for the cell at position `i, j`
                                  in `grid` *)
    (* Rendering parameters *)
    val name : string          (* a display name for the automaton *)
    val side_size : int        (* width and height of cells in pixels *)
    val cell_color : state -> G.color
                               (* color for each state *)
    val legend_color : G.color (* color to render legend *)
    val render_frequency : int (* how frequently grid is rendered (in ticks) *)
  end ;;

(*......................................................................
  Automata functionality

  Implementations of cellular automata provides for the following
  functionality:

  * Creating a grid, which can then be updated by the receiver to form
    the initial grid of a trial.

  * Initializing the graphics window in preparation for rendering.

  * Mapping an update function simultaneously over all cells of a grid.

  * Running the automaton using a particular update function, and
    rendering it as it evolves. 

as codified in the `AUTOMATON` signature. *)
  
module type AUTOMATON =
  sig
    (* state -- Possible states that a grid cell can be in *)
    type state
    (* grid -- 2D grid of cell states *)
    type grid = state array array
    (* create_grid () -- Returns a grid with all cells in initial
       states. *)
    val create_grid : unit -> grid
    (* graphics_init () -- Initialize the graphics window to the
       correct size and other parameters. Auto-synchronizing is off,
       so our code is responsible for flushing to the screen upon
       rendering. *)
    val graphics_init : unit -> unit
    (* step_grid grid -- Updates the `grid` by updating each cell
       simultaneously as per the CA's update rule. *)
    val step_grid : grid -> unit
    (* run_grid grid update -- Starts up the automaton on the provided
       initial `grid` using the `update` rule, rendering to the
       graphics window until a key is pressed. The `update` rule is a
       function operating as per `map_grid`. Assumes graphics has been
       initialized. *)
    val run_grid : grid -> unit
  end ;;

(*......................................................................
  Implementing automata based on a specification

  Given an automata specification (satisfying `AUT_SPEC`), the
  `Automaton` functor delivers a module satisfying `AUTOMATON` that
  implements the specified automaton. *)

module Automaton (Spec : AUT_SPEC)
       : (AUTOMATON with type state = Spec.state) =
  struct
    type state = Spec.state
    type grid = Spec.state array array

    (* create_grid -- See module type documentation *)
    let create_grid () : grid =
      Array.make_matrix Spec.grid_size Spec.grid_size Spec.initial

    (* graphics_init -- See module type documentation *)
    let graphics_init () : unit =
      G.open_graph ""; 
      G.resize_window (Spec.grid_size * Spec.side_size)
                      (Spec.grid_size * Spec.side_size);
      G.set_font cFONT;
      G.auto_synchronize false

    (* copy_grid src dst -- Destructively updates `dst` grid to have
       contents of `src` grid *)
    let copy_grid (src : grid) (dst : grid) : unit =
      for i = 0 to Spec.grid_size - 1 do
        for j = 0 to Spec.grid_size - 1 do
          dst.(i).(j) <- src.(i).(j)
        done
      done
        
    (* step_grid grid fn -- See module type definition. *)
    let step_grid =
      let temp_grid = create_grid () in
      fun (grid : state array array) ->
        for i = 0 to Spec.grid_size - 1 do
          for j = 0 to Spec.grid_size - 1 do
            temp_grid.(i).(j) <- Spec.update grid i j
          done 
        done;
      copy_grid temp_grid grid

    (* render_grid grid legend -- Renders the `grid` to the already
       initalized graphics window including the textual `legend` *)
    let render_grid (grid : state array array) (legend : string) : unit =
      (* Draw the grid of cells *)
      for i = 0 to Spec.grid_size - 1 do
        for j = 0 to Spec.grid_size - 1 do
          G.set_color (Spec.cell_color grid.(i).(j));
          G.fill_rect (i * Spec.side_size) (j * Spec.side_size)
                      (Spec.side_size - 1) (Spec.side_size - 1)
        done
      done;
      (* Draw the legend *)
      G.moveto (Spec.side_size * 2) (Spec.side_size * 2);
      G.set_color Spec.legend_color;
      G.draw_string legend;
      (* Flush to the screen *)
      G.synchronize ()

    (* run_grid -- See module type documentation *)
    let run_grid grid =
      let tick = ref 0 in
      render_grid grid (Printf.sprintf "%s: tick %d" Spec.name !tick);
      ignore (G.read_key ());    (* pause at start until key is pressed *)
      while not (G.key_pressed ()) do
        if !tick mod Spec.render_frequency = 0 then
          render_grid grid (Printf.sprintf "%s: tick %d" Spec.name !tick);
        step_grid grid;
        tick := succ !tick;
      done;
      G.close_graph () ;;
  end ;;
