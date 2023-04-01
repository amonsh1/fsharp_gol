module Lib

open Mindmagma.Curses

type cell = (int * int)

let cells_for_display =
    (fun cells ->
        cells
        |> Set.map (fun (x, y) -> sprintf "(%i, %i)" x y)
        |> String.concat " ; ")

let get_neighbours ((cell_x, cell_y): cell) (area_size: (int * int)) =
    let neighborhood_size = 1

    let calculate_coord (cell_coord: int) (board_coord: int) area_size =
        let c = cell_coord - neighborhood_size + board_coord

        match c with
        | v when v >= area_size -> v - area_size
        | v when v < 0 -> area_size - v - 2
        | v -> v


    List.map
        (fun board_x ->
            (Set.map
                (fun board_y ->
                    (calculate_coord cell_x board_x (fst area_size)),
                    (calculate_coord cell_y board_y (snd area_size)))
                (Set.ofList [ 0..2 ])))
        [ 0..2 ]
    |> Set.unionMany
    |> (fun v -> Set.difference v (Set.empty.Add((cell_x, cell_y))))


let alive_cells_to_string alive_cells =
    alive_cells
    |> (Map.toList)
    |> List.fold
        (fun state (c, v) ->
            (printf "%i" 1
             state + (sprintf "(%i, %i) : %i" (fst c) (snd c) v)))
        ""

let get_neighbours_count alive_cells area_size =
    Set.fold
        (fun state c ->
            Set.fold
                (fun state neighbor ->
                    (Map.change
                        neighbor
                        (fun v ->
                            match v with
                            | Some s -> Some(s + 1)
                            | None -> Some(1))
                        state))
                state
                (get_neighbours c area_size))
        (Map.empty<cell, int>)
        alive_cells

let filter_neighbours (neighbours: Map<cell, int>) alive_cells =
    let to_birth = (Set.empty.Add(3))
    let to_live = (Set.empty.Add(2).Add(3))

    neighbours
    |> Map.filter (fun v count ->
        (Set.difference (Set.empty.Add(count)) to_birth |> Set.isEmpty)
        || ((Set.difference (Set.empty.Add(v)) alive_cells |> Set.isEmpty)
            && (Set.difference (Set.empty.Add(count)) to_live |> Set.isEmpty)))


let get_new_generation =
    (fun alive_cells area_size ->
        get_neighbours_count alive_cells area_size
        |> (fun neighbours -> filter_neighbours neighbours alive_cells))


let loop () =
    let Screen = NCurses.InitScreen()
    NCurses.NoDelay(Screen, true)
    NCurses.NoEcho()
    NCurses.Keypad(Screen, true)
    let max_x: int ref = ref 0
    let max_y: int ref = ref 0

    let rec loop_ (generation) count =
        NCurses.GetMaxYX(Screen, max_y, max_x)

        let area_size = (max_x.Value, max_y.Value - 1)
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let generation =
            get_new_generation generation area_size |> Map.keys |> Set.ofSeq
        sw.Stop()

        NCurses.Clear() |> ignore
    
        Seq.iter
            (fun (x, y) -> NCurses.MoveAddChar(y, x, (uint32 '#')) |> ignore)
            generation

        let t = sprintf "%ims" sw.ElapsedMilliseconds
        NCurses.MoveAddString(0, max_x.Value - (String.length t), t) |> ignore
        let size_str = (sprintf "%i x %i" max_x.Value max_y.Value)
        NCurses.MoveAddString(1, max_x.Value - (String.length size_str) - 1, size_str) |> ignore

        NCurses.Nap 100 |> ignore
        NCurses.Refresh() |> ignore
        let c = NCurses.GetChar()

        if c = 113 then // 'q' to quit
            ()
        else
            loop_ generation (count + 1)

    loop_
        (Set.ofList
            [ (30, 19); (30, 20); (30, 21); (29, 17); (30, 17); (31, 17) ])
        0

    NCurses.EndWin()
