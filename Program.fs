open Mindmagma.Curses

type cell = (int * int)

let get_neighbours ((cell_x, cell_y): cell) =
    let neighborhood_size = 1

    let calculate_coord (cell_coord: int) (board_coord: int) =
        cell_coord - neighborhood_size + board_coord

    List.map
        (fun board_x ->
            (List.map
                (fun board_y ->
                    (calculate_coord cell_x board_x),
                    (calculate_coord cell_y board_y))
                [ 0..2 ]))
        [ 0..2 ]
    |> List.concat

let get_neighbours_without_self =
    (fun cell -> get_neighbours cell |> List.except [ cell ])

let fit ((x, y): cell) area_size =
    let fit_ cell =
        match cell with
        | v when v >= area_size -> v - area_size
        | v when v < 0 -> area_size - v - 2
        | v -> v

    (fit_ x), (fit_ y)

let get_alive_neighbours checked_cell alive_cells area_size =
    get_neighbours_without_self checked_cell
    |> List.map (fun c -> fit c area_size)
    |> Set.ofList
    |> Set.intersect (Set.ofList alive_cells)
    |> Set.toList
    |> List.distinct


let get_new_generation alive_cells area_size =
    let for_check =
        (List.map (fun c -> get_neighbours c) alive_cells)
        |> List.concat
        |> List.distinct
        |> List.map (fun c -> fit c area_size)

    let is_alive checked_cell =
        let alive_n = get_alive_neighbours checked_cell alive_cells area_size

        let cell_is_alive =
            (List.exists (fun v -> v = checked_cell) alive_cells)

        let still_alive =
            cell_is_alive
            && (alive_n |> List.length |> (fun c -> c = 2 || c = 3))

        let become_alive = List.length alive_n = 3
        still_alive || become_alive

    let rec iter cells_for_check new_generation =
        match cells_for_check with
        | h :: t ->
            if is_alive h then
                iter t (List.append new_generation [ h ])
            else
                iter t new_generation
        | [] -> new_generation

    iter for_check [] |> List.distinct

let draw_area area_size =
    List.iter
        (fun y ->
            NCurses.MoveAddString(y, 0, (String.init area_size (fun _ -> ".")))
            |> ignore)
        [ 0..area_size ]


let loop () =
    let area_size = 10
    let start_state = [ (1, 0); (2, 1); (0, 2); (1, 2); (2, 2) ]

    let Screen = NCurses.InitScreen()
    NCurses.NoDelay(Screen, true)
    NCurses.NoEcho()
    NCurses.Keypad(Screen, true)

    let rec loop_ (generation: cell list) =
        draw_area area_size

        List.iter
            (fun (x, y) -> NCurses.MoveAddChar(y, x, (uint32 '#')) |> ignore)
            generation

        NCurses.Refresh() |> ignore
        NCurses.Nap(500) |> ignore
        NCurses.Clear() |> ignore

        let c = NCurses.GetChar()

        if c = 113 then // 'q' to quit
            ()
        else
            loop_ (get_new_generation generation area_size)

    loop_ start_state
    NCurses.EndWin()

loop ()
