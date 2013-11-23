open Core.Std
open Efl

module type Seed = sig
  val initial_prompt : string

  val get_candidates : string -> string list

  val filter_candidates : char -> string list

  val complete : string -> string
  (* TO ADD:
       - bg color
       - prompt color
       - match color
  *)
end

module Run (Init : Seed) = struct
  open Init

  let () = Efl.Elm.init Sys.argv

  let win = Elm_win.add "delmenu" `basic
  let () =
    Elm_win.title_set win "delmenu" ;
    let _x, _y, width, _h = Elm_win.screen_size_get win in
    Evas_object.resize win width 15 (* TODO: adapt to font size *) ;
    Elm_win.override_set win true ;
    Evas_object.show win

  let box = Elm_box.add win
  let () =
    Elm_box.horizontal_set box true ;
    Evas_object.size_hint_weight_set box Evas.hint_expand Evas.hint_expand ;
    Elm_box.align_set box 0. 0. ;
    Elm_box.padding_set box 10 10 ;
    Elm_win.resize_object_add win box ;
    Evas_object.show box

  let prompt = Elm_label.add win
  let () =
    (* If the style is not set, the color change is ignored... *)
    ignore (Elm_object.style_set prompt "marker") ;
    Evas_object.color_set prompt 255 255 255 255 ;
    Elm_object.text_set prompt initial_prompt ;
    Elm_box.pack_end box prompt ;
    Evas_object.show prompt

  (* let foo _ = () *) (* Uncomment this line to get a segfault. *)

  let input = Elm_label.add win
  let update_input txt = Elm_object.text_set input @@ "<color=#ffffff>" ^ txt
  let () =
    update_input "bite" ;
    Elm_box.pack_end box input ;
    Evas_object.show input 

  let cleaner = ref (fun () -> ())
  let redraw lst =
    !cleaner () ;
    (* Don't handle more elements than we can display. 
       FIXME: that's hackish, 30 might be too many or too few elements depending
       on the size of the elements. Improve. *)
    let lst = List.take lst 30 in
    let removables =
      List.map lst ~f:(fun s ->
        let lbl = Elm_label.add win in
        Elm_object.text_set lbl @@ sprintf "<color=#ffffff>%s</color>" s ;
        Elm_box.pack_end box lbl ;
        Evas_object.show lbl ;
        lbl
      )
    in
    cleaner :=
      begin fun () ->
        List.iter removables ~f:(fun obj ->
          Elm_box.unpack box obj ;
          Evas_object.del obj
        )
      end

  let () =
    let completions = get_candidates "" in
    redraw completions

  let () =
    Elm.run () ;
    Elm.shutdown ()
end
