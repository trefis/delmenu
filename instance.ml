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
    Evas_object.show win

  let box = Elm_box.add win
  let () =
    Elm_box.horizontal_set box true ;
    Evas_object.size_hint_weight_set box Evas.hint_expand Evas.hint_expand ;
    Elm_box.align_set box 0. 0. ;
    Elm_box.padding_set box 10 10 ;
    Elm_win.resize_object_add win box ;
    Evas_object.show box

  let lbl = Elm_label.add win
  let () =
    (* If the style is not set, the color change is ignored... *)
    ignore (Elm_object.style_set lbl "marker") ;
    Evas_object.color_set lbl 255 255 255 255 ;
    Elm_object.text_set lbl initial_prompt ;
    Elm_box.pack_end box lbl ;
    Evas_object.show lbl

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

  let select entry =
    let txt = Elm_object.text_get lbl in
    Elm_object.text_set lbl (sprintf "%s%s" txt entry)

  let user_update =
    let previous = ref "" in
    fun prompt ->
      let value = Elm_entry.entry_get prompt in
      let value = String.sub value 15 (String.length value - 15) in
      if String.is_prefix value ~prefix:(!previous) then (
        let new_char = value.[String.length value - 1] in
        if new_char = ' ' then (
          Elm_entry.entry_set prompt "<color=#ffffff>" ;
          select value
        ) else (
          previous := value ;
          let candidates = filter_candidates new_char in
          redraw candidates
        )
      ) else (
        previous := value ;
        let candidates = get_candidates value in
        redraw candidates
      )

  let complete prompt =
    let value = Elm_entry.entry_get prompt in
    let value = String.sub value 15 (String.length value - 15) in
    let new_value = complete value in
    Elm_entry.entry_set prompt @@ sprintf "<color=#ffffff>%s" new_value ;
    Elm_entry.cursor_line_end_set prompt ;
    redraw [ new_value ]

  let run_entry prompt =
    let value  = Elm_entry.entry_get prompt in
    let value  = String.sub value 15 (String.length value - 15) in
    let result = sprintf "%s%s" (Elm_object.text_get lbl) value in
    let result = String.drop_prefix result (String.length initial_prompt) in
    printf "%s\n%!" result ;
    Elm.exit ()

  let prompt = Elm_entry.add win
  let () =
    let add_callback = Evas_object_smart.callback_add_safe prompt in
    Elm_entry.entry_set prompt "<color=#ffffff>" ;
    Elm_entry.line_wrap_set prompt `none ;
    Elm_entry.single_line_set prompt true ;
    Elm_entry.scrollable_set prompt false ;
    add_callback Elm_entry.E.changed user_update ;
    add_callback Elm_entry.E.activated run_entry ;
    add_callback Elm_entry.E.unfocused complete ;
    add_callback Elm_entry.E.aborted (fun _ -> Elm.exit ());
    Elm_box.pack_end box prompt ;
    Evas_object.show prompt 

  let () =
    (* Should never be focused, only here so we can catch when the user use
       <tab>.
       Yes, this is hackish. *)
    let fake_prompt = Elm_entry.add win in
    let add_callback = Evas_object_smart.callback_add_safe fake_prompt in
    add_callback Elm_entry.E.focused
      (fun _ -> ignore (List.map2_exn [fake_prompt;prompt] [false;true] ~f:Elm_object.focus_set)) ;
    Elm_box.pack_end box fake_prompt ;
    Evas_object.show fake_prompt 

  let () =
    let completions = get_candidates "" in
    redraw completions

  let () =
    Elm_object.focus_set prompt true ;
    Elm.run () ;
    Elm.shutdown ()
end
