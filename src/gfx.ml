
(*
 * Copyright (C) 2007-2020 The laby team
 * You have permission to copy, modify, and redistribute under the
 * terms of the GPL-3.0. For full license terms, see gpl-3.0.txt.
 *)
 

open Gtk
open Gdk
open GWindow


(* Hilfsfunktion zum erstellen fettgeschriebener Labels*)
let bold_label text packing =
  let lbl = GMisc.label ~text ~packing () in
  let font_desc = GPango.font_description_from_string "Sans Bold 10" in
  lbl#misc#modify_font font_desc;
  lbl


(* Skaliert 8-Bit-Farbwerte auf 16-Bit für RGB *)
let rgb255 r g b = `RGB (r * 257, g * 257, b * 257)

(* Hexstring "#RRGGBB" → RGB. Beispiel: let color = hex "#ff6400" *)
let hex s =
  if String.length s <> 7 || s.[0] <> '#' then invalid_arg "hex";
  let r = int_of_string ("0x" ^ String.sub s 1 2) in
  let g = int_of_string ("0x" ^ String.sub s 3 2) in
  let b = int_of_string ("0x" ^ String.sub s 5 2) in
  rgb255 r g b


(*Hilfsfunktion, um Vorder- und Hintergrundfarbe von Widgets zu ändern*)
(*Beispiel: set_bg_fg window ~bg:(hex "#372f2d") ~fg:`NAME "white"*)
let set_bg_fg widget ~bg ~fg =
  widget#misc#modify_bg [`NORMAL, bg];
  widget#misc#modify_fg [`NORMAL, fg]

(* === Hilfsfunktion: Standard-Dokumenteordner finden === *)
let get_default_documents_folder () =
  let home = Sys.getenv_opt "HOME" in
  match home with
  | Some h ->
      let candidates = [
        Filename.concat h "Dokumente";  (* deutsch *)
        Filename.concat h "Documents";  (* englisch *)
      ] in
      (match List.find_opt Sys.file_exists candidates with
       | Some path -> path
       | None -> h)
  | None -> "."


let chosen_programing_language = ref "unknown"

(* === Hilfsfunktion: Standard-Dateiname aus Sprache und Datum === *)
let make_current_name () =
  (* aktuelles Datum *)
  let open Unix in
  let tm = localtime (time ()) in
  let date =
    Printf.sprintf "%04d-%02d-%02d"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
  in
  let time =
  Printf.sprintf "%02d-%02d-%02d"
    tm.tm_hour tm.tm_min tm.tm_sec
  in
  (* Dateiname zusammenbauen *)
  Printf.sprintf "laby_%s_%s_%s.txt" !chosen_programing_language date time


(* === Save-Funktion mit Dialog === *)
let save_file (view_prog : GSourceView3.source_view) =
  (* Versuche, ein Dokumente-Verzeichnis des Benutzers zu finden *)
  let home = Sys.getenv_opt "HOME" in
  let default_folder = get_default_documents_folder () in
  let dialog = GWindow.file_chooser_dialog
    ~action:`SAVE
    ~title:"Save your Laby source code as text file"
    ()
  in
  ignore (dialog#add_button "Cancel" `CANCEL);
  ignore (dialog#add_button "Save" `SAVE);

  (* Setze Standardordner und Dateiname *)
  dialog#set_current_folder default_folder;
  dialog#set_current_name (make_current_name());

  (match dialog#run () with
   | `SAVE ->
       (match dialog#filename with
        | Some filename ->
            let buffer = view_prog#source_buffer in
            let start_iter = buffer#start_iter in
            let end_iter = buffer#end_iter in
            let text = buffer#get_text ~start:start_iter ~stop:end_iter () in
            (try
               let oc = open_out filename in
               output_string oc text;
               close_out oc;
               print_endline ("Saved to " ^ filename)
             with Sys_error msg ->
               prerr_endline ("Save failed: " ^ msg))
        | None -> ())
   | _ -> ());
  dialog#destroy ()



(* === Open-Funktion mit Dialog === *)
let open_file (view_prog : GSourceView3.source_view) =
  (* Versuche, ein Dokumente-Verzeichnis des Benutzers zu finden *)
  let home = Sys.getenv_opt "HOME" in
  let default_folder = get_default_documents_folder () in
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Open Laby source code"
    ()
  in
  ignore (dialog#add_button "Cancel" `CANCEL);
  ignore (dialog#add_button "Open" `OPEN);
  dialog#set_current_folder default_folder;

  (* try ... with ... sorgt dafür, dass der Dialog immer zerstört wird *)
  (try
     match dialog#run () with
     | `OPEN ->
         (match dialog#filename with
          | Some filename ->
              let ic = open_in filename in
              let len = in_channel_length ic in
              let text = really_input_string ic len in
              close_in ic;
              view_prog#buffer#set_text text
          | None -> ())
     | _ -> ()
   with _ -> ());
  dialog#destroy ()


let log = Log.make ["gfx"]

let conf =
  Conf.void
    (F.x "graphic interface configuration" [])

let conf_tilesize =
  Conf.int ~p:(conf#plug "tile-size") ~d:55
    (F.x "size of tiles in pixels" [])

let conf_playback_rate =
  Conf.float ~p:(conf#plug "playback-rate") ~d:2.5
    (F.x "number of iterations per second" [])

let conf_cue_rate =
  Conf.float ~p:(conf#plug "cue-rate") ~d:30.0
    (F.x "number of iterations per second in fast-forward/rewind mode" [])

let conf_source_style =
  Conf.string ~p:(conf#plug "source-style") ~d:"laby_theme"
    (F.x "highlighting style to use for source code" [])

let conf_window =
  Conf.void ~p:(conf#plug "window")
    (F.x "initial window geometry" [])


(*
let conf_window_width =
  Conf.int ~p:(conf_window#plug "width") ~d:600
    (F.x "width of window" [])


let conf_window_height =
  Conf.int ~p:(conf_window#plug "height") ~d:900
    (F.x "height of window" [])


*)

exception Error of F.t

type ressources =
    {
      size : int;
      void_p : GdkPixbuf.pixbuf;
      exit_p : GdkPixbuf.pixbuf;
      wall_p : GdkPixbuf.pixbuf;
      rock_p : GdkPixbuf.pixbuf;
      web_p : GdkPixbuf.pixbuf;
      nrock_p : GdkPixbuf.pixbuf;
      nweb_p : GdkPixbuf.pixbuf;
      ant_n_p : GdkPixbuf.pixbuf;
      ant_e_p : GdkPixbuf.pixbuf;
      ant_s_p : GdkPixbuf.pixbuf;
      ant_w_p : GdkPixbuf.pixbuf;
    }

type controls =
    {
      window: GWindow.window;
      start_vbox: GPack.box;
      start_image: GMisc.image;
      main_hpaned: GPack.paned;
      (* left_paned: GPack.paned; *)
      menu_quit: GMenu.menu_item;
      menu_home: GMenu.menu_item;
      menu_level: GMenu.menu_item;
      menu_levels: GMenu.menu;
      button_start: GButton.button;
      button_prev: GButton.tool_button;
      button_next: GButton.tool_button;
      button_play: GButton.toggle_tool_button;
      button_backward: GButton.toggle_tool_button;
      button_forward: GButton.toggle_tool_button;
      button_execute: GButton.button;
      map_image: GMisc.image;
      interprets: GEdit.combo_box;
      view_prog: GSourceView3.source_view;
      view_help: GSourceView3.source_view;
      box_help: GPack.box;
      view_mesg: GText.view;
      view_title: GMisc.label;
      view_comment: GMisc.label;
    }

let messages l h m =
  if (l <= 0) && (Sys.os_type = "Win32" || not (Unix.isatty Unix.stdout)) then
    let message_type =
      match l with -4 | -3 | -2 -> `ERROR | -1 -> `WARNING | _ -> `INFO
    in
    let w =
      GWindow.message_dialog
	~title:("laby: " ^ Fd.render_raw h) ~buttons:GWindow.Buttons.ok
	~message:(Fd.render_raw m)
	~message_type ()
    in
    let _ = w#run () in w#destroy ()


let exception_handler e =
  let bt = Printexc.get_backtrace () in
  Run.error (
    F.x "exception: <exn>" [
      "exn", F.exn ~bt e;
    ]
  )

let gtk_init () =
  GtkSignal.user_handler := exception_handler;
  let _ = GtkMain.Main.init () in
  Run.report messages;
  (* work around messed up gtk/lablgtk *)
  Sys.catch_break false;
  begin match Sys.os_type with
  | "Unix" ->
      Sys.set_signal Sys.sigpipe (Sys.Signal_default);
      Sys.set_signal Sys.sigquit (Sys.Signal_default);
  | _ -> ()
  end;
  Sys.set_signal Sys.sigterm (Sys.Signal_default);
  let tile_size = max 5 conf_tilesize#get in
  let pix p =
    let file = Res.get ["tiles"; p ^ ".svg"] in
    begin try
      GdkPixbuf.from_file_at_size file tile_size tile_size

    with
    | GdkPixbuf.GdkPixbufError(GdkPixbuf.ERROR_UNKNOWN_TYPE, _) ->
	let file = Res.get ["tiles"; p ^ ".png"] in
	GdkPixbuf.from_file_at_size file tile_size tile_size

    end
  in
  {
    size = tile_size;
    void_p = pix "void";
    exit_p = pix "exit";
    wall_p = pix "wall";
    rock_p = pix "rock";
    web_p = pix "web";
    nrock_p = pix "nrock";
    nweb_p = pix "nweb";
    ant_n_p = pix "ant-n";
    ant_e_p = pix "ant-e";
    ant_s_p = pix "ant-s";
    ant_w_p = pix "ant-w";
  }

let draw_state state ressources (pixbuf : GdkPixbuf.pixbuf) =
  let size = ressources.size in
  let tile i j p =
    let px = size / 2 + i * size in
    let py = size / 2 + j * size in
    (* GdkPixbuf.copy_area ~dest:pixbuf ~dest_x:px ~dest_y:py p *)
    GdkPixbuf.composite ~dest:pixbuf ~alpha:255
      ~dest_x:px ~dest_y:py ~width:size ~height:size
      ~ofs_x:(float px) ~ofs_y:(float py) ~scale_x:1.0 ~scale_y:1.0 p
  in
  let i0, j0 = State.pos state in
  let disp_tile i j t =
    begin match t with
    | `Void -> tile i j ressources.void_p
    | `Exit -> if i <> i0 || j <> j0 then tile i j ressources.exit_p
    | `Wall -> tile i j ressources.wall_p
    | `Rock -> tile i j ressources.rock_p
    | `Web -> tile i j ressources.web_p
    | `NRock -> tile i j ressources.nrock_p
    | `NWeb -> tile i j ressources.nweb_p
    end
  in
  State.iter_map state disp_tile;
  begin match State.dir state with
  | `N -> tile i0 j0 ressources.ant_n_p
  | `E -> tile i0 j0 ressources.ant_e_p
  | `S -> tile i0 j0 ressources.ant_s_p
  | `W -> tile i0 j0 ressources.ant_w_p
  end;
  begin match State.carry state with
  | `Rock -> tile i0 j0 ressources.rock_p
  | `None -> ()
  end

let labeled_combo text packing strings =
  let box = GPack.hbox ~packing () in
  let _ = GMisc.label ~text ~xpad:5 ~ypad:8 ~packing:box#pack () in
  fst (GEdit.combo_box_text ~strings ~packing:box#add ())

let label packing =
  GMisc.label ~ypad:5 ~line_wrap:true ~packing ()

let label_txt text packing =
  ignore (GMisc.label ~text ~ypad:5 ~line_wrap:true ~packing ())

let label_menu = F.x "Menu" []
let label_level = F.x "Level" []
let label_welcome = F.x "Welcome to laby, a programming game." []
let label_language = F.x "Language:" []
let label_prog = F.x "Program:" []
let label_mesg = F.x "Messages:" []
let label_help = F.x "Help:" []
let label_start = F.x "Start" []


let layout languages =
  let scrolled ?(vpolicy=`ALWAYS) packing =
    GBin.scrolled_window ~packing ~hpolicy:`AUTOMATIC ~vpolicy ()
  in
  let monofont = GPango.font_description_from_string "monospace" in
  let window = GWindow.window ~resizable:true ~title:"Modified Laby" () in
  window#maximize ();
  
  (* --- Eigene Fensterfarben erstellen mit Lablgtk2 --- *)
  (*Erfundene Bodenfarbe: #372f2d --> RGB: RGB(55,47,45) *)
	let color_w_bg = hex "#372f2d" in
	window#misc#modify_bg [`NORMAL, color_w_bg];
	window#misc#modify_fg [`NORMAL, `NAME "white"];

  let windowbox = GPack.vbox ~packing:window#add () in
  
  (*Farbe der Windowbox ändern*)
  (*Das betrifft die Farben dieser Fenster: Menu-Leiste, Programmierfenster-Leiste, Tool-Leiste, Nachrichtenfenster-Leiste*)
  (*
  let color_wb_bg = `NAME "darkslategray" in
	let color_wb_fg = `NAME "white" in

	windowbox#misc#modify_bg [`NORMAL, color_wb_bg];
	windowbox#misc#modify_fg [`NORMAL, color_wb_fg];
  *)
  
  let menu_bar = GMenu.menu_bar ~packing:windowbox#pack () in

  (*Farbe der Menu-Leiste ändern. Mauerfarbe: #ff6400 -> RBG(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75) *)
	let color_menu_bg = hex "#ff6400" in
	menu_bar#misc#modify_bg [`NORMAL, color_menu_bg];
	menu_bar#misc#modify_fg [`NORMAL, `NAME "black"];

  let menu_levels = GMenu.menu () in
  let sub_main = GMenu.menu () in
  let menu_main = GMenu.menu_item ~label:(Fd.render_raw label_menu)
    ~packing:menu_bar#append () in
  let menu_level = GMenu.menu_item
    ~label:(Fd.render_raw label_level) ~packing:menu_bar#append () in
  let menu_fullscreen = GMenu.menu_item ~label:"Fullscreen"
    ~packing:sub_main#append () in
  let menu_unfullscreen = GMenu.menu_item ~label:"Leave Fullscreen"
    ~packing:sub_main#append ~show:false () in
  let menu_quit = GMenu.menu_item ~label:"Quit"
    ~packing:sub_main#append () in
  let menu_home = GMenu.menu_item ~label:"Home"
    ~packing:sub_main#append () in


  let fullscreen () =
    menu_fullscreen#misc#hide ();
    menu_unfullscreen#misc#show ();
    window#fullscreen ();
  in
  let unfullscreen () =
    menu_unfullscreen#misc#hide ();
    menu_fullscreen#misc#show ();
    window#unfullscreen ();
  in
  ignore (menu_fullscreen#connect#activate ~callback:fullscreen);
  ignore (menu_unfullscreen#connect#activate ~callback:unfullscreen);
  menu_level#set_submenu menu_levels;
  menu_main#set_submenu sub_main;
  let main_vbox = GPack.vbox ~packing:windowbox#add () in

  (* Start-up screen *)
  let start_vbox = GPack.vbox ~packing:main_vbox#add
    ~spacing:10 ~border_width:25 () in
  (*
  (*Farbe des Start-Fensters ändern*)(* bräunliches hellgrün: #b8c28e -> RGB(184,194,142) *)

	let color_svb_bg = hex "#b8c28e" in
	let color_svb_fg = `NAME "black" in

	start_vbox#misc#modify_bg [`NORMAL, color_svb_bg];
	start_vbox#misc#modify_fg [`NORMAL, color_svb_fg];
  *)
  let start_image = GMisc.image ~packing:start_vbox#add () in
  let mstart_vbox = GPack.vbox ~packing:start_vbox#pack () in
  let _ = GMisc.label ~markup:(Fd.render_raw label_welcome)
    ~justify:`CENTER ~packing:mstart_vbox#pack () in
  let interprets =
    labeled_combo (Fd.render_raw label_language) mstart_vbox#pack languages
  in
  let button_start = GButton.button ~packing:mstart_vbox#pack
    ~label:(Fd.render_raw label_start) () in

	(* Fokus direkt auf Start-Button setzen *)
	button_start#misc#grab_focus ();

  (* Game screen *)
  let hpaned = GPack.paned `HORIZONTAL ~packing:main_vbox#add () in
  let tile_size = max 5 conf_tilesize#get in
  hpaned#set_position (10 + 550 * tile_size / 40);

  let left_paned = GPack.paned `VERTICAL ~packing:hpaned#add () in
  (* let screen_height = Gdk.Screen.height () in *)
  left_paned#set_position(Gdk.Screen.height ()*5/8);

  (* ignore (
    left_paned#misc#connect#size_allocate ~callback:(fun _ ->
      let alloc = left_paned#misc#allocation in
     left_paned#set_position (alloc.Gtk.height*3 / 4);
     ()
    )
  ); *)

  (* ignore (
  GMain.Idle.add (fun () ->
    let alloc = left_paned#misc#allocation in
    left_paned#set_position (alloc.Gtk.height * 2 / 3);
    false
  )
); *)


  (* let total_height = conf_window_height#get in
  left_paned#set_position (total_height / 2); *)

  let lvbox = GPack.vbox ~packing:left_paned#add () in



  (*
  (*Farbe des Level-Fensters ändern*)(* bräunliches hellgrün: #b8c28e -> RGB(184,194,142)*)

	let color_lvb_bg = hex "#b8c28e" in
	let color_lvb_fg = `NAME "black" in

	lvbox#misc#modify_bg [`NORMAL, color_lvb_bg];
	lvbox#misc#modify_fg [`NORMAL, color_lvb_fg];
  *)
  let vpaned = GPack.paned `VERTICAL ~packing:hpaned#add () in

  ignore (GMain.Idle.add (fun () ->
    vpaned#set_position (Gdk.Screen.height () * 3 / 4);
    false
  ));


  (* vpaned#set_position (Gdk.Screen.height ()*5/6); *)
  
  (*let view_title = label lvbox#pack *)
  let view_title = bold_label "" lvbox#pack in
  let view_comment = label lvbox#pack in
  let sw_laby = scrolled ~vpolicy:`AUTOMATIC lvbox#add in
  

  (*Box-help used to be here*)

    let mesg_box = GPack.vbox ~packing:left_paned#add () in

  (*Farbe der Leiste über dem Nachrichtenfenster ändern. Mauerfarbe: #ff6400 -> RBG(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75)*)
  (* EventBox für den Hintergrund *)
	let eb_mesg = GBin.event_box ~packing:mesg_box#pack () in
	let color_eb_mesg_bg = hex "#ff6400" in
	eb_mesg#misc#modify_bg [`NORMAL, color_eb_mesg_bg];

	(* Nichtfettes Label aus F.t erstellen *)
	(* let label_mesg_widget = GMisc.label ~text:(Fd.render_raw label_mesg) () in

  (*Fettes Label erstellen*)
	eb_mesg#add (label_mesg_widget :> GObj.widget); *)
  let label_mesg_widget = bold_label (Fd.render_raw label_mesg) eb_mesg#add in
	label_mesg_widget#misc#modify_fg [`NORMAL, `NAME "black"];
  
  (*Code für nichtfettes Label*)
  (*label_txt (Fd.render_raw label_mesg) rbvbox#pack;*)
  
  (* let sw_mesg = scrolled mesg_box#pack in
  let view_mesg = GText.view ~editable:false ~packing:sw_mesg#add  () in *)
  let sw_mesg = GBin.scrolled_window
  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  mesg_box#pack ~expand:true ~fill:true (sw_mesg :> GObj.widget);

  let view_mesg = GText.view ~editable:false ~packing:sw_mesg#add () in

  view_mesg#misc#modify_font monofont;

  view_mesg#set_left_margin 10;
  view_mesg#set_right_margin 6;
  view_mesg#set_pixels_above_lines 5;
  view_mesg#set_pixels_below_lines 2;



  (*Rechte Seite. Enthält: Programmierfenster, Toolbar, Nachrichtenfenster*)
  let rtvbox = GPack.vbox ~packing:vpaned#add1 () in
  
  (*Farbe der Rtv-Box ändern. Betrifft: Programmierfenster-Leiste, Tool-Leiste*)
  (* Mauerfarbe: #ff6400 -> RGB(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75). Bräunliches hellgrün: #b8c28e -> RGB(184,194,142)*)
	let color_rtvb_bg = hex "#ff6400" in
	rtvbox#misc#modify_bg [`NORMAL, color_rtvb_bg];
	rtvbox#misc#modify_fg [`NORMAL, `NAME "black"];
  
  (*
  (*Farbe der Leiste über dem Programmierfenster ändern. Mauerfarbe: #ff6400 -> RBG(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75)*)
  (* EventBox für den Hintergrund *)
	let eb_prog = GBin.event_box ~packing:rtvbox#pack () in

	let color_eb_prog_bg = hex "#ff6400" in
	eb_prog#misc#modify_bg [`NORMAL, color_eb_prog_bg];

	(* Label aus F.t erstellen *)
	let label_prog_widget = GMisc.label ~text:(Fd.render_raw label_prog) () in
	eb_prog#add (label_prog_widget :> GObj.widget);
	label_prog_widget#misc#modify_fg [`NORMAL, `NAME "black"];
	*)
	
	(*Ohne Farbänderung der Leiste über dem Programmierfenster, die folgende Zeile ausführen lassen. Sonst nicht.*)

  (* Nicht fett geschriebene Labels*)
  (* label_txt (Fd.render_raw label_prog) rtvbox#pack; *)
  ignore (bold_label (Fd.render_raw label_prog) rtvbox#pack);
  
  let sw_prog = scrolled rtvbox#add in
  let view_prog =
    GSourceView3.source_view
      ~auto_indent:true ~indent_width:2 ~insert_spaces_instead_of_tabs:true
      ~show_line_numbers:true ~packing:sw_prog#add ()
  in
  view_prog#set_indent 1;
  
  let font = GPango.font_description_from_string "Ubuntu Mono 11" in
	view_prog#misc#modify_font font;


  (*view_prog#misc#modify_font monofont;*)
  
  (*Hintergrundfarbe des Programmierfensters ändern *)
  (*view_prog#misc#modify_bg [`NORMAL, `BLACK];*)
  (*Leider wird die farbige Umrandung markierten Textes (Highlight-Farbe) so nicht mehr angezeigt.*)

  (*Die Menu-Items für's Speichern und Öffnen sind hier definiert, da es um den Inhalt des Programmierfensters geht.*)
  let save_item = GMenu.menu_item ~label:"Save" ~packing:sub_main#append () in
  save_item#connect#activate ~callback:(fun () -> save_file view_prog);

  let open_item = GMenu.menu_item ~label:"Open" ~packing:sub_main#append () in
  open_item#connect#activate ~callback:(fun () -> open_file view_prog);


  
  let bbox = GPack.hbox ~packing:rtvbox#pack () in
  (*
  (*Farbe der B-Box ändern. Betrifft: Tool-Leiste. Mauerfarbe: #ff6400 -> RBG(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75)*)

	let color_bb_bg = hex "#ff6400" in
	bbox#misc#modify_bg [`NORMAL, color_bb_bg];
	bbox#misc#modify_fg [`NORMAL, `NAME "black"];
  *)
  let button_execute = GButton.button ~packing:bbox#pack ~stock:`EXECUTE () in
  (* let rbvbox = GPack.vbox ~packing:vpaned#add2 () in *)

  let box_help = GPack.vbox ~packing:vpaned#add2 () in

  (*Farbe der Hilfe-Leiste ändern. Mauerfarbe: #ff6400 -> RBG(255,100,0). Steinfarbe: #5c4f4b -> RGB(92,79,75) *)
  (* EventBox für den Hintergrund *)
  let eb_help = GBin.event_box ~packing:box_help#pack () in
  let color_eb_help_bg = hex "#ff6400" in
  eb_help#misc#modify_bg [`NORMAL, color_eb_help_bg];

  let label_help_widget = bold_label (Fd.render_raw label_help) eb_help#add in
	label_help_widget#misc#modify_fg [`NORMAL, `NAME "black"];

  (*Ursprünglicher Code ohne Farbänderung der Hilfe-Leiste:*)
  (* label_txt (Fd.render_raw label_help) box_help#pack; *)

  let sw_help = GBin.scrolled_window
  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
  in
  box_help#pack ~expand:true ~fill:true (sw_help :> GObj.widget);
  
  (* let sw_help = scrolled box_help#pack in *)
  let view_help =
    GSourceView3.source_view ~editable:false ~packing:sw_help#add ()
    (* GSourceView3.source_view ~height:500 ~editable:false ~packing:sw_help#add () *)
  in
  (* sw_help#add ~editable:false (view_help :> GObj.widget); *)

  view_help#set_indent 1;
  view_help#misc#modify_font monofont;

  let toolbar = GButton.toolbar ~packing:bbox#pack () in
  let button stock = GButton.tool_button ~packing:toolbar#insert ~stock () in
  let tbutton stock =
    GButton.toggle_tool_button ~packing:toolbar#insert ~stock ()
  in
  let button_backward = tbutton `MEDIA_REWIND in
  let button_prev = button `GO_BACK in
  let button_play = tbutton `MEDIA_PLAY in
  let button_next = button `GO_FORWARD in
  let button_forward = tbutton `MEDIA_FORWARD in
  (* view_prog#misc#grab_focus (); *)
  
  


  (*eb_mesg and sw_mesg used to be here*)




  let map_image = GMisc.image ~packing:sw_laby#add_with_viewport () in
  button_execute#set_focus_on_click false;
  {
    window = window;
    start_vbox = start_vbox;
    start_image = start_image;
    main_hpaned = hpaned;
    menu_quit = menu_quit; menu_home = menu_home;
    menu_level = menu_level; menu_levels = menu_levels;
    button_start = button_start;
    button_prev = button_prev; button_next = button_next;
    button_play = button_play;
    button_backward = button_backward;
    button_forward = button_forward;
    button_execute = button_execute;
    map_image = map_image;
    interprets = interprets;
    view_prog = view_prog; view_mesg = view_mesg;
    box_help = box_help; view_help = view_help;
    view_title = view_title; view_comment = view_comment;
  }

let make_pixbuf tile_size level =
  let sizex, sizey = Level.size level in
  let width, height = tile_size * (1 + sizex), tile_size * (1 + sizey) in
  (* let width = (Gdk.Screen.width ()/3) in
  let height = (width*sizey/sizex) in *)
  GdkPixbuf.create ~width ~height ~has_alpha:true ()


let display_gtk ressources =

  let amods = Mod.pool () in
  let mods = List.filter (fun x -> x#check) amods in
  let language_list =
    List.sort (compare) (List.map (fun x -> x#name) mods)
  in
  let levels_list =
    List.sort (compare) (Res.get_list ~ext:"laby" ["levels"])
  in
  if mods = [] then Run.fatal (
    F.x "no mod is available among: <list>" [
      "list", F.v (List.map (fun x -> F.string x#name) amods);
    ]
  );

  let c = layout language_list in

  let level_load name =
    let l = Level.load (Res.get ["levels"; name]) in
    c.map_image#set_pixbuf (make_pixbuf ressources.size l); l
  in
  let syntaxd = Res.get ["syntax"] in
  let add_search_path m l = m#set_search_path (l @ m#search_path) in
  let ssm = GSourceView3.source_style_scheme_manager true in
  add_search_path ssm [syntaxd; Res.path [syntaxd; "styles"]];
  let style = ssm#style_scheme conf_source_style#get in
  c.view_prog#source_buffer#set_style_scheme style;
  c.view_help#source_buffer#set_style_scheme style;
  
  (*Versuch Fensterfarbe zu ändern wird ignoriert, wenn mit Style-Scheme gearbeitet wird.*)
  (*
  c.view_prog#misc#modify_base [`NORMAL, `NAME "#000000"];
	c.view_prog#misc#modify_text [`NORMAL, `NAME "#ffffff"];

	c.view_help#misc#modify_base [`NORMAL, `NAME "#000000"];
	c.view_help#misc#modify_text [`NORMAL, `NAME "#ffffff"];
	*)
  
  let slm = GSourceView3.source_language_manager false in
  add_search_path slm [syntaxd; Res.path [syntaxd; "language-specs"]];


  (* gui outputs *)
  let msg str =
    c.view_mesg#buffer#place_cursor c.view_mesg#buffer#end_iter;
    c.view_mesg#buffer#insert (str ^ "\n")
  in
  let f_msg m = msg (Fd.render_raw m) in
  let help h =
    begin match h with
    | None ->
	c.box_help#misc#hide ()
    | Some help ->
	c.view_help#buffer#set_text help;
	c.box_help#misc#show ()
    end
  in
  let draw image state =
    let p : GdkPixbuf.pixbuf = image#pixbuf in
    GdkPixbuf.fill p 0l;
    draw_state state ressources p;
    image#set_pixbuf p
  in

  (* game creation *)
  let command = Game.play msg help (draw c.map_image) in

  (* gui inputs *)
  let rid = ref None in
  let start_animation = ref (Level.generate Level.dummy) in
  let start_play () =
    c.start_vbox#misc#hide ();
    c.menu_quit#misc#hide ();
    begin match !rid with
      | None -> ()
      | Some id -> GMain.Timeout.remove id; rid := None
    end;
    rid := None;
    c.menu_home#misc#show ();
    c.menu_level#misc#show ();
    c.main_hpaned#misc#show ()

  in
  let exit_play () =
    c.main_hpaned#misc#hide ();
    c.menu_home#misc#hide ();
    c.menu_level#misc#hide ();
    let callback () =
      start_animation := State.random_walk !start_animation;
      draw c.start_image (!start_animation);
      true
    in
    let rate = conf_playback_rate#get in
    let ms = int_of_float (1000. /. rate) in
    rid := Some (GMain.Timeout.add ~ms ~callback);
    c.menu_quit#misc#show ();
    c.start_vbox#misc#show ()
  in
  let show_execute () = c.button_execute#set_relief `NORMAL in
  let hide_execute () = c.button_execute#set_relief `NONE in
  let ctrl_sensitive b =
    c.button_backward#misc#set_sensitive b;
    c.button_forward#misc#set_sensitive b;
    c.button_play#misc#set_sensitive b;
    c.button_prev#misc#set_sensitive b;
    c.button_next#misc#set_sensitive b;
  in
  let play_inactive () =
    c.button_forward#set_active false;
    c.button_backward#set_active false;
    c.button_play#set_active false
  in
  let clear () =
    c.view_mesg#buffer#set_text "";
    ctrl_sensitive false;
    show_execute ();
    play_inactive ();
  in
  let setupmod () =
    begin match
      try Some (List.nth language_list c.interprets#active)
      with _ -> None
    with
      | Some name ->
        chosen_programing_language := name;
	 let lmod = List.find (fun x -> x#name = name) mods in
	 c.view_prog#buffer#set_text (command#chg_mod lmod);
	 let l = slm#language name in
	 if l = None then
	   log#warning (
	    F.x "cannot load syntax for <name> mod" [
	      "name", F.string name;
	    ]
	  );
    
	 c.view_prog#source_buffer#set_language l;
	 c.view_help#source_buffer#set_language l;
      | None -> ()
    end
  in
  let newmod () =
    command#chg_program (c.view_prog#buffer#get_text ());
    setupmod ();
    clear ()
  in
  let execute () =
    clear ();
    command#chg_program (c.view_prog#buffer#get_text ());
    begin match command#run with
    | true ->
	f_msg (F.h [F.s "——"; Say.good_start; F.s "——"]);
	ctrl_sensitive true
    | false ->
	f_msg (F.h [F.s "——"; Say.bad_start; F.s "——"]);
	ctrl_sensitive false
    end;
    hide_execute ();
  in
  let newlevel name =
    begin match List.mem name levels_list with
    | true ->
	let l = level_load name in
	c.view_title#set_text (Level.title l);
	c.view_comment#set_text (Level.comment l);
	command#chg_level l;
	clear ()
    | false -> ()
    end
  in
  let prev () = if not command#prev then play_inactive () in
  let next () = if not command#next then play_inactive () in
  let play =
    let rid = ref None in
    begin fun direction rate () ->
      begin match !rid with
      | None ->
	  let callback () =
	    begin match direction with
	    | `Forward -> next (); true
	    | `Backward -> prev (); true
	    end
	  in
          let ms = int_of_float (1000. /. rate) in
	  rid := Some (GMain.Timeout.add ~ms ~callback);
      | Some id ->
	  play_inactive ();
	  GMain.Timeout.remove id; rid := None
      end
    end
  in
  let destroy () =
    command#quit;
    c.window#destroy ();
    GMain.Main.quit ()
  in
  let altdestroy _ = destroy (); true in
  let smod = Mod.conf_selected#get in
  let select i m = if m = smod then c.interprets#set_active i in
  List.iteri select language_list;

  let group = ref None in
  let add_language l =
    let item = GMenu.radio_menu_item ?group:!group
      ~label:l ~packing:c.menu_levels#append () in
    if !group = None then group := Some (item#group);
    ignore (item#connect#activate ~callback:(fun () -> newlevel l))
  in
  List.iter add_language levels_list;

  (* declaring callbacks *)
  let play_cb = play `Forward conf_playback_rate#get in
  let forward_cb = play `Forward conf_cue_rate#get in
  let backward_cb = play `Backward conf_cue_rate#get in
  ignore (c.window#event#connect#delete ~callback:altdestroy);
  ignore (c.window#connect#destroy ~callback:destroy);
  ignore (c.button_start#connect#clicked ~callback:start_play);
  ignore (c.button_prev#connect#clicked ~callback:prev);
  ignore (c.button_next#connect#clicked ~callback:next);
  ignore (c.button_play#connect#toggled ~callback:play_cb);
  ignore (c.button_backward#connect#toggled ~callback:backward_cb);
  ignore (c.button_forward#connect#toggled ~callback:forward_cb);
  ignore (c.button_execute#connect#clicked ~callback:execute);
  ignore (c.interprets#connect#changed ~callback:newmod);
  ignore (c.view_prog#buffer#connect#changed ~callback:show_execute);
  ignore (c.menu_quit#connect#activate ~callback:destroy);
  ignore (c.menu_home#connect#activate ~callback:exit_play);
  (* now we must have everything up *)
  setupmod ();
  exit_play ();
  c.window#maximize ();
  (*c.window#set_default_size conf_window_width#get conf_window_height#get;*)
  c.window#show ();

  

  if List.mem "0.laby" levels_list
  then newlevel "0.laby"
  else if levels_list <> [] then newlevel (List.hd levels_list);
  c.start_image#set_pixbuf (make_pixbuf ressources.size Level.dummy);
  draw c.start_image (!start_animation);
  ignore (GMain.Main.main ())

let run_gtk () =
  let ressources =
    begin try gtk_init () with
    | Gtk.Error m ->
	raise (
	  Error (
	    F.x "gtk error: <error>" ["error", F.q (F.string m)]
	  )
	)
    end
  in
  display_gtk ressources


