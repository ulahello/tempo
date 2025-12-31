let eps = 0.001

let rec revrange : int -> int list = function
  | n when n <= 0 -> []
  | n -> n :: revrange (n - 1)

let range n = List.rev (revrange n)

module Pqueue_Test = struct
  let push_pop () =
    let q = Tempo.Pqueue.create 8 in
    let q =
      List.fold_left Tempo.Pqueue.push q (range (Tempo.Pqueue.capacity q))
    in
    Alcotest.(check (list int))
      "push works" [ 1; 2; 3; 4; 5; 6; 7; 8 ] (Tempo.Pqueue.to_list q);
    Alcotest.(check bool) "full" true (Tempo.Pqueue.is_full q);

    let q, x = Tempo.Pqueue.pop q in
    Alcotest.(check (option int)) "1st pop" (Some 1) x;

    let q, x = Tempo.Pqueue.pop q in
    Alcotest.(check (option int)) "2nd pop" (Some 2) x;

    let q, x = Tempo.Pqueue.pop q in
    Alcotest.(check (option int)) "3rd pop" (Some 3) x;

    Alcotest.(check (list int))
      "pop works" [ 4; 5; 6; 7; 8 ] (Tempo.Pqueue.to_list q)

  let push_clobber () =
    let q = Tempo.Pqueue.create 8 in
    let q =
      List.fold_left Tempo.Pqueue.push q (range (Tempo.Pqueue.capacity q))
    in
    Alcotest.(check (list int))
      "push works" [ 1; 2; 3; 4; 5; 6; 7; 8 ] (Tempo.Pqueue.to_list q);

    let q = Tempo.Pqueue.push q 24 in
    Alcotest.(check (list int))
      "push clobbers oldest element when full"
      [ 2; 3; 4; 5; 6; 7; 8; 24 ]
      (Tempo.Pqueue.to_list q)

  let clear () =
    let q = Tempo.Pqueue.create 8 in
    let q =
      List.fold_left
        (fun q i ->
          let q = Tempo.Pqueue.push q (i * 1) in
          let q = Tempo.Pqueue.push q (i * 2) in
          let q = Tempo.Pqueue.push q (i * 3) in
          let q, _ = Tempo.Pqueue.pop q in
          q)
        q
        (range (Tempo.Pqueue.capacity q))
    in
    Alcotest.(check bool) "not empty" false (Tempo.Pqueue.is_empty q);
    let q = Tempo.Pqueue.clear q in
    Alcotest.(check bool) "now is empty" true (Tempo.Pqueue.is_empty q);
    Alcotest.(check (list int)) "empty contents" [] (Tempo.Pqueue.to_list q)

  let truncate_back () =
    let q = Tempo.Pqueue.create 8 in
    let q =
      List.fold_left Tempo.Pqueue.push q (range (Tempo.Pqueue.capacity q))
    in
    let q = Tempo.Pqueue.truncate_back q 3 in
    Alcotest.(check (list int))
      "truncated down to 3" [ 6; 7; 8 ] (Tempo.Pqueue.to_list q);
    let q = Tempo.Pqueue.truncate_back q 100 in
    Alcotest.(check (list int))
      "truncate up is nop" [ 6; 7; 8 ] (Tempo.Pqueue.to_list q)
end

module Tap_Test = struct
  let display () =
    let t = Tempo.Tap.create 10 true in
    let t = List.fold_left Tempo.Tap.push_bpm t [ 120.051; 112.41; 121.105 ] in
    Alcotest.(check string)
      "to string works" "[121.1, 112.4, 120.1]"
      (Tempo.Tap.string_of_tapper t);

    let t = Tempo.Tap.clear t in
    Alcotest.(check string)
      "empty to string" "[]"
      (Tempo.Tap.string_of_tapper t);

    let t = Tempo.Tap.push_bpm t 112.76 in
    Alcotest.(check string)
      "to string with one element works" "[112.8]"
      (Tempo.Tap.string_of_tapper t)

  let is_recording () =
    let t = Tempo.Tap.create 10 true in
    Alcotest.(check bool)
      "starts not recording" false (Tempo.Tap.is_recording t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check bool) "records first tap" true (Tempo.Tap.is_recording t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check bool) "keeps recording taps" true (Tempo.Tap.is_recording t);
    let t = Tempo.Tap.clear t in
    Alcotest.(check bool) "stops once cleared" false (Tempo.Tap.is_recording t)

  let bpm () =
    let t = Tempo.Tap.create 10 true in
    Alcotest.(check (float eps)) "starts at 0.0 BPM" 0.0 (Tempo.Tap.bpm t);

    let _ =
      List.fold_left
        (fun (t : Tempo.Tap.t) (elem, new_avg) ->
          let t = Tempo.Tap.push_bpm t elem in
          Alcotest.(check (float eps))
            "check new average" new_avg (Tempo.Tap.bpm t);
          t)
        t
        [ (23.0, 23.0); (26.0, 24.5); (29.0, 26.0); (61.0, 34.75) ]
    in
    ()

  let tap () =
    let t = Tempo.Tap.create 3 true in
    Alcotest.(check int) "starts empty" 0 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "1st tap, no span yet" 0 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "2nd tap" 1 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "3rd tap" 2 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "4th tap" 3 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "5th tap, oldest dropped" 3 (Tempo.Tap.count t);

    let t = Tempo.Tap.toggle_bounded t in
    let t = Tempo.Tap.tap t in
    Alcotest.(check int)
      "1st tap after unbounding, can hold more" 4 (Tempo.Tap.count t);
    let t = Tempo.Tap.tap t in
    Alcotest.(check int) "2nd tap after unbounding" 5 (Tempo.Tap.count t);
    let t = Tempo.Tap.toggle_bounded t in
    Alcotest.(check bool) "is now bounded" true (Tempo.Tap.is_bounded t);
    Alcotest.(check int) "truncated after bounding" 3 (Tempo.Tap.count t)

  let truncate () =
    let t = Tempo.Tap.create 3 true in
    let t = List.fold_left Tempo.Tap.push_bpm t [ 80.0; 70.0; 60.0 ] in
    Alcotest.(check (list (float eps)))
      "initialized" [ 60.0; 70.0; 80.0 ] (Tempo.Tap.to_list t);

    let t = Tempo.Tap.push_bpm t 50.0 in
    Alcotest.(check (list (float eps)))
      "push evicts oldest sample" [ 50.0; 60.0; 70.0 ] (Tempo.Tap.to_list t);

    let t = Tempo.Tap.toggle_bounded t in
    let t = Tempo.Tap.push_bpm t 40.0 in
    Alcotest.(check (list (float eps)))
      "nothing evicted from unbounded buffer" [ 40.0; 50.0; 60.0; 70.0 ]
      (Tempo.Tap.to_list t);

    let t = Tempo.Tap.toggle_bounded t in
    Alcotest.(check (list (float eps)))
      "after bounding, oldest evicted" [ 40.0; 50.0; 60.0 ]
      (Tempo.Tap.to_list t)

  let resize () =
    let t = Tempo.Tap.create 3 true in
    let t = List.fold_left Tempo.Tap.push_bpm t [ 80.0; 70.0; 60.0 ] in
    Alcotest.(check (list (float eps)))
      "initialized" [ 60.0; 70.0; 80.0 ] (Tempo.Tap.to_list t);
    let t = Tempo.Tap.resize t 2 in
    Alcotest.(check (list (float eps)))
      "after resize, oldest evicted" [ 60.0; 70.0 ] (Tempo.Tap.to_list t);
    Alcotest.(check int) "capacity is smaller now" 2 (Tempo.Tap.capacity t)
end

let () =
  Alcotest.run "Tempo"
    [
      ( "tapper",
        [
          Alcotest.test_case "display" `Quick Tap_Test.display;
          Alcotest.test_case "is recording" `Quick Tap_Test.is_recording;
          Alcotest.test_case "bpm" `Quick Tap_Test.bpm;
          Alcotest.test_case "tap" `Quick Tap_Test.tap;
          Alcotest.test_case "truncate" `Quick Tap_Test.truncate;
          Alcotest.test_case "resize" `Quick Tap_Test.resize;
        ] );
      ( "queue",
        [
          Alcotest.test_case "push pop" `Quick Pqueue_Test.push_pop;
          Alcotest.test_case "push clobber" `Quick Pqueue_Test.push_clobber;
          Alcotest.test_case "clear" `Quick Pqueue_Test.clear;
          Alcotest.test_case "truncate back" `Quick Pqueue_Test.truncate_back;
        ] );
    ]
