let read_file (file:string) : string =
  let lines = ref [] in
  let channel = open_in file in
  try while true; do
      lines := input_line channel :: !lines
  done; ""
  with End_of_file ->
    close_in channel;
    String.concat "\n" (List.rev !lines)

let main() = 
  let program = read_file "test.tig" |> 
    Lexing.from_string |>
    Parser.prog Lexer.token
  in
  program