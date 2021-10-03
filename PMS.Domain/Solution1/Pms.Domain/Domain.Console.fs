[<EntryPoint>]
let main args =
  match args with
  | [|"-createProject"|] ->
    printf "
  <-------------------------------------------------------->
    | Please give needed information                     |
    |                                                    |
    |        -> Name                                     |
    |        -> Address                                  |
    |        -> ERF Number                               |
    |        -> Project Cost                             |
    |        -> Estimated Deadline                       |
    |                                                    |
  <--------------THANK YOU FOR USING PMS------------------>"

  | _ -> printf "
    Please use one of the following commands
      -> -createProject"

  0