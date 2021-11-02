namespace Types

open System

module Helpers =

  let unreachable() = failwith "Invalid state has been reached"

  type FirstName = FirstName of string

  module FirstName =

    let createFirstName (input : string) : FirstName =
      input |> FirstName.FirstName

  type LastName = LastName of string

  module LastName =

    let createLastName (input : string ) : LastName =
      input |> LastName.LastName

  type Int99 = Number of int

  module Int99 =

    let createInt99Result  (input : int) : Result<Int99,string>   =
      if (input > 0) && (input < 99)
      then Ok    (input |> Int99.Number)
      else Error  $"Invalid input for int 99 REF : {input}"

    let createInt99  (input : int) : Int99   =
      if (input > 0) && (input < 99)
      then input |> Int99.Number
      else failwithf $"Invalid input for int 99 REF : {input}"

  type Int999 = Number of int

  type SystemDate = System of DateTime

  module Int999 =

    let createInt999Result (input : int) : Result<Int999,string> =
      if (input > 0) && (input < 999)
      then Ok    (input |> Int999.Number)
      else Error (failwith $"Invalid input for int999 REF : {input}")

  type String50 = String of string

  module String50 =

    let createString50Result (input : string) : Result<String50, string> =
      if (input.Length > 0) && (input.Length < 50)
      then Ok (input |> String50.String)
      else Error (failwith $"Input can not be more than 50 characters REF : {input}")

  type String10 = String of string

  module String10 =
    let createString10Result ( input : string) : Result<String10,string> =
      if (input.Length > 0) && (input.Length < 10)
      then Ok (input |> String10.String)
      else Error $"Input can not be more than 10 characters REF {input}"

  type PositiveFloat = PositiveFloat of float

  module PositiveFloat =

    let createPositiveFloatResult (input : float) : Result<PositiveFloat, string> =

      if (input > 0.0)
      then Ok (input |> PositiveFloat.PositiveFloat)
      else Error  $"Input amount is less than zero REF : {input} "

    let getPositiveFloat (x : PositiveFloat) : float =
      match x with
      | PositiveFloat output -> output

  type TelephoneNumber = TelephoneNumber of Int32

  module TelephoneNumber =
    let createTelephoneNumber (input : int) : TelephoneNumber =
      if (input.ToString().Length) < 10
      then input |> TelephoneNumber.TelephoneNumber
      else failwith $"Input has more than 10 characters REF {input}"

  type ProjectId = ProjectId of Int99

  module ProjectId =
    let createProjectNumber (input : int) : ProjectId =

        match (input |> Int99.createInt99Result) with
          | Ok result -> result |> ProjectId.ProjectId
          | Error s   -> failwith $"Invalid Project number REF : {input} Message {s}"

  type HouseNumber = HouseNumber of Int999

  module HouseNumber =

    let createAddressNumber (input : int) : HouseNumber =

      match (input |> Int999.createInt999Result) with
      | Ok result -> result |> HouseNumber.HouseNumber
      | Error s   -> failwith $"Invalid house number REF : {input} Message {s}"

  type StreetName = StreetName of String50

  module StreetName =

    let createStreetName (input : string) : StreetName =
      match (input |> String50.createString50Result) with
      | Ok result -> result |> StreetName.StreetName
      | Error s   -> failwith $"Invalid street name REF : {input} Message {s}"

  type City = CityName of String50

  module City =

    let createCity (input : string) : City =
      match (input |> String50.createString50Result) with
      | Ok result -> result |> City.CityName
      | Error s   -> failwith $"Invalid city name REF : {input} Message {s}"

  type PhysicalAddress = HouseNumber * StreetName * City

  type CashAmount = CashAmount of PositiveFloat

  module CashAmount =

    let createCashAmount (input : float) : CashAmount =
      match (input |> PositiveFloat.createPositiveFloatResult) with
      | Ok result -> result |> CashAmount.CashAmount
      | Error s -> failwith $"Invalid Cash amount REF : {input} Message {s} "

    let getCashAmount (x : CashAmount) : float =
      match x with
      | CashAmount xFloat -> xFloat |> PositiveFloat.getPositiveFloat

    let addCashAmount (a : CashAmount) (b : CashAmount) : CashAmount =
      let addition =
        ( (a |> getCashAmount) + (b |> getCashAmount) )

      addition
      |> PositiveFloat
      |> CashAmount.CashAmount

  type ERFNumber = ERFNumber of String10

  module ERFNumber =

    let createERFNumber (input : string) : ERFNumber =
      match (input |> String10.createString10Result) with
      | Ok result -> result |> ERFNumber.ERFNumber
      | Error s -> failwith $"Invalid ERF number REF : {input} Message : {s}"

  type Version = Version of int

  module Version =
    let toInt version =
      match version with
      | Version int -> int

    let initialVersion = Version 1

    let nextVersion version =
      let currentVersion =
        version |> toInt
      (currentVersion + 1)  |> Version

    let previousVersion version =
      let currentVersion =
        version |> toInt
      (currentVersion - 1) |> Version