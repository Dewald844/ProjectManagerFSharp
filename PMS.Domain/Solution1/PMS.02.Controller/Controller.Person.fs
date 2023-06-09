namespace Controller

open Domain.Person
open Types.Helpers
open Domain

module Person =

    let persons : Map<Person.PersonId, Version * Person.Person > =  Map.empty

    type CreateError =
        | InvalidDetails of string
        | FailedEvent of Person.FailedEvent

    let createPerson (person : PersonDetails) =
        let newId = Int99.createInt99 ((Map.count persons) + 1) |> PersonId
        let command = Person.Command.Create person
        let interpretEvent = Command.interpretCommand NonExisting command
        match interpretEvent with
        | Ok _ ->
            let newPerson = Event.evolveEvent interpretEvent
            Ok (persons |> Map.add newId (Version 1, newPerson))
        | Error failedEvent ->
            Error (CreateError.FailedEvent failedEvent)

    type UpdateError =
        | InvalidDetails of string
        | FailedEvent of Person.FailedEvent

    let updateTelephoneNumber (personId : Person.PersonId) (telephoneNumber :TelephoneNumber) =
        let command = Person.Command.Update (UpdateMethod.UpdateTelephoneNumber telephoneNumber)
        let personO = persons |> Map.tryFind personId
        match personO with
        | Some (version, person) ->
            let interpretEvent = Command.interpretCommand person.PersonState command
            match interpretEvent with
            | Ok _ ->
                let newPerson = Event.evolveEvent interpretEvent
                let newPersonL = persons |> Map.filter (fun id  _ -> id <> personId)
                Ok (newPersonL |> Map.add personId (version |> Version.nextVersion, newPerson))
            | Error failedEvent ->
                Error (UpdateError.FailedEvent failedEvent)
        | None -> Error (UpdateError.InvalidDetails "Person does not exist")

    let updateAddress (personId : Person.PersonId) (address : PhysicalAddress) =
        let command = Person.Command.Update (UpdateMethod.UpdatePhysicalAddress address)
        let personO = persons |> Map.tryFind personId
        match personO with
        | Some (version, person) ->
            let interpretEvent = Command.interpretCommand person.PersonState command
            match interpretEvent with
            | Ok _ ->
                let newPerson = Event.evolveEvent interpretEvent
                let newPersonL = persons |> Map.filter (fun id  _ -> id <> personId)
                Ok (newPersonL |> Map.add personId (version |> Version.nextVersion, newPerson))
            | Error failedEvent ->
                Error (UpdateError.FailedEvent failedEvent)
        | None -> Error (UpdateError.InvalidDetails "Person does not exist")

    let updateEmailAddress (personId : Person.PersonId) (emailAddress : EmailAddress) =
        let command = Person.Command.Update (UpdateMethod.UpdateEmail emailAddress)
        let personO = persons |> Map.tryFind personId
        match personO with
        | Some (version, person) ->
            let interpretEvent = Command.interpretCommand person.PersonState command
            match interpretEvent with
            | Ok _ ->
                let newPerson = Event.evolveEvent interpretEvent
                let newPersonL = persons |> Map.filter (fun id  _ -> id <> personId)
                Ok (newPersonL |> Map.add personId (version |> Version.nextVersion, newPerson))
            | Error failedEvent ->
                Error (UpdateError.FailedEvent failedEvent)
        | None -> Error (UpdateError.InvalidDetails "Person does not exist")
