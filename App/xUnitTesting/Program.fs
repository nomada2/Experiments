namespace Testing

// xunit
// xunit.runner.visualstudio 
// fsunit
open System

module Messaging =
    type Envelope<'a> = {
        Id : Guid
        Created : DateTimeOffset
        Item : 'a }

    let envelop getId getTime item = {
        Id = getId ()
        Created = getTime ()
        Item = item }

module MessagingTests =
    open Xunit
    open Xunit.Extensions

    type Foo = { Text : string; Number : int }

    [<Fact>]
    let ``my test``() =
        Assert.Equal(1,11)

    [<Theory>]
    [<InlineData("7B0EC0F8-B213-46D5-87CA-F095229CDACB", 635575303021425464L, 1., "Bar", 42)>]
    [<InlineData("39E940D6-B820-42BC-8F4D-AE587593DB95", 635575279586114123L, 0., "Baz", 1337)>]
    let ``envelop returns correct result`` (id : string) (ticks : int64) (offset : float) (text : string) (number : int) =
        let getId _ = Guid id
        let getTime _ = DateTimeOffset(ticks, TimeSpan.FromHours offset)
        let item = { Text = text; Number = number }

        let actual = Messaging.envelop getId getTime item

        Assert.Equal(Guid id, actual.Id)
        Assert.Equal(
            DateTimeOffset(ticks, TimeSpan.FromHours offset),
            actual.Created)
        Assert.Equal(item, actual.Item)


module FsUitTesting =
    
    open Xunit
    open Xunit.Extensions
    open FsUnit

    [<Fact>]
    let ``test with FSUnit`` () =
        1 |> should equal 1